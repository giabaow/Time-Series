setwd("S:/Sem 5/Time Series/02")
library(readxl)
#---------------------------------------Moving Averages-------------------------------------------
SmartTV <- read_excel("SmartTV_Google.xlsx")

str(SmartTV)

library(ggplot2)

ggplot(SmartTV, aes(x=Date, y = SmartTV_Google))+
  geom_line()
#seems like linear seasonality -> tougher to handle
#--> moving averages

#Manual calculation
mean(SmartTV$SmartTV_Google[1:7])
mean(SmartTV$SmartTV_Google[2:8])
mean(SmartTV$SmartTV_Google[3:9]) #the steps continue to 4:10, 5:11.........

#previous step pretty tedious ngl, so lets use package to do it for us
install.packages("forecast")
library(forecast)

M7 <- ma(SmartTV$SmartTV_Google, order=7) #odd number we lose n-1
M7
M12 <- ma(SmartTV$SmartTV_Google, order=12)
M12
#here, higher order MA is better, because no seasonality was left -> best estimation for trend effect

SmartTV$M7 <- M7
SmartTV$M12 <- M12

# Lets see which one fits better on a plot(visually)
ggplot(SmartTV, aes(x=Date))+
  geom_line(aes(y=SmartTV_Google, color="Original"), size=1)+
  geom_line(aes(y= M7, color= "MA(7)"), size = 1)+
  geom_line(aes(y= M12, color= "MA(12)"), size = 1)
# 12 unit moving average smooths it out too much -> we lose all seasonality
# However, for filtering, it is better
# y(t) = trend * season * noise --> divide by moving average (trend)
# --> Filtered y(t) = season * noise
#we can see that MA 12 is more linear

#Filter the Moving Average out
SmartTV$TrendFiltered <- SmartTV$SmartTV_Google / SmartTV$M12

#Handling Seasonality
SmartTV$Month <- months(SmartTV$Date)

#Get raw seasonal indexes
SeasonEffect <- aggregate(TrendFiltered ~ Month, data = SmartTV,
                          FUN = mean) #Aggregate the TrendFiltered values, grouped by Month, and apply the mean function to each group.

#this shows how many times the predicted value the actual value is
mean(SeasonEffect$TrendFiltered)
#almost 1, but not quite - we want it to be 1
SeasonEffect$CorrectedIndex <- SeasonEffect$TrendFiltered / mean(SeasonEffect$TrendFiltered)
SeasonEffect
mean(SeasonEffect$CorrectedIndex) #so no noise now
#1 on the dot - we corrected the downwards noise we had

#Add the corrected index to the original dataset
SmartTV <- merge(SmartTV, SeasonEffect[, c("Month", "CorrectedIndex")],
                 by = "Month", all.x = TRUE) #all.x essentially makes this a left join

head(SmartTV)

#Data is ordered alphabetically by month (so April data are first) -> lets reorder
SmartTV <- SmartTV[order(SmartTV$Date),]

head(SmartTV)

SmartTV$SeadAdj <- SmartTV$SmartTV_Google / SmartTV$CorrectedIndex

#check seasonality with ggplot
ggplot(SmartTV, aes(x = Date, y=SeadAdj))+
  geom_line(size=1) + theme_minimal()

SmartTV$t <- 1:nrow(SmartTV)

LinTrend <- lm(SeadAdj ~ t, data = SmartTV)
summary(LinTrend)
SmartTV$Trend <- LinTrend$fitted.values

SmartTV$ModelledTS <- SmartTV$Trend * SmartTV$CorrectedIndex

# Check this one's viability with ggplot (this should resemble the time-series pretty well)
ggplot(SmartTV, aes(x=Date))+
  geom_line(aes(y=SmartTV_Google, color="Original"), size=1)+
  geom_line(aes(y=ModelledTS, color="ModelledTS"), size=1)
#the plot looks very good
# Solution would be similar with an additive model, just replace the / in adjustments with -
#and normalization would be normalizing to 0 instead of 1

#------------------------Package Calucaltion-----------------------------------------------

#use decompose function that would give us seasonal and trend function in one step
#Seasonality adjustment with decompose

SmartTV_ts <- ts(SmartTV$SmartTV_Google, start = c(2011,1), frequency=12)

plot(SmartTV_ts)

DecomposeMult <- decompose(SmartTV_ts, type = "multiplicative")
str(DecomposeMult)

plot(DecomposeMult)

as.numeric(SmartTV$M12)

DecomposeMult$trend

as.numeric(SmartTV$CorrectedIndex)
DecomposeMult$seasonal

SmartTV <- SmartTV[, c("Date", "SmartTV_Google")]
SmartTV$SeadAdj <- SmartTV$SmartTV_Google / as.numeric(DecomposeMult$seasonal)

SmartTV$t <- 1:nrow(SmartTV)
LinTrend <- lm(SeadAdj ~ t, data=SmartTV)
SmartTV$Modeled <- LinTrend$fitted.values * as.numeric(DecomposeMult$seasonal)

#Prediction#########################################################
install.packages("lubridate")
library(lubridate)

ForecastDates <- SmartTV$Date[nrow(SmartTV)] %m+% months(c(1:12))

Forecast <- data.frame(Date = ForecastDates, 
                       SmartTV_Google = rep(NA, 12),
                       SeasAdj = rep(NA, 12),
                       t = nrow(SmartTV) + 1:12,
                       Modeled = rep(NA, 12)) #quite empty

#now forcasting with predict function
str(Forecast)

Forecast$Modeled <- predict(LinTrend, newdata = Forecast)
str(Forecast)

Forecast$Modeled <- Forecast$Modeled * as.numeric(DecomposeMult$seasonal[1:12]) #future * seasonal
str(Forecast)

PredictedSmartTV <- rbind(SmartTV, Forecast)

ggplot(PredictedSmartTV, aes(x=Date))+
  geom_line(aes(y= SmartTV_Google, color='Original time series'), size=1)+
  geom_line(aes(y= Modeled, color='Predictions for 2015'), size=1)


#----------------------------------------------------------------------------------#
FB_Mobility <- read_excel("FacebookMobilityData.xlsx")

FB_Mobility$Date <- as.Date(FB_Mobility$Date)

ggplot(data=FB_Mobility, aes(x=Date, y=FacebookMobility)) +
  geom_line(size=1) + geom_hline(yintercept = 0, color='red')

FB_Filtered <- FB_Mobility[FB_Mobility$Date >= "2020-03-20" & FB_Mobility$Date <= "2020-12-31",]
FB_Filtered$t <- 1:nrow(FB_Filtered)
Model_TrendOnly <- lm(FacebookMobility ~ t + I(t^2), data = FB_Filtered)
summary(Model_TrendOnly)

FB_Filtered$Y_hat <- Model_TrendOnly$fitted.values

ggplot(FB_Filtered, aes(x=Date)) + 
  geom_line(aes(y= FacebookMobility, color='Real Y'), size=1)+
  geom_line(aes(y= Y_hat, color='Estimated Y'), size=1)+
  geom_hline(yintercept = 0, color="darkblue", size=1)

FB_Filtered$HolidayFlag <- 0

FB_Filtered$HolidayFlag[FB_Filtered$Date == "2020-03-15"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date == "2020-04-10"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date == "2020-04-12"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date == "2020-04-13"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date == "2020-05-01"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date == "2020-05-31"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date == "2020-06-01"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date == "2020-08-20"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date == "2020-10-23"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date == "2020-11-01"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date >= "2020-12-24" 
                        & FB_Filtered$Date <= "2021-01-01"] <- 1

Model_Holiday <- lm(FacebookMobility ~ t + I(t^2) + DayofWeek + HolidayFlag, data = FB_Filtered)

summary(Model_Holiday)

FB_Filtered$Y_hat <- Model_Holiday$fitted.values
ggplot(FB_Filtered, aes(x=Date)) +
  geom_line(aes(y=FacebookMobility, color = "Real Y"), size=1)+
  geom_line(aes(y=Y_hat, color = "Estimated Y"), size = 1)

contrasts(FB_Filtered$DayofWeek)

Contrast_matrix <- contr.sum(7)
rownames(Contrast_matrix) <- rownames(contrasts(FB_Filtered$DayofWeek))

colnames(Contrast_matrix) <- c("Friday", "Monday", "Saturday", "Sunday", "Thursday", "Tuesday")

contrasts(FB_Filtered$DayofWeek) <- Contrast_matrix
Model_Season_Contrast <- lm(FacebookMobility ~ t + I(t^2) + DayofWeek, 
                            data = FB_Filtered)

summary(Model_Season_Contrast)

Coefficient_Data <- cbind(
  data.frame(Name_Dummy = names(Model_Season$coefficients),
             Coefficients_Dummy = Model_Season$coefficients),
  data.frame(Name_Contrast = names(Model_Season_Contrast$coefficients),
             Coefficients_Contrast = Model_Season$coefficients)
)

Coefficient_Data

#Modelling Structural Breaks
FB_Mobility$t <- 1:nrow(FB_Mobility)
str(FB_Mobility)
FB_Mobility$DayofWeek <- as.factor(weekdays(FB_Mobility$Date))
Model_Cubic í- lm(FacebookMobility, color = "Real Y")