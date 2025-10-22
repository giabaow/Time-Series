library(readxl)
SmartTV <- read_excel("SmartTV_Google.xlsx")
SmartTV$Date <- as.Date(SmartTV$Date)
str(SmartTV)

library(ggplot2)
ggplot(SmartTV, aes(x = Date, y = SmartTV_Google)) +
  geom_line(size = 1)
#trend effect is linear
#the higher the time series takes the trend, the larger the magnitude of the seasonal swings
#-> seasonality is multiplicatively related to the trend.

#USING MOVING AVERAGE TO REDUCE THE SEASONALITY
install.packages("forecast")
library(forecast)
ma(SmartTV$SmartTV_Google, order = 7)        

#convert it to numberical data and add it to our dataframe
SmartTV$MA7 <- as.numeric(ma(SmartTV$SmartTV_Google, order = 7))
SmartTV$MA12 <- as.numeric(ma(SmartTV$SmartTV_Google, order = 12))

#plot it
ggplot(SmartTV, aes(x=Date)) +
  geom_line(aes(y=SmartTV_Google,color="Original Y_t"), size = 1) +
  geom_line(aes(y=MA7,color="MA7"), size = 1) + 
  geom_line(aes(y=MA12,color="MA12"), size = 1)


#Seasonal ratio
SmartTV$TrendFiltered <- SmartTV$SmartTV_Google / SmartTV$MA12
SmartTV$TrendFiltered
#TrendFiltered is SEASON x NOISE component

#get seasonal indices
SmartTV$Month <- months(SmartTV$Date)
SeasonEffect <- aggregate(TrendFiltered ~ Month, data = SmartTV, FUN = mean)
#normalize them
mean(SeasonEffect$TrendFiltered)
SeasonEffect$CorrSeasonIndex <- SeasonEffect$TrendFiltered/(mean(SeasonEffect$TrendFiltered))
SeasonEffect
mean(SeasonEffect$CorrSeasonIndex)
#season index of December: 1,465 which means December search volume is on average 46,5% higher 
#than the general trend would suggest
#while the June is on average 7% lower than the trend expectation

#Merge with the orginal dataframe
SmartTV <- merge(SmartTV, SeasonEffect[, c("Month", "CorrSeasonIndex")], by = "Month", all.x = TRUE)

#Only now the data frame is sorted by month names. We correct this, sort things back by Date
SmartTV <- SmartTV[order(SmartTV$Date),]

#SEASONAL ADJUST AND FIT TREND
#Remove the seasonal indice to get the TREND x NOISE component
SmartTV$SeaAdjust <- SmartTV$SmartTV_Google/SmartTV$CorrSeasonIndex

ggplot(SmartTV, aes(x=Date)) +
  geom_line(aes(y = SmartTV_Google, color = "Actual")) +
  geom_line(aes(y=SeaAdjust, color = "SeaAdjust"))
#trend with some noise

#Fit the linear trend
SmartTV$t <- 1:nrow(SmartTV)
LinTrend = lm(SeaAdjust ~ t, data = SmartTV)
SmartTV$Trend <- LinTrend$fitted.values #Y = B0 + B1t
str(SmartTV)

#Add back the seasonal indices
SmartTV$ModelledTS <- SmartTV$Trend * SmartTV$CorrSeasonIndex

ggplot(SmartTV, aes(x=Date)) + 
  geom_line(aes(y=SmartTV_Google, color = "Original time series"), size=1) +
  geom_line(aes(y=ModelledTS, color = "Modelled time series"), size=1)





#------------------------------------------------------------------
#Seasonal Adjustment with the decompose Function
#Decompose Function helps us to generate corrected seasonal indices using MA
SmartTV_ts <- ts(SmartTV$SmartTV_Google, start = c(2011,1), frequency = 12)
SmartTV_ts
plot(SmartTV_ts)

#Determine if it should be additive or multiplicative model
DecompMult <- decompose(SmartTV_ts, type = 'multiplicative')
str(DecompMult) #it print the seasonal indices

plot(DecompMult)

as.numeric(SmartTV$MA12)
#same as
DecompMult$trend

SmartTV <- SmartTV[,c("Date", "SmartTV_Google")]
SmartTV$SeaAdjust <- SmartTV$SmartTV_Google/as.numeric(DecompMult$seasonal)

#again
SmartTV$t <- 1:nrow(SmartTV)
LinTrend <- lm(SeaAdjust ~ t, data = SmartTV)
SmartTV$Modelled <- LinTrend$fitted.values * as.numeric(DecompMult$seasonal)
library(ggplot2)
ggplot(SmartTV, aes(x=Date)) +
  geom_line(aes(y=SmartTV_Google, color = "Original time series"), size = 1) +
  geom_line(aes(y=Modelled, color = "Modelled time series"), size = 1)




#------------------------------------------------------------------
#Prediction from a Decomposition Model
install.packages("lubridate")
library(lubridate)

ForecastDates <- SmartTV$Date[nrow(SmartTV)] %m+% months(c(1:12))
ForecastDates 

#create data frame
Forecast <- data.frame(Date = ForecastDates,
                       SmartTV_Google = rep(NA,12),
                       SeaAdjust = rep(NA,12),
                       t = nrow(SmartTV) + 1:12,
                       Modelled = rep(NA,12))

str(Forecast)
Forecast$Modelled <- predict(LinTrend, newdata = Forecast)
Forecast$Modelled <- Forecast$Modelled * as.numeric(DecompMult$seasonal[1:12])
str(Forecast)
PredictedSmartTV <- rbind(SmartTV, Forecast)

ggplot(PredictedSmartTV, aes(x=Date)) +
  geom_line(aes(y=SmartTV_Google, color = "Original time series"), size = 1) +
  geom_line(aes(y=Modelled, color = "Modelled time series"), size = 1)






#------------------------------------------------------------------
#Facebook Mobility Data from Hungary
library(readxl)
FB_Mobility <- read_excel("FacebookMobilityData.xlsx")

FB_Mobility$Date <- as.Date(FB_Mobility$Date)
str(FB_Mobility)

library(ggplot2)
ggplot(FB_Mobility, aes(x=Date, y=FacebookMobility)) + 
  geom_line(size=1) +
  geom_hline(yintercept=0, color = "red", size=1)

FB_Filtered <- FB_Mobility[FB_Mobility$Date >= "2020-03-20" &
                             FB_Mobility$Date <= "2020-12-31",]

ggplot(FB_Filtered, aes(x=Date, y=FacebookMobility)) + 
  geom_line(size=1) +
  geom_hline(yintercept=0, color = "red", size=1)


FB_Filtered$t <- 1:nrow(FB_Filtered)
#quadratic trend
Model_TrendOnly <- lm(FacebookMobility ~ t + I(t^2), data = FB_Filtered)
summary(Model_TrendOnly)

FB_Filtered$Y_hat <- Model_TrendOnly$fitted.values
ggplot(FB_Filtered, aes(x=Date)) +
  geom_line(aes(y=FacebookMobility, color="Real Y"), size=1) +
  geom_line(aes(y=Y_hat, color="Estimated Y"), size=1) +
  geom_hline(yintercept=0, color = "darkblue", size=1)
#we have the perfect quadratic line for trend, now we need to add seasonality
#seasonality can be extracted from date using the function "weekdays"


FB_Filtered$DayofWeek <- weekdays(FB_Filtered$Date)
str(FB_Filtered)

#add Dummy table for Season (Day of week)
FB_Filtered$DayofWeek <- as.factor(FB_Filtered$DayofWeek) 
Model_Season <- lm(FacebookMobility ~ t + I(t^2) + DayofWeek, data = FB_Filtered)
summary(Model_Season)

FB_Filtered$Y_hat <- Model_Season$fitted.values

ggplot(FB_Filtered, aes(x=Date)) +
  geom_line(aes(y=FacebookMobility, color="Real Y"), size=1) +
  geom_line(aes(y=Y_hat, color="Estimated Y"), size=1) +
  geom_hline(yintercept=0, color = "darkblue", size=1)


#Handle the effect of Holidays
FB_Filtered$HolidayFlag <- 0 # Every day initially 0

# Give 1 for the holidays
FB_Filtered$HolidayFlag[FB_Filtered$Date=="2020-03-15"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date=="2020-04-10"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date=="2020-04-12"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date=="2020-04-13"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date=="2020-05-01"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date=="2020-05-31"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date=="2020-06-01"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date=="2020-08-20"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date=="2020-10-23"] <- 1
FB_Filtered$HolidayFlag[FB_Filtered$Date=="2020-11-01"] <- 1
FB_Filtered$HolidayFlag[(FB_Filtered$Date>="2020-12-24") &
                          FB_Filtered$Date<="2021-01-01"] <- 1 # Christmas period
str(FB_Filtered)

Model_Holiday <- lm(FacebookMobility ~ t + I(t^2) + DayofWeek + HolidayFlag, data = FB_Filtered)
FB_Filtered$Y_hat <- Model_Holiday$fitted.values

ggplot(FB_Filtered, aes(x=Date)) +
  geom_line(aes(y=FacebookMobility, color="Real Y"), size=1) +
  geom_line(aes(y=Y_hat, color="Estimated Y"), size=1) +
  geom_hline(yintercept=0, color = "darkblue", size=1)








#------------------------------------------------------------------
#Analysis of Seasonality with Contrast Variables  
contrasts(FB_Filtered$DayofWeek)

ContrastMatrix <- contr.sum(7)
rownames(ContrastMatrix) <- rownames(contrasts(FB_Filtered$DayofWeek))
colnames(ContrastMatrix) <- c("Friday", "Monday", "Saturday", "Sunday", "Thursday", "Tuesday")

ContrastMatrix

contrasts(FB_Filtered$DayofWeek) <- ContrastMatrix
Model_Season_Contrast <- lm(FacebookMobility ~ t + I(t^2) + DayofWeek, data = FB_Filtered)
summary(Model_Season_Contrast)


CoefficientWindow <- cbind(
  data.frame(Name_Dummy=names(Model_Season$coefficients),
             Coefficients_Dummy= Model_Season$coefficients),
  data.frame(Name_Contrast=names(Model_Season_Contrast$coefficients),
             Coefficients_Contrast= Model_Season_Contrast$coefficients)
)

rownames(CoefficientWindow) <- NULL # optical tuning, otherwise repeats the variations of the Dummy model
CoefficientWindow

#According to the contrast model, mobility is on average 0.039 percentage points 
#lower on Wednesdays compared to the time series average.

Wed_contrast <- sum(-CoefficientWindow$Coefficients_Contrast[4:9])
Wed_contrast
# on average, people are 1.9 percentage points (0.019) more mobile on a Wed 
#than on the average day of the survey period

#If we want, we can calculate the average Facebook Mobility Index for a Wednesday
Wed_average <- CoefficientWindow$Coefficients_Contrast[1] + Wed_contrast
Wed_average
# -0.3805093 on average, people move about 38% less on Sunday compared to February 2020


#compared to referece day (Wed) or the average day of the survey period????
#why compare to Feb 2020????
#to be continued...








#------------------------------------------------------------------
#Modelling of Structural Breaks
ggplot(FB_Filtered, aes(x=Date)) +
  geom_line(aes(y=FacebookMobility, color="Real Y"), size=1) +
  geom_line(aes(y=Y_hat, color="Estimated Y"), size=1) +
  geom_hline(yintercept=0, color = "darkblue", size=1)

FB_Filtered <- FB_Mobility[FB_Mobility$Date >= "2020-03-20",]

ggplot(FB_Filtered, aes(x=Date, y=FacebookMobility)) + geom_line(size=1) +
  geom_hline(yintercept=0, color = "red", size=1)

FB_Filtered$t <- 1:nrow(FB_Filtered)
FB_Filtered$DayofWeek <- weekdays(FB_Filtered$Date)
FB_Filtered$DayofWeek <- as.factor(FB_Filtered$DayofWeek)

Model_Cubic <- lm(FacebookMobility ~ t + I(t^2) + I(t^3) + DayofWeek, data = FB_Filtered)
summary(Model_Cubic)

FB_Filtered$Y_hat <- Model_Cubic$fitted.values

ggplot(FB_Filtered, aes(x=Date)) +
  geom_line(aes(y=FacebookMobility, color="Real Y"), size=1) +
  geom_line(aes(y=Y_hat, color="Estimated Y"), size=1) +
  geom_hline(yintercept=0, color = "darkblue", size=1)

FB_Filtered$Section <- "Section 1"
FB_Filtered$Section[FB_Filtered$Date > "2020-07-25" &
                      FB_Filtered$Date <= "2021-01-02"] <- "Section 2"
FB_Filtered$Section[FB_Filtered$Date > "2021-01-02"] <- "Section 3"
FB_Filtered$Section <- as.factor(FB_Filtered$Section)
str(FB_Filtered)

Model_Struct <- lm(FacebookMobility ~ t + Section + t*Section + DayofWeek, data = FB_Filtered)
summary(Model_Struct)
FB_Filtered$Y_hat <- Model_Struct$fitted.values

ggplot(FB_Filtered, aes(x=Date)) +
  geom_line(aes(y=FacebookMobility, color="Real Y"), size=1) +
  geom_line(aes(y=Y_hat, color="Estimated Y"), size=1) +
  geom_hline(yintercept=0, color = "darkblue", size=1)

#The strucchange Package to find the appropriate break point
install.packages("strucchange")
library(strucchange)
#we can find the appropriate break point by the breakpoint function
BreakPoints <- breakpoints(FB_Filtered$FacebookMobility ~ 1)
#1 is assume that the data is flat

# How many breakpoints are there?
length(BreakPoints$breakpoints)
FB_Filtered$Date[BreakPoints$breakpoints[1]]
FB_Filtered$Date[BreakPoints$breakpoints[2]]
FB_Filtered$Date[BreakPoints$breakpoints[3]]
FB_Filtered$Date[BreakPoints$breakpoints[4]]
FB_Filtered$Date[BreakPoints$breakpoints[5]]
plot(BreakPoints)







