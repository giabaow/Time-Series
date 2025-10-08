getwd()
setwd("")

#Traditional vs Time series data
library(readxl)
Titanic <- read_excel("Titanic_PS4.xlsx", sheet = "Titanic")
PS4 <- read_excel("Titanic_PS4.xlsx", sheet = "PS4")
str(Titanic)
str(PS4)

PS4$Date <- as.Date(PS4$Date)

library(ggplot2)
ggplot(Titanic, aes(x=TitanicUtasSorszama, y =Viteldij)) +
  geom_line()

ggplot(PS4, aes(x=Date, y=Google_PS4)) +
  geom_line()

PS4$t <- 1:20
str(PS4)

trend_model <- lm(Google_PS4 ~ t, data = PS4)
summary(trend_model)
#Adj R squared is quite high but dont rely on it now, we have to check the residuals

PS4$Quarter <- quarters(PS4$Date)
str(PS4)
PS4$Quarter <- as.factor(PS4$Quarter)
PS4$Quarter <- relevel(PS4$Quarter, ref = 'Q4')

ST_model <- lm(Google_PS4 ~ t + Quarter, data = PS4)
summary(ST_model)
#coef t = 15.894: it is growing trend, if we add one more additional time period, the trend grow by 15.894
#coef Q: compare to Q4, on avg, the 1st quarter is lower by 156.919 than the Q4

PS4$Y_hat <- ST_model$fitted.values
PS4$residuals <- ST_model$residuals

str(PS4)

ggplot(PS4, aes(x=Date)) +
  geom_line(aes(y = Google_PS4, color = "Real Y"), size = 1) + 
  geom_line(aes(y = Y_hat, color = "Y hat"), size = 1)


ggplot(PS4, aes(x=Date)) +
  geom_line(aes(y = residuals), size = 1)

#Autocorrelation of a time series
PS4$residuals_log1 <- c(NA, PS4$residuals[1:(length(PS4$residuals)-1)])

PS4$residuals_log2 <- c(NA, NA, PS4$residuals[1:(length(PS4$residuals)-2)])

cor(PS4[3:nrow(PS4), 
        c("residuals", "residuals_log1", "residuals_log2")])

#GazAR
GazAr <- read_excel("GazAr.xlsx")
str(GazAr)

ggplot(GazAr, aes(x = Ev, y = GazAr)) +
  geom_line() #only trend

GazAr$t <- 1:nrow(GazAr)

TrendGazAr <- lm(GazAr ~ t, data = GazAr)
summary(TrendGazAr)

GazAr$Trend <- TrendGazAr$fitted.values
GazAr$residuals <- TrendGazAr$residuals

ggplot(GazAr, aes(x=Ev)) +
  geom_line(aes(y = GazAr, color = "GazAr"), size = 1) +
  geom_line(aes(y = Trend, color = "Trend model pred"), size = 1)

ggplot(GazAr, aes(x = Ev, y = residuals)) + geom_line()

ExpTrendGazAr <- lm(log(GazAr)~ t, data = GazAr)
summary(ExpTrendGazAr)

GazAr$ExpTrend <- exp(ExpTrendGazAr$fitted.values)
GazAr$resid_exp <- exp(ExpTrendGazAr$residuals)

ggplot(GazAr, aes(x= Ev)) +
  geom_line(aes(y=GazAr, color = "Original"), size = 1) +
  geom_line(aes(y=Trend, color = "Lin pred"), size = 1) +
  geom_line(aes(y=ExpTrend, color = "Exp pred"), size = 1)

s_e_Lin <- sqrt(sum((GazAr$GazAr - GazAr$Trend)^2)/nrow(GazAr))
s_e_Exp <- sqrt(sum((GazAr$GazAr - GazAr$ExpTrend)^2)/nrow(GazAr))


#HDTV
HDTV_Google <- read_excel("HDTV_Google.xlsx")
str(HDTV_Google)
HDTV_Google$Date <- as.Date(HDTV_Google$Date)
ggplot(HDTV_Google, aes(x = Date, y= HDTV_Google)) + geom_line()
#the deviation get lower and lower in this plot

HDTV_Google$t <- 1:nrow(HDTV_Google)
HDTV_Google$Month <- months(HDTV_Google$Date)

HDTV_Google$Month <- as.factor(HDTV_Google$Month)
HDTV_Google$Month <- relevel(HDTV_Google$Month, ref = "December")

HDTV_Model <- lm(HDTV_Google ~ t + Month, data = HDTV_Google)
summary(HDTV_Model)

HDTV_ExpModel <- lm(log(HDTV_Google) ~ t + Month, data = HDTV_Google)
summary(HDTV_ExpModel) #Adjusted R-squared:  0.9815 almost perfect fit

HDTV_Google$lin_pred <- HDTV_Model$fitted.values
HDTV_Google$Exp_pred <- exp(HDTV_ExpModel$fitted.values)
HDTV_Google$lin_resid <- HDTV_Model$residuals
HDTV_Google$exp_resid <- exp(HDTV_ExpModel$residuals)

ggplot(HDTV_Google, aes(x = Date)) +
  geom_line(aes(y=HDTV_Google, color = "Orignal"), size = 1) +
  geom_line(aes(y=lin_pred, color = "Lin pred"), size = 1) +
  geom_line(aes(y=Exp_pred, color = "Exp pred"), size = 1)

ggplot(HDTV_Google, aes(x = Date)) +
    geom_line(aes(y = lin_resid, color = "Linear residuals"), size = 1) +
  geom_line(aes(y = exp_resid, color = "Exp residuals"), size = 1)


