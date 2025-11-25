# Box-Jenkins modelling example file

rm(list = ls())    # delete all variables from memory
cat("\014")    # clean result window (Ctrl + L)

# load file
BJ <- read.csv("BJ_6.csv")
View(BJ)

BJ.ts = ts(BJ, frequency = 4, start = 1)
plot.ts(BJ$Y)
    # most probably not stationary
    # hedgehogs ---> seasonality?
# lets check
acf(BJ$Y)
  # already here I am almost sure it is not stationary
  # likely to be seasonal as well (4th, 8th, 12th etc are higher)

# BJ modelling
# Step 1. Reach stationarity (transform until stationary)
# Step 2. On stationary series: do ARMA model selection
# Step 3. Do model diagnostics (test if residuals are white noise)
       # check stationarity of final model

# Steo 1. check stationarity with ADF / KPSS
adf.test(BJ$Y)    # p = around 0.2
summary(urca::ur.df(BJ$Y, type = "drift", lags = 8, selectlags = "BIC"))
               # I cannot reject H0 even at 10% level (p > 10%)
         # I cannot reject H0: Y is non-stationary
summary(urca::ur.df(BJ$Y, type = "trend", lags = 8, selectlags = "BIC"))
       # negative trend is significant, so maybe this is correct specification
       # and ADF test is significant at 10%, but not at 5%
       # ---> so there is some weak evidence against non-stationarity (for stationarity)

kpss.test(BJ$Y)
      # p < 0.01, I can reject H0 (stationarity) at 1%
summary(urca::ur.kpss(BJ$Y))
      # p is essentially 0, very strong evidence against H0 (stationarity)

     # conclusion: BJ$Y is not stationary!

# take the first difference!

BJ$dY <- diff(BJ$Y)   # does not work as we lose one observation, does not fit into existing data frame with fixed length
BJ$dY <- c(NA,diff(BJ$Y))   # does not work as we lose one observation, does not fit into existing data frame with fixed length
plot.ts(BJ$dY[2:400])   # no trend apparently
acf(BJ$dY[2:400])
     # clearly seasonality!

# is it stationary?
  
adf.test(BJ$dY[2:400])
     # p < 0.01, reject H0 of non-stationary --> stationary!
kpss.test(BJ$Y[2:400])
     # I cannot reject H0 of stationary --> stationary!

summary(urca::ur.df(BJ$dY[2:400], type = "drift", lags = 8, selectlags = "BIC"))
summary(urca::ur.kpss(BJ$dY[2:400])) 

#as dY is already stationary, I can move to step 2: ARMA MODEL
#But i will also have to handle seasonality
#To remove seasonality
#On average the value of Q1 is lower than the Q4 by 8.72 unit,
#I want to add back 8.72

model <- lm(dY ~ dq1 + dq2 + dq3, data = BJ[2:400,])
   #significant seasonal effects
   #residuals will be seasonally adjusted
   #save residuals

dY_SA <- resid(model)
plot.ts(dY_SA)
#seemingly, there is no remaining seasonality
#and it is also stationary
#start ARMA modelling

#maybe it is white noise?
library(lmtest)
bgtest(dY_SA ~ 1, order = 8)
#I reject H0 of no autocorrelation up to 8 lags
#-> there is remaining autocorrelation which i want to find
#lets do ARMA modelling

#what is the correlation structure? --> check ACF/PACF
acf(dY_SA)
#the 1sr order autocorrelation is very significant
#probably MA(1)?

pacf(dY_SA)
#first two orders are significant
#probably AR(2)?

#AR(2)
ar2 <- arima(dY_SA, order = c(2,0,0))
ar2
# AR(2) and AR(1) parameters both significant
#seem to be a good model, but it is good enough?
#is it true that there is no remaining autocorrelation?
#a good model should contain no remaining autocorrelation
bgtest(resid(ar2) ~ 1, order = 8)
#no autocorrellation, so we keep this model

#would it be enough to have AR(1)
ar1 <- arima(dY_SA, order = c(1,0,0))
ar1
#seem to be a good model, but it is good enough?
#is it true that there is no remaining autocorrelation?
bgtest(resid(ar1) ~ 1, order = 8)
#the residuals is contain in the p2Xt-2 + ut
#there is significant autocorrelation, drop this model



#MA(1)
ma1 <- arima(dY_SA, order = c(0,0,1))
ma1
#good model but no remaining autocorrelation?
bgtest(resid(ma1) ~ 1, order= 8)
#no remaining autocorr, this is another candidate model

#in general we could try arma(2,1)
arma21 <- arima(dY_SA, order = c(2,0,1))
arma21
#ma1: -0.06 which is not really significant (-0,2 < > 0.2 is significant),
#no need to include MA(1)
#AR(2) already enough
bgtest(resid(arma21)~1, order = 8)
#as good as AR(2), but that is not suprising
#as we included an unnecessary varibale into an already good model)

#lets then choose the best model based on some info criterias
#candidates: AR(2), MA(1)
#(for illustration, we will also include AR(1) and ARMA(2,1)) into the comparision
BIC(ar2, ar1, ma1, arma21)
AIC(ar2, ar1, ma1, arma21)

#---> choose AR2

#Lets summariye what we found
summary(model)
#dY(t) = 3.23 - 8.72*dQ1(t) - 0.90*dQ2(t) - 3.33*dQ3(t) + v(t)
#but here the residual is AR(2)
ar2
#v(t) = -0.001 + 0.5264*v(t-1) - 0.2137*v(t-2) + eps(t)
#where eps(t) is already white noise

#the whole model in one tep: ARIMAX model, ARIMA with X
#ARIMA part of Y: (2,1,0)
# x part: dQ1, dQ2, dQ3

library(forecast)
#full model of original variable Y:
fullmodel <- Arima(BJ$Y, order = c(2,1,0), xreg = as.matrix(BJ[,4:6]), include.constant = TRUE)
fullmodel
   # this is the final model


#another example from yahoo finance
library(quantmod)
eurusd <- getSymbols("EURUSD=X", from='2020-01-01', to = '2025-11-20', auto.assign = FALSE)
euro <- eurusd$`EURUSD=X.Close`
summary(euro)
# 3 missing values
# find them
euro_na <- euro[is.na(euro)]
euro_na_data <- index(euro_na)
euro[euro_na_data]
#use linear intepretation to replace missing values

euro <- na.approx(euro)
summary(euro)
euro[euro_na_data]
plot(euro)

leuro <- lag(euro)
#now you can start BJ modelling

#HW: download either exchange or stock
#take log
#do modeling with log










