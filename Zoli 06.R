setwd("C:/Pasāda/Corvinus Data Science In Business/Semester 5/Time Series Analysis/06/")

ARMA <- read.csv("ARMA.csv")

v1 <- ts(ARMA$v1)
plot(v1)
#condition of weak stationary:
#expected and variance should be constant in time
#expected value and variance seem time-invariant(constant) here
#time series seems stationary here
#ARMA model requires stationary process/ts 

#expected value of White noise = 0 if its an error term
#if its a process than can be not 0

#White Noise
#weak stationary 
#cov(x(t), x(t-k)) = 0 for all k values
#there is no autocorrelation and autocovariance 
#Possible test: Durbin-Watson, Ljung-Box, Breusch-Godfrey (BG)
#Durbin-Waton test is only for first-order autocorrelation
#LB and BG are appropriate to test higher order autocorrelation
#we prefer BG test
#if too many laggs, LB test can cause problem(it follows chi sq, but too many laggs don't follow chi sq)

library(lmtest)
bgtest(v1~1, order = 10)
#H0: there is no autocorrelation up to 10 lags
#H1: there is autocorrelation

#case A: we fail to reject H0, up to lags the ts is white noise
#it means that it is a random process, there is no need to build a
#model, there is no information which could be explained

#case B: we reject H0, there is information which should be explained,
#so we build possible ARMA models

#p-value is almost 0, we reject H0, there is autocorrelation

#we try to build the proper ARMA model to forecast v1 process

#ARIMA(p,d,q)
#for stationary process I(0), d=0
#d shows how many times we should take the difference of the ts
#to have a staionary process 
#I - order of integration
#ARMA(p,q)

#AR(p), p-th order autoregressive process
#in case of autoregressive process we use the lagged values
#of the dependent variable as explantory variables
#AR(1) - explanatory variable: y(t-1)
#AR(2) - explanatory variable: y(t-1), y(t-2)
#to define the order of the process we should check the correlogram
#correlogram: ACF and PACF
#for stationary autoregressive process the ACF converges to 0
#PACF drops to 0 after a given lag (k>p)

#MA(q) - q-th order moving average process
#in case of moving aveage process we use the lagged values
#of the error term as explantory variables 
#for moving average process the ACF drops to 0 after a given lag
#(k>q), PACF converges to 0

#check the acf and pacf
acf(v1)
#it converges to 0
pacf(v1)
#it drops to 0 after 1 lag
#4th lag is also out of the confidence interval of 0 value
#95% confidence interval of the 0 value. It means that 1 lag out of 20 (5/100)
#could be out of the interval randomly
#it seems just a random effect, there is no exact explanation to that case,
#that the first lag is significant, the 2nd and the 3rd are not significant
#and the 4th is significant again. it could be the case for the periodic 
#(e.g. quarterly) data, but it is not periodic
#conlusion: PACF drops to 0 after 1 lag, it could be AR(1)

#AR(1)
model1 <- arima(v1, order=c(1,0,0)) #ARIMA(p,d,q)
model1
#y(t) = 9.87+0.638*y(t-1)+u(t), where u(t) is normally distributed white noise ~ NWS
#in that estimation the intercept is the expected value
#t-test is almost 0 so significant
coeftest(model1) #z-test instead of t-test
#z test instead of the t test here
#because of the estimation technique,  it is not OLS, it is maximum likelihood 

#most important condition to forecast a ts 
#the error term should be white noise to do correct forecast
#if error not white noise, it is not totally random, it contains information to be explained, could be biased(?)
bgtest(resid(model1)~1, order=10) #test the errors
#H0: there is no autocorrelation up to 10 lags
#always right null hypothesis
#the p-value is 38%, we fail to reject H0, it is white noise up to 10 lag
#we can use this to forecast the ts
tsdiag(model1)
#it is an option

#normality of the error term
#Jargue-Berra test for normality
library(tseries)
jarque.bera.test(resid(model1))
#H0: the error term is normally distributed
#p-value is 88%, so we fail to reject H0,
#the error term is normally distributed

#ggplot
qqnorm(resid(model1))
qqline(resid(model1))
#if the ends are curved, it is t-distribution
#the line shows the normal distribution
#points fit to that line, the error term follows normal distribution

#AR(I) model is appropriate to forecast the ts, as the error term
#of the model is normally distributed white noise (NWS)

#Forecast
#types:
#in-sample or out-of-sample
#static and dynamnic forecast. Static uses only observed values
#in calculation, dynamic only estimated (e.g. y_hat)

#install.packages("forecast")
library(forecast)

#In-sample foarecast
plot(v1, col="black")
lines(fitted(model1), col="red")

#out of sample forecast
prediction <- predict(model1, n.ahead = 10) #forecast 10 future data points
prediction$pred
ts.plot(v1, prediction$pred, lty=c(1,3), col=c(5,2))
abline(h=9.87)
#the forecast converges to the expected value of the process,
#in this way we use the ARMA models only for short term forecasting

#Build ARMA model for v6
v6 <- ts(ARMA$v6)
plot(v6)
#it is not that nice than in case of v1
#it could be stationary

#if we have doubts about it, than try to find a statistical answer
#by now we know that for AR(1) the stationary condition was to 
#have |phi| < 1
#we have problem if the parameter is excatly 1, we call these process as unit root
#phi > 1, it would be explosive

#phi = 1 case helps us
#the autocorrelation does not converges to 0 (not sure if true or not)
acf(v6)
#it drops to 0 so it also shows stationarity
#ACF for AR processes converges to 0, for MA processes drops to 0
#if the process is not stationary, the values of ACF remains
#close to 1 for the lags 

#check ACF and PACF
acf(v6)
#it drops to 0 after 2 lags
#it could be maximum MA(2)
pacf(v6) #converges to 0 

#for long lag structure
#3rd order is the max
#more than 3 shows problem (heteroskadasticity, clustering, and volatility)
# dont model above AR(3)

#Note: we estimate maximum ARMA(3,3), the longer lag structure
#signs problems with ergodicity, in case of financial time series
#the longer lag structure signs heteroskadasticity problem,
#clustered volatility

#Possible models: MA(1) and MA(2)
#Zoli strategy to build model:
#start with highest model
#cause if MA(3) is not statisfactory(not white noise), then can drop MA(1) and MA(2)

#install.packages("tseries")
#MA(2)
ma2 <- arima(v6, order=c(0,0,2))
bgtest(resid(ma2)~1, order=10)
#white noise error term up to 10 lags
jarque.bera.test(resid(ma2))
#NWN
ma2
#y(t) = 2.946 + 0.535*u(t-1)-0.287*u(t-2) + u(t), where u(t)~NWN
#MA(2) is possible

#check MA(1)
ma1 <- arima(v6, order=c(0,0,1))
bgtest(resid(ma1)~1, order=10)
#p-value is almost 0, so the error term is not WN, we drop this mode

#To conclude the steps of ARMA modelling
#Step1: check stationarity
#Step2: check the correlogram ~ ACF and PACF
#Step3: acc to the correlogram determine the possible model version
#Step4: run the models, keep those in which the error term is NWN
#Step5: if you have more than 1 possible models, choose the best one
#based on AIC or BIC
#Step6: do the forecast


#v4
#self do not sure correct or not
v4 <- ts(ARMA$v4)
plot(v4)


acf(v4)
pacf(v4)

#possible model: ma(1)
ma1 <- arima(v4, order=c(0,0,1))
bgtest(resid(ma1)~1, order=10)
#H0: there is no autocorrelation up to 10 lags
#H1: there is autocorrelation

# Fit AR(1) to the residuals of your deterministic model

jarque.bera.test(resid(ma1))

AIC(ma1)
BIC(ma1)

ma1 #y(t) = 2.96+0.7347*y(t-1)+u(t)

library(forecast)
lines(fitted(ma1), col="red")
prediction <- predict(ma1, n.ahead = 10) #forecast 10 future data points
prediction$pred
ts.plot(v4, prediction$pred, lty=c(1,3), col=c(5,2))
abline(h=2.96) #intercept
