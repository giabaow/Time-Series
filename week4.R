set.seed(1992)
WhitenoiseTable <- data.frame(Date = 1:1000,
                              Y_t = runif(1000, -20, 60))
str(WhitenoiseTable)

###
library(ggplot2)
ggplot(WhitenoiseTable, aes(x = Y_t)) +
  geom_line(aes(y = Date))
###

set.seed(1992)
WhitenoiseTable$X_t <- rnorm(1000, 80, 20)

library(ggplot2)
ggplot(WhitenoiseTable, aes(x = Date)) +
  geom_line(aes(y=Y_t, color = "Y_t"), size = 1) +
  geom_line(aes(y=X_t, color = "X_t"), size = 1) +
  geom_hline(yintercept = 20, color = "blue", size = 1) +
  geom_hline(yintercept = 80, color = "red", size = 1) 
#it appears that both time series are stationary in a weak sense: they fluctuate around 
#their fixed mean with a standard deviation that appears to be constant.

#Theoretical value of mean and sd:
#Y_t: mean = (60+(-20))/2 = 20 (a+b/2), sd = (60+(-20))/sqrt(12) = 23.09 (a+b/sqrt(12))
#X_t: mean = 80, sd = 20

#we can calculate the adjusted sampled standard deviations (to get unbiased estimate)
sd(WhitenoiseTable$Y_t) #23.01883
sd(WhitenoiseTable$X_t) #19.50523

#Check with histogram
hist(WhitenoiseTable$Y_t)
hist(WhitenoiseTable$X_t)


###--------------------------------------------------------------
#Examination of the autocorrelation of white noise

acf(WhitenoiseTable$Y_t)
pacf(WhitenoiseTable$Y_t)
#Unfortunately, both ACF and PACF do so in the case of 𝑘=6 lag! 
#But neither value is so far out of the range, 
#so we might almost believe that we have generated a 
#white noise that is uniformly distributed.

#In the case of k = 6,  it would be nice to do some hypothesis testing 
#to see if our time series is really a white noise.



acf(WhitenoiseTable$X_t)
pacf(WhitenoiseTable$X_t)
#it’s not white noise we’ve generated


###--------------------------------------------------------------
#Statistical testing of white noise processes
###------------------------------------------
#1. The Ljung-Box test
#H0:p1 = p2 =...= 0
#H1: exist pj # 0
autocor <- acf(WhitenoiseTable$X_t, plot = FALSE)
str(autocor)
autocor_df <- data.frame(Lags = autocor$lag[2:31,1,1],
                         autocor = autocor$acf[2:31,1,1])
str(autocor_df)

#The distribution of the test is X^2(p-b), 
#so we expand the table with the part of the test function
autocor_df$ToolForTeststatistic <- autocor_df$autocor^2/(1000-autocor_df$Lags)
Teststatistic <- (1000)*(1000+2)*sum(autocor_df$ToolForTeststatistic)

#p value
1-pchisq(Teststatistic, 30-0)
#0.5637739 -> 56.4%, so even alpha = 10%, we can accept H0 -> with 90% confidence

#Built in function
Box.test(WhitenoiseTable$X_t, lag = 30, type = "Ljung-Box")


###------------------------------------------
#2.The Breusch-Godfrey test
library(lmtest)
bgtest(WhitenoiseTable$X_t ~ 1, order = 30)
#p-value = 0.5201
#we accept 𝐻0 that our time series Xt is a white noise for 
#30 lags in the population

#better for testing residual autocorrelation



###------------------------------------------
#The Durbin-Watson test
dwtest(WhitenoiseTable$X_t ~ 1)
#p-value = 0.504
#so we can accept 𝐻0 for all the usual 𝛼, 
#that the time series can be considered white noise in the out-of-sample world









###------------------------------------------------------------------------------------
#Load the Lottery.xlsx file
library(readxl)
Lottery <- read_excel("Lottery.xlsx")
Lottery$Date <- as.Date(Lottery$Date)
str(Lottery)
Lottery$Mean <- rowMeans(Lottery[, 12:16])
#the Mean time series is the sum of random effects, 
#so it must be normally distributed by the Central Limit Theorem
#the theoretical mean of the time series should be (90+1)/2 = 45.5

ggplot(Lottery, aes(x=Date, y=Mean)) +
  geom_line() + 
  geom_hline(yintercept = 45.5, color="blue")
#It looks quite white noise…at least 
#the time series looks stationary with the given uniform distribution mean

hist(Lottery$Mean)
#the bell-shaped curve of the normal distribution we expect comes out very nicely.

#Well, but are the elements not autocorrelated? 
#That is, can they also be taken as white noise?
#Let’s see the correlograms:
acf(Lottery$Mean)
pacf(Lottery$Mean)
#This makes it seem plausible: both 𝐴𝐶𝐹 and 𝑃𝐴𝐶𝐹 
#only exit the 95% confidence interval around 0 
#for a small number of lags (e.g., lags 21 and 30), 
#and even if they do, they don’t exit very much.


#But let’s take a look at our three hypotheses tests to answer the question for sure!
Box.test(Lottery$Mean, lag = 35, type = "Ljung-Box")
#35 is suggested by acf function
#p-value = 0.475

library(lmtest)
bgtest(Lottery$Mean ~ 1, order = 35)
#p-value = 0.4482

dwtest(Lottery$Mean ~ 1)
#p-value = 0.4482 

#Accept H0: the weekly average of the lottery numbers can be considered as white noise




###-----------------------------------------------
#Number of tickets with 2 matches
Lottery_Converted <- Lottery[Lottery$Date > "1998-01-03",]
ggplot(Lottery_Converted, aes(x=Date, y=Numberofwinners2)) + geom_line()
#not stationarity
#here is some upward trend between 2000-2004, 
#a downward trend between 2004-2010, and some cycle effect.

acf(Lottery_Converted$Numberofwinners2)
pacf(Lottery_Converted$Numberofwinners2)

Box.test(Lottery_Converted$Numberofwinners2, lag=30, type="Ljung-Box")
bgtest(Lottery_Converted$Numberofwinners2 ~ 1, order = 30)
dwtest(Lottery_Converted$Numberofwinners2 ~ 1)

#Out of curiosity, we can look at the total amount of money paid out 
#on the tickets with two matches in HUF.
#This is the Prize2 timeline
ggplot(Lottery_Converted, aes(x=Date, y=Prize2)) + geom_line()
acf(Lottery_Converted$Prize2)
pacf(Lottery_Converted$Prize2)
#What is worth to note about 𝑃𝐴𝐶𝐹 is that 
#because the upward trend is so strong in this time series, 
#even in 𝑃𝐴𝐶𝐹 there are quite distant lags, e.g. 14, 
#where the partial autocorrelation leaves the 95% confidence interval around 0.

#the absolute value of the regular 𝐴𝐶𝐹 doesn’t start to decline because of the strong trend



