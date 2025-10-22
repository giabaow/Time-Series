library(readxl)

GerGDP <- read_excel("GDP_Germany.xlsx")

str(GerGDP)

#----------------------------------------------------
#Moving average
install.packages("zoo")
library(zoo)

GerGDP$MA12 <- rollmean(GerGDP$gdp_per_capita, k = 12, fill = NA)
library(ggplot2)
ggplot(GerGDP, aes(x=year)) +
  geom_line(aes(y=gdp_per_capita, color="True Y"), size=1) +
  geom_line(aes(y=MA12, color="MA(12)"), size=1)

GerGDP$MA8 <- rollmean(GerGDP$gdp_per_capita, k = 8, fill = NA, align = "right")

ggplot(GerGDP, aes(x=year)) +
  geom_line(aes(y=gdp_per_capita, color="True Y"), size=1) +
  geom_line(aes(y=MA8, color="MA(8)"), size=1)


#---------------------------------------------------------------------
#Exponential Moving Average/Smoothing
intall.packages("pracma")
library(pracma)

GerGDP$Expon <- movavg(GerGDP$gdp_per_capita, n = 9, type ="e")
ggplot(GerGDP, aes(x=year)) +
  geom_line(aes(y=gdp_per_capita, color="True Y"), size=1) +
  geom_line(aes(y=Expon, color="Exponential Smoothing"), size=1)


#---------------------------------------------------------------------
#The Hodrick–Prescott Filter
install.packages("mFilter")
library(mFilter)
HP_GerGDP <- hpfilter(GerGDP$gdp_per_capita, freq = 100, type = "lambda")
str(HP_GerGDP)
#the result is a list-object, we will add it to our dataframe
GerGDP$HP <- HP_GerGDP$trend[,1]

ggplot(GerGDP, aes(x=year)) +
  geom_line(aes(y=gdp_per_capita, color="Valós Y"), size=1) +
  geom_line(aes(y=HP, color="H-P szűrő"), size=1)

GerGDP$HP_Cycle <- HP_GerGDP$cycle

ggplot(GerGDP, aes(x=year)) +
  geom_line(aes(y=HP_Cycle), size=1)



#---------------------------------------------------------------------
Finland <- read_excel("MacroData_Finland.xlsx")

Finland$Date <- as.Date(Finland$Date)
str(Finland) 

Finland$LogGDP <- log(Finland$GDP)
Finland$LogI <- log(Finland$Investment)
Finland$LogC <- log(Finland$Consumption)

#We will use the H-P filter to generate the cycle component of 
#all three log-transformed time series. 
#We pay attention to the fact that these time series are quarterly, 
#so we will use 𝜆=1600 in the H-P filter.
Finland$Cik_GDP <- hpfilter(Finland$LogGDP, freq = 1600, type = "lambda")$cycle
Finland$Cik_I <- hpfilter(Finland$LogI, freq = 1600, type = "lambda")$cycle
Finland$Cik_C <- hpfilter(Finland$LogC, freq = 1600, type = "lambda")$cycle

ggplot(Finland, aes(x=Date)) +
  geom_line(aes(y=Cik_GDP, color="GDP"), size=1) +
  geom_line(aes(y=Cik_I, color="Inv"), size=1) +
  geom_line(aes(y=Cik_C, color="Cons"), size=1)

#Persistence
#measure how quickly a cycle dies out over time,
#This is typically measured by the first-order autocorrelation
acf(Finland$Cik_GDP, lag.max = 1, plot = FALSE)
acf(Finland$Cik_I, lag.max = 1, plot = FALSE)
acf(Finland$Cik_C, lag.max = 1, plot = FALSE)

#Co-movement
cor(Finland[,8:10])

#Lead-Lag Simultaneity
ccf(Finland$Cik_GDP, Finland$Cik_I, lag.max = 6, plot = FALSE)
ccf(Finland$Cik_GDP, Finland$Cik_C, lag.max = 6, plot = FALSE)
#The highest absolute correlation occurs at 𝑘=0, 
#so we can say that investment is a coincident variable with GDP.

#Magnitude of Fluctuations (volatility)
library(psych)
describe(Finland[,8:10])
#Based on the standard deviations in the sd column, 
#the amplitude of the investment cycles fluctuates the most. 
#This is not surprising, as investments often depend on 
#the “whims” of business leaders!



