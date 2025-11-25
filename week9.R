#load file
US <- read.csv("USA_macro_q.csv")

US_ts <- ts(US, frequency = 4, start = c(1947,1))
plot.ts(US_ts[,2:4])
#not stationary: GDP, M3R
#stationary: UNEMP
#GDP, M3R have exponential trends

lGDP <- log(US_ts[,2])
lM3R <- log(US_ts[,4])
unemp <- US_ts[,3]

#adf, kpss tests: GDP and money needs differencing 
#i will upload the proper test

dlGDP <- diff(lGDP, differences = 1, lag = 1)
dlM3R <- diff(lM3R, differences = 1, lag = 1)

plot.ts(dlGDP)
plot.ts(dlM3R)
#there are stationary! unemp is also stationary

#estimate a VAR(2)

install.packages("vars")
library(vars)
























