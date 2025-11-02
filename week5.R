AR_MA <- read.csv("AR_MA.csv")
str(AR_MA)
library(ggplot2)

#AR process
ggplot(AR_MA, aes(x=t, y=X_t)) + geom_line(size=1)
acf(AR_MA$X_t)
pacf(AR_MA$X_t) #=> deviate significantly from 0 only up to lag 𝑘=3 AR(3)

#MA process
ggplot(AR_MA, aes(x=t, y=Y_t)) + geom_line(size=1)
acf(AR_MA$Y_t) #=> we deviate significantly from 0 only up to lag 𝑘=4 MA(4)
pacf(AR_MA$Y_t)

#ARMA Process 
ggplot(AR_MA, aes(x=t, y=Z_t)) + geom_line(size=1)
acf(AR_MA$Z_t)
pacf(AR_MA$Z_t)


#Simulation
set.seed(2018)
AR_MA$u_t_v1 <- rnorm(200,0,10)
set.seed(2019)
AR_MA$u_t_v2 <- rnorm(200,0,10)
set.seed(2020)
AR_MA$u_t_v3 <- rnorm(200,0,10)
set.seed(2021)
AR_MA$u_t_v4 <- rnorm(200,0,10)

str(AR_MA)

AR_MA$ARMA_v1 <- 3
AR_MA$ARMA_v2 <- 3
AR_MA$ARMA_v3 <- 3
AR_MA$ARMA_v4 <- 3

str(AR_MA)
for (index in 4:10) {
  print(index)
}

for (index in 3:200) {
  AR_MA$ARMA_v1[index] <- 3 + 0.8*AR_MA$ARMA_v1[index-1] - 0.4*AR_MA$ARMA_v1[index-2] + 0.7*AR_MA$u_t_v1[index-1] + 0.3*AR_MA$u_t_v1[index-2] + AR_MA$u_t_v1[index]
  
  AR_MA$ARMA_v2[index] <- 3 + 0.8*AR_MA$ARMA_v2[index-1] - 0.4*AR_MA$ARMA_v2[index-2] + 0.7*AR_MA$u_t_v2[index-1] + 0.3*AR_MA$u_t_v2[index-2] + AR_MA$u_t_v2[index]
  
  AR_MA$ARMA_v3[index] <- 3 + 0.8*AR_MA$ARMA_v3[index-1] - 0.4*AR_MA$ARMA_v3[index-2] + 0.7*AR_MA$u_t_v3[index-1] + 0.3*AR_MA$u_t_v3[index-2] + AR_MA$u_t_v3[index]
  
  AR_MA$ARMA_v4[index] <- 3 + 0.8*AR_MA$ARMA_v4[index-1] - 0.4*AR_MA$ARMA_v4[index-2] + 0.7*AR_MA$u_t_v4[index-1] + 0.3*AR_MA$u_t_v4[index-2] + AR_MA$u_t_v4[index]
}
str(AR_MA)

ggplot(AR_MA, aes(x=t)) +
  geom_line(aes(y=ARMA_v1, color="v1"), size=1) +
  geom_line(aes(y=ARMA_v2, color="v2"), size=1) +
  geom_line(aes(y=ARMA_v3, color="v3"), size=1) +
  geom_line(aes(y=ARMA_v4, color="v4"), size=1)


#Fitting ARMA Models and Estimating Coefficients
acf(AR_MA$ARMA_v4) #MA(1,2)
pacf(AR_MA$ARMA_v4) #AR(1,2,3) 
#=> ARMA(3,2) ARMA(2,2) ARMA(1,1)

my_arma_model <- arima(AR_MA$ARMA_v4, order = c(3,0,2))
my_arma_model
library(lmtest)
coeftest(my_arma_model)
#coefficients are not significant -> NO

my_arma_model <- arima(AR_MA$ARMA_v4, order = c(2,0,2))
my_arma_model
library(lmtest)
coeftest(my_arma_model)
#now significant

resid <- my_arma_model$residuals
acf(resid)
pacf(resid)
bgtest(resid ~ 1, order = 23)



#4Theoretical Properties of ARMA Processes
#4.1. The Case of AR(p) Processes
set.seed(2021)
u_t <- rnorm(n=300, 0, 1)

# generating AR(2)
ar_2 <- rep(1, times=300)

for (index in 3:300) {
  ar_2[index] <- 1 + 0.8*ar_2[index-1] - 0.1*ar_2[index-2] + u_t[index]
}

str(ar_2)

#theoretical 
#𝑐=1
#𝜙1=0.8
#𝜙2=−0.1
mean_theoretical <- 1/(1-(0.8-0.1)) #3.333333
variance_theoretical <- ((1+0.1)/(1-0.1))*(1/((1+0.1)^2-0.8^2)) #2.14425
acf_0 <- 1
acf_1 <- 0.8/(1+0.1) #0.7272727
acf_2 <- 0.8*acf_1 - 0.1*acf_0 #0.4818182

#emperical
mean(ar_2) #3.113222
var(ar_2) #2.335028
acf(ar_2, lag.max = 2, plot = FALSE) #1.000 0.728 0.492

#quite similar => we have fitted the right 𝐴𝑅𝑀𝐴(𝑝,𝑞) model to our time series


#4.2. The Case of MA(q) Processes
set.seed(2021)
u_t <- rnorm(n=300, 0, 1)

# generating MA(2)
ma_2 <- rep(1, times=300)

for (index in 3:300) {
  ma_2[index] <- 1 - 0.7*u_t[index-1] + 0.5*u_t[index-2] + u_t[index]
}

str(ma_2)
mean_theoretical <- 1
variance_theoretical <- 1 + (-0.7)^2 + 0.5^2
acf_0 <- 1
acf_1 <- ((-0.7)+(-0.7*0.5))/(1+(-0.7)^2+0.5^2)
acf_2 <- 0.5/(1+(-0.7)^2+0.5^2)
mean_theoretical

mean(ma_2)
var(ma_2)
acf(ma_2, lag.max = 2, plot = FALSE)










