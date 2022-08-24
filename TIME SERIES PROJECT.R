# PROJECT ON TIME SERIES ANALYSIS

#load required r packages
library(IRdisplay)
library(magrittr)
library(tidyverse)
library(scales)
library(gridExtra)
library(forecast)
library(tseries)
library(ggthemes)
theme_set(theme_economist())

#load helper R functions
setwd("C:/Users/Administrator/Desktop/Time Series Project Materials/")
source("R Functions/compare_models_function.R")
source("R Functions/sim_random_walk_function.R")
source("R Functions/sim_stationary_example_function.R")

print("Loading is completed")

# Packages successfully installed
# Now, we will compare a basic linear regression model
# and a basic AR(1) time series model 
compare.models(n=100)
 
# simulating the random walk model

dat <- sim.random.walk()

#plotting the random walk model

# acf and pacf plots 
dat %>% ggplot(aes(t,X)) + geom_line() +
  xlab("T") + ylab("X") + ggtitle("ACF plot")

ggAcf(dat$X,type="correlation")+ ggtitle("ACF")
ggAcf(dat$X,type="partial")+ ggtitle("PACF")

# dealing with stationarity and plots 
df <- sim.stationary.example(n=1000)
head(df);dim(df)

#plotting non stationary and stationary

g1<-ggplot(df,aes(x=t,y=X1)) + geom_line() +
  xlab("T") + ylab("X1") + ggtitle("Nonstationary")
g2<-ggplot(df,aes(x=t,y=X3)) + geom_line() +
  xlab("T") + ylab("X3") + ggtitle("stationary")
grid.arrange(g1,g2)

# ACF plot for stationary and non stationary 
g1<-ggAcf(df$X1,type="correlation")+xlab("T")+
  ylab("X1")+ ggtitle("Nonstationary")
g2<-ggAcf(df$X3,type="correlation")+xlab("T")+
  ylab("X3")+ ggtitle("stationary")

# augmented dickey fuller test (adf)
adf.test(df$X1)
adf.test(df$X3)

# given that we know X1 is non stationary,
#we will use differncing for conversion 
diff<- df$X1 - lag(df$X1,1)
#plotting it out (order of differencing is 1) 
g1<- ggAcf(df$X1,type="correlation")
g2<- ggAcf(df$X2,type="correlation")

# detrending 
detrend<- resid(lm(X2~t,data=df))
#plotting
g1<-ggAcf(df$X2,type="correlation")
g2<-ggAcf(detrended,type="correlation")

# AR,MA,ARMA,ARIMA, SLT models and box jenkins method 
# Firstly, we will load the data 
ur <-read.csv("file path")
head(ur);dim(ur)

# checking the data 
#chaing the class of objects if necessary 
# check the time series plot 
ggplot(ur,aes(x="",y="")) + geom_line()
ggAcf(ur$y-value,type="correlation")
adf.test(ur$y-value)
# now, we will use built in functions to fit 
# our different models
ar.model<- auto.ar(ur$y-value,max.d=0,max.q=0,allowdrift=T)
ma.model<- auto.ma(ur$y-value,max.d=0,max.p=0,allowdrift=T)
arma.model<- auto.arma(ur$y-value,max.d=0,allowdrift=T)
arima.moedel<- auto.arima(ur$y-value,allowdrift=T)

# checking the  residuals of the model fit
#calculating the residuals
ar.residual <- resid(ar.model)
ma.residual <- resid(ma.model)
arma.residual <- resid(arma.model)
arima.residual <- resid(arima.model)

# plotting the pacf plot 
ggAcf(ar.residual,type"partial")
ggAcf(ma.residual,type"partial")
ggAcf(arma.residual,type"partial")
ggAcf(arima.residual,type"partial")

# running the Ljung Box test on residuals

Box.test(ar.residual,type="Ljung-Box",lag=1)
Box.test(ma.residual,type="Ljung-Box",lag=1)
Box.test(arma.residual,type="Ljung-Box",lag=1)
Box.test(arima.residual,type="Ljung-Box",lag=1)

# making a forecast for each model
# using the forecast package 
ar.forecast <- forecast(ar.model,h=24,level=80)
ma.forecast <- forecast(ma.model,h=24,level=80)
arma.forecast <- forecast(arma.model,h=24,level=80)
arima.forecast <- forecast(arima.model,h=24,level=80)

# plotting the forecast using autoplot func

g1 <- autoplot(ar.forecast)
g2 <- autoplot(ma.forecast)
g3 <- autoplot(arma.forecast)
g4 <- autoplot(arima.forecast)
grid.arrange(g1,g2,g3,g4,nrow=2,ncol=2)

# fitting the STL decomposition model

# specify frequency, use the ts func

ur.ts <- ts(ur$y-value,frequency = 12)

# fitting thr model

stl.model <- stl(ur.ts,s.window = "periodic")

#plotting the model
autoplot(stl.model)

#make forecast 
stl.forecast <- forecast(stl.model)


# ----------- END OF PROJECT------------------#