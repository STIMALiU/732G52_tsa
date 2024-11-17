

#-------------------------------------------------------------------------------
# # vitt burs: 
#-------------------------------------------------------------------------------

nobs<-200
y<-rnorm(n = nobs)
plot(y,t="l")

# SAC = acf()
acf(y)





#-------------------------------------------------------------------------------
# Stationäritet
#-------------------------------------------------------------------------------

# Tidsserien ska ha ett konstant väntevärde över tiden
# Tidsserien ska ha en konstant varians över tiden
# Autokorrelationen ska bara bero på det relativa avståndet mellan två tidpunkter


# en stationär tidserie (vitt brus)
nobs<-300
time_index<-1:nobs
set.seed(654)
y<-rnorm(n = nobs)
plot(time_index,y,t="l",ylim=c(-10,10))
acf(y)

# Icke-konstant varians
set.seed(6435)
y<-rnorm(n = nobs,mean = 0,sd = abs(rnorm(n = nobs,mean = scale(time_index)+1,sd = 0.01)))
plot(time_index,y,t="l",ylim=c(-10,10))
acf(y)

x<-scale(time_index)
# Icke-konstant väntevärde
set.seed(35)
y<-10+x+0.1*x^2-2*x^3+rnorm(n=nobs,sd=0.5)
plot(time_index,y,t="l")
acf(y,lag.max = 40)

set.seed(84)
# Icke-konstant väntevärde
# Icke-konstant varians
e<-rnorm(n = nobs,mean = 0,sd = abs(rnorm(n = nobs,mean = scale(time_index)+0.3,sd = 0.01)))
y<-10+x+0.1*x^2-2*x^3+e
plot(time_index,y,t="l")



#-------------------------------------------------------------------------------
# Autokorrelationen är inte stationär
#-------------------------------------------------------------------------------

# Nu testar vi att generera en tidserie där autokorrelation varierar över tid.
# I början varierar tidserien mjukt (= positiv autokorrelation), för att sedan
# slå över till en negativ autokorrelation.

#-------------------------------------------------------------------------------
# generera data:
N<-300  # antal obseravtioner
sd_val<-1
y0<-1
x_val<-seq(-2,2,length=N)
# serien skapas med en AR(1) modell där AR-parametern ändras över tid 
ar1_para_vect<- -tanh(4*x_val)
set.seed(71)
y<-rep(0,N)
for(i in 1:N){
  if(i==1){
    y[i]<-y0+rnorm(n = 1,mean = 0,sd = sd_val)    
  }else{
    y[i]<-ar1_para_vect[i]*y[i-1]+rnorm(n = 1,mean = 0,sd = sd_val)    
  }
}
#-------------------------------------------------------------------------------
# vi kollar på tidsserien:
plot(y,t="l")
abline(v=150,lty="dashed",col="red")
# vi ser att någonstans i mitten så ändrar serien karaktär 
# testa att köra om koden ovan med olika seeds.

# autokorrelation i början av serien:
acf(y[1:100])

# autokorrelation i slutet av serien:
acf(y[201:300])

# detta är ett (något överdrivet) exempel när autokorrelation ändras över tid
# detta innebär att autokorrelation inte bara beror på det relativa tidsavståndet
# mellan tidpunkter, utan även var i tiden som man undersöker.


#-------------------------------------------------------------------------------
# Exempel på stationär autokorrelation:
#-------------------------------------------------------------------------------

rm(list = ls())
library(ggplot2)
library(cowplot)
nobs<-300
x<-(1:nobs)

#-------------------------------------------------------------------------------
# fall 1
sd_val<-4
set.seed(76)
# vi simulerar e (slumptermen) så att den inte är oberoende, från en AR(1)-modell
# y_t = rho*y_(t-1) + error
set.seed(35)
# testa båda varianterna av e nedan:
e<-arima.sim(n = nobs,list(ar=c(0.9)),rand.gen = rnorm,sd=1)
e<-arima.sim(n = nobs,list(ar=c(0.7)),rand.gen = rnorm,sd=1)
plot(x,e,t="l")

# SAC
acf(e)
e_current<-e[2:300]
e_lag1<-e[1:299]
plot(e_lag1,e_current)

cor(e_lag1,e_current)

acf(e)
?acf
acf(e,lag.max = 30)
acf(e,lag.max = 30,type = "cov")

e_acf<-acf(e,lag.max = 30,plot = FALSE)
str(e_acf)
e_acf$acf
head(e_acf$lag)
head(e_acf$acf)
e_acf$acf[,,1]

# SPAC
e_pacf<-pacf(e)
head(e_pacf$acf)
head(e_pacf$lag)


#-------------------------------------------------------------------------------
# fall 2
set.seed(36)
e<-arima.sim(n = nobs,list(ar= -0.8),rand.gen = rnorm,sd=1)

plot(x,e,t="l")
e_current<-e[2:300]
e_lag1<-e[1:299]
plot(e_lag1,e_current)

cor(e_lag1,e_current)

acf(e)
#?acf
acf(e,lag.max = 40)
acf(e,lag.max = 30,type = "cov")



#-------------------------------------------------------------------------------
# Differentiering 
#-------------------------------------------------------------------------------


nobs<-300
time_index<-1:nobs
set.seed(654)
y<-10+0.05*time_index+rnorm(n = nobs)
plot(time_index,y,t="l")
acf(y,lag.max = 50)
# diff med 1 tidsförskjutning (lag=1)
z<-diff(x = y,lag = 1)
plot(time_index[-1],z,t="l")
acf(z,lag.max = 50)

# inversen av diff
# xi = första värdet på orginal tidserien
y2<-diffinv(x = z,xi = y[1])
all(y2==y)
# notera att differensen är skillnaden mellan två på varandra följande värden


