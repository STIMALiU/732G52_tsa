#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Del 1
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# ts-objekt
#-------------------------------------------------------------------------------

rm(list=ls()) # rensar i den globala miljön


# vi skapar ts-objekt med ts()
# se video:
# https://www.youtube.com/watch?v=0RVGyvFzwwk
# https://www.youtube.com/watch?v=uW3PQmzvUcw

#-------------------------------------------------------------------------------
# simulera lite data
x1<-c(4,5,7,5,8,12,20,17,15,16,19,22,25,26,22,15,11,16,20,26)
?ts
# skapar årsdata:
y1<-ts(data = x1,start = 2003,frequency = 1)
print(y1)
plot(y1)
# notera skalan på x-axeln

# ger samma som
plot.ts(y1)
# Den generiska funktionen plot() har en ts-metod: plot.ts()

is.ts(y1)
class(y1)
str(y1)

# olika funktioner på ts-objekt:
start(y1)
end(y1)
frequency(y1)
time(y1)



library(lmtest)
# Vi kan ta ut datum från ett ts-objekt:
y1_date<-as.Date(y1)
y1_date 



# vi kan även plotta med ggplot2 via paketet ggfortify
# se: https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_ts.html
library(ggfortify)
autoplot(y1)+theme_bw()+ggtitle("Min tidserie")



# skapar kvartalsdata:
y2<-ts(data = x1,start = 2009,frequency = 4)
plot(y2)
# notera skalan på x-axeln

as.Date(y2)


#-------------------------------------------------------------------------------
# månadsdata med ts:

# skapar data:
set.seed(354)
time_val<-1:60
set.seed(3432)
x3<-2*sin((2*pi)/(12)*time_val)+rnorm(n =  length(time_val),mean = 0,sd = 0.4)+12


y3<-ts(data = x3,start = 2017,frequency = 12)
plot(y3)

library(lmtest)
# Vi kan ta ut datum från ett ts-objekt:
y3_date<-as.Date(y3)
y3_date
library(lubridate)
# vi kan ta fram olika typer av kalenderdata:
month(y3_date)
year(y3_date)
day(y3_date)

?wday
wday(y3_date) 
# notera 1 kan betyda "måndag" eller "söndag" beroende på
# vilken inställning som man har

wday(y3_date,label = TRUE)


week(y3_date) # veckonummer





#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Del 2
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# AirPassengers data
#-------------------------------------------------------------------------------
rm(list=ls()) # rensar i den globala miljön

data("AirPassengers")
class(AirPassengers)
# ts objects:
#https://www.youtube.com/watch?v=uW3PQmzvUcw
?AirPassengers

AirPassengers
print(AirPassengers)
# hur ser plotten ut?
# vilka egenskaper har tidserien?

plot(AirPassengers)
# Vi ser en stigande trend (inte säkert att det är linjär, ser lite böjd ut), 
# och ett tydligt säsongsmönster
# Vi ser även att variansen ökar med tiden -> vi testar med log-transform

plot(log(AirPassengers))
# nu ser serien mer stabil ut, variansen verkar vara ungefär samma för hela
# tidsspannet. Vi ser en tydlig trend och säsongsberoende.


#-------------------------------------------------------------------------------
# SAC: Sample autocorrelation function
#-------------------------------------------------------------------------------

# tas fram med acf() funktionen
# acf = autocorrelation function
?stats::acf
acf(x = as.vector(AirPassengers))
acf(x = as.vector(AirPassengers),lag.max = 40) # ökar antalet laggar som visas
# vi ser att tidserien inte är stationär
# Det ser vi även genom att bara kolla på
plot(AirPassengers)

# vi kan ta ut värdena för acf om vi vill:
acf_val<-acf(x = as.vector(AirPassengers),plot = FALSE)
str(acf_val)
acf_val$acf[,,1]
plot(acf_val$acf[,,1],ylim=c(0,1))


# plotta acf med ggplot2:
library(ggfortify)
autoplot(acf(x = as.vector(AirPassengers),plot = FALSE,lag.max = 40))+theme_bw()


# testa att beräkna olika laggar (diffar) på serien:
diff(AirPassengers) # lag 1 - notera att vi förlorar den första obs i data
diff(AirPassengers,lag = 12) # lag 12 - vi förlorar de första 12 obs i data

AirPassengers_lag1<-diff(AirPassengers)
plot(AirPassengers_lag1)
AirPassengers_lag12<-diff(AirPassengers,lag = 12)
plot(AirPassengers_lag12)

acf(as.vector(AirPassengers_lag1),lag.max = 40)
# vi ser stora laggar på multiplar av 12 
# -> vi har ett tydligt månadsmönster i data


acf(as.vector(AirPassengers_lag12),lag.max = 40)


AirPassengers_lag1_lag12<-diff(AirPassengers_lag1,lag = 12)
plot(AirPassengers_lag1_lag12)
acf(as.vector(AirPassengers_lag1_lag12),lag.max = 40)
# nu verkar serien vara stationär

# upprepa ovanstående, men ta log-transform på AirPassengers först.


#-------------------------------------------------------------------------------
# tempdub data
#-------------------------------------------------------------------------------
library(TSA)
data(tempdub)
?tempdub
class(tempdub)
plot(tempdub)
autoplot(tempdub)

# vad är det för sorts data?
# vilken tidsskala? vilket tidsspann?
# beskriv tidseriens egenskaper: trender, säsonger, stationär etc
# undersök SAC och olika laggar på serien

#-------------------------------------------------------------------------------
# CREF data
#-------------------------------------------------------------------------------

data(CREF)
?CREF
plot(CREF)

# vad är det för sorts data?
# vilken tidsskala? vilket tidsspann?
# beskriv tidseriens egenskaper
# undersök SAC och olika laggar på serien

#-------------------------------------------------------------------------------
# larain data
#-------------------------------------------------------------------------------
?larain
data(larain)
plot(larain)

# vad är det för sorts data?
# vilken tidsskala? vilket tidsspann?
# beskriv tidseriens egenskaper
# undersök SAC och olika laggar på serien

#-------------------------------------------------------------------------------
# winnebago data
#-------------------------------------------------------------------------------
?winnebago
data(winnebago)
plot(winnebago)

# vad är det för sorts data?
# vilken tidsskala? vilket tidsspann?
# beskriv tidseriens egenskaper
# undersök SAC och olika laggar på serien


#-------------------------------------------------------------------------------
# oilfilters data
#-------------------------------------------------------------------------------

data("oilfilters")
?oilfilters
plot(oilfilters)


# vad är det för sorts data?
# vilken tidsskala? vilket tidsspann?
# beskriv tidseriens egenskaper
# undersök SAC och olika laggar på serien



#-------------------------------------------------------------------------------
# star data
#-------------------------------------------------------------------------------
?star
data(star)
plot(star)

# vad är det för sorts data?
# vilken tidsskala? vilket tidsspann?
# beskriv tidseriens egenskaper
# undersök SAC och olika laggar på serien


