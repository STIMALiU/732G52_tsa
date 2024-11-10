

rm(list=ls())
library(fpp3)



?decompose


data("AirPassengers")
class(AirPassengers)
?AirPassengers
plot(AirPassengers)
plot(log(AirPassengers))
library(lmtest)
air_time<-as.Date(AirPassengers)

# komponentuppdelning för AirPassengers
plot(AirPassengers)  # kolla på data

# vi ser att amplituden på säsongen ökar med värdet på tidserien
# det tyder på att en multiplikativ modell är lämplig
# om säsongen är mer konstant över tid som är additiv modell mer lämplig.


# notera att i denna modell (i decompose() ) skattas trenden med ett 
# glidande medelvärde
?decompose
# The additive model used is:
# Y[t] = T[t] + S[t] + e[t]
# The multiplicative model used is:
# Y[t] = T[t] * S[t] * e[t


# vi börjar med att en multiplikativ modell
# filter decompose() bestämmer vikterna i glidande medelvärde och 
# ska summera till 1.

filter_length<-11 # vi bestämmer längden på filtret
my_filter1<-rep(1/filter_length,filter_length) # vi låter alla obs i filtret ha
# samma vikt

G1<-decompose(AirPassengers,type = "mult",filter = my_filter1)
plot(G1)
# olika delar av uppdelningen:
str(G1)
# kolla på säsong
plot(G1$seasonal[1:12],pch=16)
abline(h =1)
barplot(G1$seasonal[1:12])
abline(h =1)
# eftersom vi har en multiplikativ modell så ligger säsongskomponenten runt 1

# kolla på trend
plot(G1$trend)
# slumptermen/residualerna:
plot(G1$random)

y_hat1<-G1$seasonal*G1$trend
# vi ser en bra anpassning
plot(air_time,AirPassengers,t="o",lwd=1.5)
lines(air_time,y_hat1,col="red",t="l",lwd=1.5)
library(cowplot)
library(ggplot2)
source("https://raw.githubusercontent.com/STIMALiU/732G52_tsa/refs/heads/main/code/residual_diagnostics.R")


# notera att jag justerat residualerna så att de är runt 0,
# det går även att ändra koden i residual_diagnostics() så att plottar 
# blir runt 1 istället
residual_diagnostics(res_vect = as.vector(G1$random-1),fit_vect = as.vector(y_hat1))
acf(as.vector(na.omit(G1$random)))
# finns beroende kvar


# testar en additiv modell
G2<-decompose(AirPassengers,type = "add",filter = my_filter1)
plot(G2)
# olika delar av uppdelningen:
str(G2)
# kolla på säsong
plot(G2$seasonal[1:12],pch=16)
abline(h =0)
barplot(G2$seasonal[1:12])
abline(h =0)
# kolla på trend
plot(G2$trend)
# slumptermen/residualerna:
plot(G2$random) # ser inte bra ut!!!

y_hat2<-G2$trend+G2$seasonal
# inte lika bra anpassning!
# överskattar trenden i början, och underskattar trenden i slutet
plot(air_time,AirPassengers,t="o",lwd=1.5)
lines(air_time,y_hat2,col="red",t="l",lwd=1.5)
# en sämre anpassning här!

library(cowplot)
library(ggplot2)
residual_diagnostics(res_vect = as.vector(G1$random-1),fit_vect = as.vector(y_hat1))
# finns ett tydligt beroende kvar i slumptermen:
acf(as.vector(na.omit(G2$random)))
# mer beroenden här!

# utvärderingsmått:

MSD_G1<-mean((AirPassengers-y_hat1)^2,na.rm = TRUE)
MSD_G2<-mean((AirPassengers-y_hat2)^2,na.rm = TRUE)

MAD_G1<-mean(abs(AirPassengers-y_hat1),na.rm = TRUE)
MAD_G2<-mean(abs(AirPassengers-y_hat2),na.rm = TRUE)

MAPE_G1<-mean(abs((AirPassengers-y_hat1)/AirPassengers),na.rm = TRUE)
MAPE_G2<-mean(abs((AirPassengers-y_hat2)/AirPassengers),na.rm = TRUE)

df_error<-data.frame(MSD=c(MSD_G1,MSD_G2),MAD=c(MAD_G1,MAD_G2),MAPE=c(MAPE_G1,MAPE_G2))
rownames(df_error)<-c("mult","add")
df_error
# vilken modell är bäst här?

# testa att ändra i storleken i filtret för det glidande medelvärdet i koden ovan.


# finns många funktioner för glidande medelvärde (moving average) i R, ex:
?zoo::rollmean()
# andra exempel:
# filter(), ma() i paketet forcast, SMA() i paketet TTR och sma() i paketet smooth.



#-------------------------------------------------------------------------------
# Fler dataset: 
#-------------------------------------------------------------------------------

# Gör komponentuppdelning för tempdub data. Testa båda typerna av modeller.
# Undersök de skattade komponenterna. Undersök anpassningen till data-
# Beräkna utvärderingsmått för båda modellerna.
# Var modellerna bra? Vilken var bäst av de två?

# Upprepa sedan för winnebago och/eller oilfilters.


