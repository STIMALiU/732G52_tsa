#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Tidserieregression
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

rm(list=ls()) # rensar i den globala miljön

# i detta skript ska vi testa att modellera trender och säsonger med hjälp av
# regression.


# läser in en funktion som utvärderar residualerna med olika plottar:
source("https://raw.githubusercontent.com/STIMALiU/732G52_tsa/refs/heads/main/code/residual_diagnostics.R")
# funktionens kod finns:
# https://github.com/STIMALiU/732G52_tsa/blob/main/code/residual_diagnostics.R
# för att läsa in automatiskt så måste man klicka på "raw" och sen kopiera
# webbadressen  till source()-funktionen.

# funktionen residual_diagnostics har argumenten:
# res_vect = vector with residuals 
# fit_vect = vector with fitted values
# alpha = transparency of points 
# binwidth = binwidth for the histogram


#-------------------------------------------------------------------------------
# tempdub data: linjär trend
#-------------------------------------------------------------------------------



library(TSA)
data(tempdub)
?tempdub
class(tempdub)
plot(tempdub)
library(lmtest)
tempdub_time<-as.Date(tempdub)

# vi kan inte använda en datum-vektor direkt i regressionsmodelleringen 
# vi skapar ett index-variabel, som kommer att vara vår förklarande variabel
# över tiden.

nobs<-length(tempdub)
time_index<-1:nobs
head(time_index)
tail(time_index)

# anpassar en regressionsmodell med en enkel linjär trend
lm_temp<-lm(tempdub~time_index)
summary(lm_temp)
# vi noterar att det inte finns ett signifikant samband för tidsindex
# data verkar inte ha stöd för en linjär trend, vilket vi också såg när
# vi plottade data.

res_vect<-residuals(lm_temp)
fit_vect<-fitted(lm_temp)

residual_diagnostics(res_vect = res_vect,fit_vect = fit_vect)
# residualerna ser inte bra ut här!
# plotten residuals vs index (= tidsindex) visar oss om viktiga tidsberoenden 
# finns kvar i data, vilket det gör här. Vi ser ett tydligt periodiskt mönster.
# residualerna ser inte normalfördelade ut. 

# det är tydligt här att en modell med bara en linjär trend inte är lämpligt här

# plottar anpassade värden med tidserien:
plot(tempdub)
lines(tempdub_time,fit_vect,col="red",lwd=2)

# vi behöver en modell för säsonger!






#-------------------------------------------------------------------------------
# tempdub data: säsongsvariabler
#-------------------------------------------------------------------------------

# vi behöver dummy-variabler för månader, vi har två alt:
# 1) Skapa dummy-variabler manuellt och använda dessa i en designmatris
# 2) Skapa en kategorisk variabel med månadsnamn/nummer som vi sen kodar som 
# faktor innan använder lm()


# alt 1:

# lägg till modell för månader:
X_month<-matrix(0,nrow = nobs,ncol = 12)
colnames(X_month)<-tolower(month.abb)
head(X_month)
library(lubridate)
month_val<-month(tempdub_time) # tar ut månadsnummer
for(i in 1:12){
  X_month[,i]<-ifelse(month_val==i,1,0)
}
head(X_month,15) # notera vilka rader som har en 1

# ovan skapar vi en lämplig designmatris för månader "för hand",
# det går även att använda funktionen model.matrix()


# tar bort april -> baseline som vi jämför med -> vi har 11 dummy-variabler för månader
# vi lägger till den linjära trenden med (men troligen så behövs den inte)
X<-cbind(time=time_index,X_month[,-4])
head(X)
reg_data<-data.frame(y=as.vector(tempdub),X)
head(reg_data)
nobs/12  # 12 år
# vi skattar vår modell på de 11 första åren
# sen gör vi prediktion på det sista året

# träningsdata:
train_index<-1:132
reg_data_train<-reg_data[train_index,]
# testdata (för prognos)
test_index<-133:144
reg_data_test<-reg_data[test_index,]

lm_temp2<-lm(y~.,data=reg_data_train)

res_vect<-residuals(lm_temp2)
fit_vect<-fitted(lm_temp2)
residual_diagnostics(res_vect = res_vect,fit_vect = fit_vect)
# residualerna ser mycket bättre ut nu!

# kollar SAC:
acf(res_vect)
# verkar vara mycket lite autokorrelation i residualerna

# vi testar Durbin-Watson Test för att undersöka om det finns beroende i residualerna 
library(lmtest)
dwtest(formula = lm_temp2,alternative = "two.sided")
library(car)
durbinWatsonTest(model = lm_temp2)
# troligen inte så mycket autokorrelation

summary(lm_temp2)
round(coef(lm_temp2),3)
barplot(round(coef(lm_temp2),3)[-1])
# notera att april är vår jämförelsepunkt!
# hur tolkar vi dessa skattade regressionsparameterar?


#-------------------------------------------------------------------------------
# anpassade värden och prediktion

# anpassade värden på träningsdata:
plot(tempdub_time,tempdub,t="o",lwd=2,ylim=c(5,80))
lines(tempdub_time[train_index],fitted(lm_temp2),col="blue",lwd=1.5)

# beräknar prediktionsintervalll på testdata
# reg_data_test[,-1] = alla förklarande variabler för testdata (tagit bort y)
lm_temp2_pred<-predict(lm_temp2, newdata = reg_data_test[,-1], 
                       interval = 'prediction',level=0.95)
lm_temp2_pred

# test data:
plot(tempdub_time,tempdub,t="o",lwd=2,ylim=c(5,80))
# anpassade värden för träningsdata:
lines(tempdub_time[train_index],fitted(lm_temp2),col="blue",lwd=1.5)
# anpassade värden för testdata:
lines(tempdub_time[test_index],lm_temp2_pred[,1],col="red",lwd=1.5)
# undre prediktionsgräns
lines(tempdub_time[test_index],lm_temp2_pred[,2],col="red",lwd=1.5,lty="dashed")
# övre prediktionsgräns
lines(tempdub_time[test_index],lm_temp2_pred[,3],col="red",lwd=1.5,lty="dashed")


# gör om plotten ovan, men i ggplot2:

reg_data2<-reg_data
reg_data2$type<-"train"
reg_data2$type[test_index]<-"prediction"

# beräkna prediktionsintervall för alla obs:
# reg_data_test[,-1] = alla förklarande variabler för testdata (tagit bort y)
lm_temp2_pred_all<-predict(lm_temp2, newdata = reg_data[,-1], interval = 'prediction')
lm_temp2_pred_all

reg_data2$fitted<-lm_temp2_pred_all[,1]
reg_data2$pi_lwr<-lm_temp2_pred_all[,2]
reg_data2$pi_upr<-lm_temp2_pred_all[,3]
reg_data2$date<-tempdub_time

head(reg_data2,4)

# plotta: data, anpassade värden, prediktioner och prediktionsintervall:
ggplot(data = reg_data2,aes(x=date,y=y))+geom_line()+theme_bw()+
  geom_line(aes(y=fitted,col=type))+
  geom_ribbon(aes(ymin=pi_lwr,ymax=pi_upr,fill=type),alpha=0.4)


# Prediktion 
# http://www.r-tutor.com/elementary-statistics/simple-linear-regression/prediction-interval-linear-regression
# https://rpubs.com/aaronsc32/regression-confidence-prediction-intervals




#-------------------------------------------------------------------------------
# Regression och Autokorrelation i residualerna 
#-------------------------------------------------------------------------------

rm(list = ls())
source("https://raw.githubusercontent.com/STIMALiU/732G52_tsa/refs/heads/main/code/residual_diagnostics.R")
library(ggplot2)
library(cowplot)
nobs<-300
x<-(1:nobs)

#-------------------------------------------------------------------------------
# fall 1
sd_val<-4
set.seed(76)
# vi simulerar e (slumptermen) så att den inte är oberoende, från en AR(1)-modell
# mer om AR och ARIMA senare i kursen
# y_t = rho*y_(t-1) + error
set.seed(35)
e<-arima.sim(n = nobs,list(ar=0.9),rand.gen = rnorm,sd=1)
plot(x,e,t="l")

y1<-10 + 0.1*x+e
plot(x,y1,t="o")

lm_y1<-lm(formula = y1~x)
res_vect<-residuals(lm_y1)
fit_vect<-fitted(lm_y1)
residual_diagnostics(res_vect = res_vect,fit_vect = fit_vect)
# residualerna ser hyffsat normalfördelade ut, men vi ser både i 
# fitted och index vs residuals att det finns korrelation kvar i data.


library(lmtest)
dwtest(formula = lm_y1,alternative = "two.sided")
library(car)
durbinWatsonTest(model = lm_y1)
# testen visar på att det finns en autokorrelation!

# vi under söker feltermen närmare:
e_current<-e[2:300]
e_lag1<-e[1:299]
# vi plottar "nuvarande värdet" mot "föregående värde" (lag 1)
plot(e_lag1,e_current)
# Vi ser en tydlig positiv korrelation mellan nuvarande och föregående värde!
cor(e_lag1,e_current)

acf(e)
?stats::acf
acf(e,lag.max = 30) # autokorrelationen är normaliserad mellan -1 och 1.
acf(e,lag.max = 30,type = "cov") # anger vi "cov" så får vi autokovariansen 
# som inte är normaliserad, skalan beror på data
# men vi tolkar tidsberoenden på samma sätt


#-------------------------------------------------------------------------------
# Notera!
#-------------------------------------------------------------------------------
# när vi har tidsberoende i residualerna, och beräknar medelfelet för beta-hat
# på vanligt sätt, då blir det inte korrekt medelfel. Detta gör att inferensen  
# inte blir korrekt -> vi ska inte göra tester/konfidensintervall då
# mer om det senare i kursen.
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# fall 2
set.seed(36)
e<-arima.sim(n = nobs,list(ar= -0.9),rand.gen = rnorm,sd=1)

plot(x,e,t="l")

y1<-10 + 0.1*x+e
plot(x,y1,t="o")


lm_y1<-lm(formula = y1~x)
res_vect<-residuals(lm_y1)
fit_vect<-fitted(lm_y1)
residual_diagnostics(res_vect = res_vect,fit_vect = fit_vect)

library(lmtest)
dwtest(formula = lm_y1,alternative = "two.sided")
library(car)
durbinWatsonTest(model = lm_y1)

# vi under söker feltermen närmare:
e_current<-e[2:300]
e_lag1<-e[1:299]
# vi plottar "nuvarande värdet" mot "föregående värde" (lag 1)
plot(e_lag1,e_current)
# Vi ser en tydlig negativ korrelation mellan nuvarande och föregående värde!
cor(e_lag1,e_current)


acf(e)
#?acf
acf(e,lag.max = 30)
acf(e,lag.max = 30,type = "cov")



#-------------------------------------------------------------------------------
# Tidserieregression: AirPassengers
#-------------------------------------------------------------------------------

data("AirPassengers")

?AirPassengers
plot(AirPassengers)
plot(log(AirPassengers))
library(lubridate)
air_time<-as.Date(AirPassengers)



nobs<-length(air_time)

time_index<-1:nobs
# Regression med linjär trend:
lm_temp<-lm(AirPassengers~time_index)
summary(lm_temp)
library(ggplot2)
library(cowplot)
res_vect<-residuals(lm_temp)
fit_vect<-fitted(lm_temp)
residual_diagnostics(res_vect = res_vect,fit_vect = fit_vect)
# ser inte bra ut! Vi har ett tydligt säsongsmönster!

acf(res_vect,lag=30)
# ser inte bra ut! Vi vill ha oberoende residualer om möjligt


lm_temp_log<-lm(log(AirPassengers)~time_index)
summary(lm_temp_log)
res_vect<-residuals(lm_temp_log)
fit_vect<-fitted(lm_temp_log)
residual_diagnostics(res_vect = res_vect,fit_vect = fit_vect)
# ser bättre ut, men vi ser tydligt säsongsberoende i index vs residuals.
exp(coef(lm_temp_log))
plot(air_time,AirPassengers,t="o")
# tar exp() på anpassade värden för att få rätt skala till orginaldata
lines(air_time,exp(fitted(lm_temp_log)),col="red")
# vi har en ok anpassning för trenden, men vi missar effekten av månader!




#-------------------------------------------------------------------------------
# Tidserieregression: AirPassengers - säsongsmodellering
#-------------------------------------------------------------------------------


# lägg till modell för månader:
X_month<-matrix(0,nrow = nobs,ncol = 12)
colnames(X_month)<-tolower(month.abb)
month_val<-month(as.Date(AirPassengers))
for(i in 1:12){
  X_month[,i]<-ifelse(month_val==i,1,0)
}
head(X_month,15)

# designmatris (= en matris med alla förklarande variabler)
# ska ha en kolumn med bara ettor, men lm lägger till det automatiskt, så
# behöver inte tänka på det:

# tar bort april -> baseline
X<-cbind(time=time_index,X_month[,-4])
head(X)
reg_data<-data.frame(y=as.vector(AirPassengers),X)
head(reg_data)
nobs/12  # 12 år
# vi skattar vår modell på de 11 första åren
# sen gör vi prediktion på det sista året

# träningsdata:
train_index<-1:132
reg_data_train<-reg_data[train_index,]
# testdata (för prognos)
test_index<-133:144
reg_data_test<-reg_data[test_index,]

lm_temp2<-lm(log(y)~.,data=reg_data_train)
summary(lm_temp2)

res_vect<-residuals(lm_temp2)
fit_vect<-fitted(lm_temp2)
residual_diagnostics(res_vect = res_vect,fit_vect = fit_vect)
# verkar finnas beroende kvar i index vs res, men ser bättre ut än tidigare!

acf(res_vect,lag=30)
# ser inte bra ut! Vi vill ha oberoende residualer om möjligt

# Notera: om antaganden som krävs för test/konfidensintervall inte är uppfyllda,
# då ska vi inte göra test/konfidensintervall för tex beta-skattningarna.
# vi kan dock undersöka punktkattningar av parametrar och göra punktprognoser.

# senare i kursen ska vi lära oss metoder för att hantera situationer där
# det finns beroende i residualerna i tidserie regression.

library(lmtest)
dwtest(formula = lm_temp2,alternative = "two.sided")
library(car)
durbinWatsonTest(model = lm_temp2)
# vad säger testet?

summary(lm_temp2)
round(coef(lm_temp2),3)
barplot(coef(lm_temp2)[-1])
# hur tolkar vi beta-hat när vi har tagit log-transform på y?
barplot(exp(coef(lm_temp2)[-1]))


#-------------------------------------------------------------------------------
# anpassade värden och prediktion
# notera: eftersom residualerna inte är oberoende så bör vi vara försiktiga med
# att beräkna predikitonsintervall för prognoser etc. Nedan visas kod hur man kan 
# göra det. (likt fallet med tempdub-data)


# anpassade värden på träningsdata:
plot(air_time,AirPassengers,t="o",lwd=2,ylim=c(95,720))
lines(air_time[train_index],exp(fitted(lm_temp2)),col="blue",lwd=1.5)

# reg_data_test[,-1] = alla förklarande variabler för testdata (tagit bort y)
lm_temp2_pred<-predict(lm_temp2, newdata = reg_data_test[,-1], interval = 'prediction')
lm_temp2_pred

# test data:
plot(air_time,AirPassengers,t="o",lwd=2,ylim=c(95,720))

# anpassade värden för träningsdata:
# notera att tar exp() på anpassade värden och intervall gränserna nedan.
lines(air_time[train_index],exp(fitted(lm_temp2)),col="blue",lwd=1.5)
# anpassade värden för testdata:
lines(air_time[test_index],exp(lm_temp2_pred[,1]),col="red",lwd=1.5)
# undre prediktionsgräns
lines(air_time[test_index],exp(lm_temp2_pred[,2]),col="red",lwd=1.5,lty="dashed")
# övre prediktionsgräns
lines(air_time[test_index],exp(lm_temp2_pred[,3]),col="red",lwd=1.5,lty="dashed")



reg_data2<-reg_data
reg_data2$type<-"train"
reg_data2$type[test_index]<-"prediction"

# beräkna prediktionsintervall för alla obs:
# reg_data_test[,-1] = alla förklarande variabler för testdata (tagit bort y)
lm_temp2_pred_all<-predict(lm_temp2, newdata = reg_data[,-1], interval = 'prediction')
lm_temp2_pred_all

# notera att tar exp() på anpassade värden och intervall gränserna nedan.
reg_data2$fitted<-exp(lm_temp2_pred_all[,1])
reg_data2$pi_lwr<-exp(lm_temp2_pred_all[,2])
reg_data2$pi_upr<-exp(lm_temp2_pred_all[,3])
reg_data2$date<-air_time

head(reg_data2,4)

# plotta: data, anpassade värden, prediktioner och prediktionsintervall:
ggplot(data = reg_data2,aes(x=date,y=y))+geom_line()+theme_bw()+
  geom_line(aes(y=fitted,col=type))+
  geom_ribbon(aes(ymin=pi_lwr,ymax=pi_upr,fill=type),alpha=0.4)









