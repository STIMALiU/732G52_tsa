#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Tidserieregression
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


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
# tempdub data
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

res_vect<-residuals(lm_temp)
fit_vect<-fitted(lm_temp)

residual_diagnostics(res_vect = res_vect,fit_vect = fit_vect)
# residualerna ser inte bra ut här!
# plotten residuals vs index (= tidsindex) visar oss om viktiga tidsberoenden 
# finns kvar i data, vilket det gör här. Vi ser ett tydligt periodiskt mönster.
# residualerna ser inte normalfördelade ut. 

# det är tydligt här att en modell med bara en linjär trend inte är lämpligt här






