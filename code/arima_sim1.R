#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Simulera ARMA modeller
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Nedan finns kod för att simulera olika ARMA modeller och för att undersöka hur
# deras teoretiska autokorrelation och teoretiska partiella autokorrelation ser ut
#
# Tanken är att ni ska jämföra:
# - den teoretiska autokorrelation mot den empiriska autokorrelation
# - den teoretiska partiella autokorrelation mot den empiriska partiella autokorrelation
# för olika ARMA-modeller
#-------------------------------------------------------------------------------







#-------------------------------------------------------------------------------
# Simulera ARMA modeller
# ARIMA(p,0,q)
#-------------------------------------------------------------------------------

?arima.sim 
# See arima for the precise definition of an ARIMA model.
?arima # Kan skatta ARIMA modeller, alt till paketet fable 
# Notera:
# från dok:
# Different definitions of ARMA models have different signs for the AR and/or MA
# coefficients. The definition used here has
# X[t] = a[1]X[t-1] + … + a[p]X[t-p] + e[t] + b[1]e[t-1] + … + b[q]e[t-q]

# notera "+ b[1]"




library(fpp3)
library(cowplot)

# nedan så kommer ni att simulera från olika ARMA-modeller för att sedan studera
# deras egenskaper (dvs olika typer av autokorrelationer)


#-------------------------------------------------------------------------------
# AR(1)
nobs<-200 # antal obs i simulerad tidsserie
# vi testar 4 olika AR(1)-modeller, vi kallar AR-parametern för phi:
# phi = 0.5. phi = 0.9, phi = -0.5, phi = -0.5
ar1_phi<-c(0.5,0.9,-0.5,-0.9) 

set.seed(83564) # vi sätter en seed, för att kunna återskapa resultaten
# Simulerar från phi = 0.5:
y1<-arima.sim(model = list(ar=ar1_phi[1]),n = nobs,rand.gen = rnorm,sd=0.1)
# Simulerar från phi = 0.9:
y2<-arima.sim(model = list(ar=ar1_phi[2]),n = nobs,rand.gen = rnorm,sd=0.1)
# Simulerar från phi = -0.5:
y3<-arima.sim(model = list(ar=ar1_phi[3]),n = nobs,rand.gen = rnorm,sd=0.1)
# Simulerar från phi = -0.9:
y4<-arima.sim(model = list(ar=ar1_phi[4]),n = nobs,rand.gen = rnorm,sd=0.1)
# skapar tidsindex
time_vect<-1:nobs

# skapar 4 delplottar med all simulerad data:
main_ar1<-paste("phi =",ar1_phi)
p1<-qplot(x=time_vect,y = as.vector(y1),geom = "line",ylab="",xlab="time",main=main_ar1[1])+theme_bw()
p2<-qplot(x=time_vect,y = as.vector(y2),geom = "line",ylab="",xlab="time",main=main_ar1[2])+theme_bw()
p3<-qplot(x=time_vect,y = as.vector(y3),geom = "line",ylab="",xlab="time",main=main_ar1[3])+theme_bw()
p4<-qplot(x=time_vect,y = as.vector(y4),geom = "line",ylab="",xlab="time",main=main_ar1[4])+theme_bw()
# plottar alla serierna tillsammans:
plot_grid(p1,p2,p3,p4,nrow=2)

# testa att ändra seed i set.seed() ovan, och skapa de 4 serierna på nytt och
# plotta dessa. Ni kommer få olika serier för varje seed, men försök att notera
# det tidsberoende mönstret för olika värden på phi

# teoretisk autokorrelation för ARMA
?ARMAacf
# teoretisk autokorrelation för AR:
max_lag<-40
acf_lag<-0:max_lag
pacf_lag<-1:max_lag
ARMAacf(ar = ar1_phi[1],lag.max = max_lag)
acf_mat<-matrix(0,nrow = max_lag+1,ncol = 4)
pacf_mat<-matrix(0,nrow = max_lag,ncol = 4)
for(i in 1:4){
  acf_mat[,i]<-ARMAacf(ar = ar1_phi[i],lag.max = max_lag,pacf = FALSE)
  pacf_mat[,i]<-ARMAacf(ar = ar1_phi[i],lag.max = max_lag,pacf = TRUE)
}

acf_main_ar1<-paste("TAC, ",main_ar1)
pacf_main_ar1<-paste("TPAC, ",main_ar1)

sacf_main_ar1<-paste("SAC, ",main_ar1)
spacf_main_ar1<-paste("SPAC, ",main_ar1)


p1_acf<-qplot(x=acf_lag,y = acf_mat[,1],geom = "line",ylab="",xlab="time",
              main=acf_main_ar1[1],ylim=c(-1,1),color=I('blue'))+geom_point()
p2_acf<-qplot(x=acf_lag,y = acf_mat[,2],geom = "line",ylab="",xlab="time",
              main=acf_main_ar1[2],ylim=c(-1,1),color=I('blue'))+geom_point()
p3_acf<-qplot(x=acf_lag,y = acf_mat[,3],geom = "line",ylab="",xlab="time",
              main=acf_main_ar1[3],ylim=c(-1,1),color=I('blue'))+geom_point()
p4_acf<-qplot(x=acf_lag,y = acf_mat[,4],geom = "line",ylab="",xlab="time",
              main=acf_main_ar1[4],ylim=c(-1,1),color=I('blue'))+geom_point()

p1_pacf<-qplot(x=pacf_lag,y = pacf_mat[,1],geom = "line",ylab="",xlab="time",
               main=pacf_main_ar1[1],ylim=c(-1,1),color=I('blue'))+geom_point()
p2_pacf<-qplot(x=pacf_lag,y = pacf_mat[,2],geom = "line",ylab="",xlab="time",
               main=pacf_main_ar1[2],ylim=c(-1,1),color=I('blue'))+geom_point()
p3_pacf<-qplot(x=pacf_lag,y = pacf_mat[,3],geom = "line",ylab="",xlab="time",
               main=pacf_main_ar1[3],ylim=c(-1,1),color=I('blue'))+geom_point()
p4_pacf<-qplot(x=pacf_lag,y = pacf_mat[,4],geom = "line",ylab="",xlab="time",
               main=pacf_main_ar1[4],ylim=c(-1,1),color=I('blue'))+geom_point()


# sample (empirisk) autokorrelation för AR:

sacf_mat<-matrix(0,nrow = max_lag+1,ncol = 4)
spacf_mat<-matrix(0,nrow = max_lag,ncol = 4)
y_mat<-cbind(y1,y2,y3,y4)
for(i in 1:4){
  sacf_mat[,i]<-acf(x = y_mat[,i],lag.max = max_lag,plot = FALSE)$acf[,,1]
  spacf_mat[,i]<-pacf(x= y_mat[,i],lag.max = max_lag,plot = FALSE)$acf[,,1]
}

p1_sacf<-qplot(x=acf_lag,y = sacf_mat[,1],geom = "line",ylab="",xlab="time",
               main=sacf_main_ar1[1],ylim=c(-1,1),color=I('blue'))+geom_point()
p2_sacf<-qplot(x=acf_lag,y = sacf_mat[,2],geom = "line",ylab="",xlab="time",
               main=sacf_main_ar1[2],ylim=c(-1,1),color=I('blue'))+geom_point()
p3_sacf<-qplot(x=acf_lag,y = sacf_mat[,3],geom = "line",ylab="",xlab="time",
               main=sacf_main_ar1[3],ylim=c(-1,1),color=I('blue'))+geom_point()
p4_sacf<-qplot(x=acf_lag,y = sacf_mat[,4],geom = "line",ylab="",xlab="time",
               main=sacf_main_ar1[4],ylim=c(-1,1),color=I('blue'))+geom_point()

p1_spacf<-qplot(x=pacf_lag,y = spacf_mat[,1],geom = "line",ylab="",xlab="time",
                main=spacf_main_ar1[1],ylim=c(-1,1),color=I('blue'))+geom_point()
p2_spacf<-qplot(x=pacf_lag,y = spacf_mat[,2],geom = "line",ylab="",xlab="time",
                main=spacf_main_ar1[2],ylim=c(-1,1),color=I('blue'))+geom_point()
p3_spacf<-qplot(x=pacf_lag,y = spacf_mat[,3],geom = "line",ylab="",xlab="time",
                main=spacf_main_ar1[3],ylim=c(-1,1),color=I('blue'))+geom_point()
p4_spacf<-qplot(x=pacf_lag,y = spacf_mat[,4],geom = "line",ylab="",xlab="time",
                main=spacf_main_ar1[4],ylim=c(-1,1),color=I('blue'))+geom_point()

plot_grid(p1,p2,p3,p4,nrow=2)

plot_grid(p1_acf,p1_pacf,p1_sacf,p1_spacf)
plot_grid(p2_acf,p2_pacf,p2_sacf,p2_spacf)
plot_grid(p3_acf,p3_pacf,p3_sacf,p3_spacf)
plot_grid(p4_acf,p4_pacf,p4_sacf,p4_spacf)







#-------------------------------------------------------------------------------
# MA(1)
rm(list = ls())
nobs<-200
set.seed(864)
ma1_theta<-c(0.5,0.9,-0.5,-0.9)
y1<-arima.sim(model = list(ma=ma1_theta[1]),n = nobs,rand.gen = rnorm,sd=0.1)
y2<-arima.sim(model = list(ma=ma1_theta[2]),n = nobs,rand.gen = rnorm,sd=0.1)
y3<-arima.sim(model = list(ma=ma1_theta[3]),n = nobs,rand.gen = rnorm,sd=0.1)
y4<-arima.sim(model = list(ma=ma1_theta[4]),n = nobs,rand.gen = rnorm,sd=0.1)
time_vect<-1:nobs

main_ma1<-paste("theta =",ma1_theta)
p1<-qplot(x=time_vect,y = as.vector(y1),geom = "line",ylab="",xlab="time",main=main_ma1[1])+theme_bw()
p2<-qplot(x=time_vect,y = as.vector(y2),geom = "line",ylab="",xlab="time",main=main_ma1[2])+theme_bw()
p3<-qplot(x=time_vect,y = as.vector(y3),geom = "line",ylab="",xlab="time",main=main_ma1[3])+theme_bw()
p4<-qplot(x=time_vect,y = as.vector(y4),geom = "line",ylab="",xlab="time",main=main_ma1[4])+theme_bw()

plot_grid(p1,p2,p3,p4,nrow=2)




# teoretisk autokorrelation för AR:
max_lag<-40
acf_lag<-0:max_lag
pacf_lag<-1:max_lag
ARMAacf(ma = ma1_theta[1],lag.max = max_lag)
acf_mat<-matrix(0,nrow = max_lag+1,ncol = 4)
pacf_mat<-matrix(0,nrow = max_lag,ncol = 4)
for(i in 1:4){
  acf_mat[,i]<-ARMAacf(ma = ma1_theta[i],lag.max = max_lag,pacf = FALSE)
  pacf_mat[,i]<-ARMAacf(ma = ma1_theta[i],lag.max = max_lag,pacf = TRUE)
}

acf_main_ma1<-paste("TAC, ",main_ma1)
pacf_main_ma1<-paste("TPAC, ",main_ma1)

sacf_main_ma1<-paste("SAC, ",main_ma1)
spacf_main_ma1<-paste("SPAC, ",main_ma1)


p1_acf<-qplot(x=acf_lag,y = acf_mat[,1],geom = "line",ylab="",xlab="time",
              main=acf_main_ma1[1],ylim=c(-1,1),color=I('blue'))+geom_point()
p2_acf<-qplot(x=acf_lag,y = acf_mat[,2],geom = "line",ylab="",xlab="time",
              main=acf_main_ma1[2],ylim=c(-1,1),color=I('blue'))+geom_point()
p3_acf<-qplot(x=acf_lag,y = acf_mat[,3],geom = "line",ylab="",xlab="time",
              main=acf_main_ma1[3],ylim=c(-1,1),color=I('blue'))+geom_point()
p4_acf<-qplot(x=acf_lag,y = acf_mat[,4],geom = "line",ylab="",xlab="time",
              main=acf_main_ma1[4],ylim=c(-1,1),color=I('blue'))+geom_point()

p1_pacf<-qplot(x=pacf_lag,y = pacf_mat[,1],geom = "line",ylab="",xlab="time",
               main=pacf_main_ma1[1],ylim=c(-1,1),color=I('blue'))+geom_point()
p2_pacf<-qplot(x=pacf_lag,y = pacf_mat[,2],geom = "line",ylab="",xlab="time",
               main=pacf_main_ma1[2],ylim=c(-1,1),color=I('blue'))+geom_point()
p3_pacf<-qplot(x=pacf_lag,y = pacf_mat[,3],geom = "line",ylab="",xlab="time",
               main=pacf_main_ma1[3],ylim=c(-1,1),color=I('blue'))+geom_point()
p4_pacf<-qplot(x=pacf_lag,y = pacf_mat[,4],geom = "line",ylab="",xlab="time",
               main=pacf_main_ma1[4],ylim=c(-1,1),color=I('blue'))+geom_point()


# sample (empirisk) autokorrelation för AR:

sacf_mat<-matrix(0,nrow = max_lag+1,ncol = 4)
spacf_mat<-matrix(0,nrow = max_lag,ncol = 4)
y_mat<-cbind(y1,y2,y3,y4)
for(i in 1:4){
  sacf_mat[,i]<-acf(x = y_mat[,i],lag.max = max_lag,plot = FALSE)$acf[,,1]
  spacf_mat[,i]<-pacf(x= y_mat[,i],lag.max = max_lag,plot = FALSE)$acf[,,1]
}

p1_sacf<-qplot(x=acf_lag,y = sacf_mat[,1],geom = "line",ylab="",xlab="time",
               main=sacf_main_ma1[1],ylim=c(-1,1),color=I('blue'))+geom_point()
p2_sacf<-qplot(x=acf_lag,y = sacf_mat[,2],geom = "line",ylab="",xlab="time",
               main=sacf_main_ma1[2],ylim=c(-1,1),color=I('blue'))+geom_point()
p3_sacf<-qplot(x=acf_lag,y = sacf_mat[,3],geom = "line",ylab="",xlab="time",
               main=sacf_main_ma1[3],ylim=c(-1,1),color=I('blue'))+geom_point()
p4_sacf<-qplot(x=acf_lag,y = sacf_mat[,4],geom = "line",ylab="",xlab="time",
               main=sacf_main_ma1[4],ylim=c(-1,1),color=I('blue'))+geom_point()

p1_spacf<-qplot(x=pacf_lag,y = spacf_mat[,1],geom = "line",ylab="",xlab="time",
                main=spacf_main_ma1[1],ylim=c(-1,1),color=I('blue'))+geom_point()
p2_spacf<-qplot(x=pacf_lag,y = spacf_mat[,2],geom = "line",ylab="",xlab="time",
                main=spacf_main_ma1[2],ylim=c(-1,1),color=I('blue'))+geom_point()
p3_spacf<-qplot(x=pacf_lag,y = spacf_mat[,3],geom = "line",ylab="",xlab="time",
                main=spacf_main_ma1[3],ylim=c(-1,1),color=I('blue'))+geom_point()
p4_spacf<-qplot(x=pacf_lag,y = spacf_mat[,4],geom = "line",ylab="",xlab="time",
                main=spacf_main_ma1[4],ylim=c(-1,1),color=I('blue'))+geom_point()

plot_grid(p1,p2,p3,p4,nrow=2)
plot_grid(p1_acf,p1_pacf,p1_sacf,p1_spacf)
plot_grid(p2_acf,p2_pacf,p2_sacf,p2_spacf)
plot_grid(p3_acf,p3_pacf,p3_sacf,p3_spacf)
plot_grid(p4_acf,p4_pacf,p4_sacf,p4_spacf)








#-------------------------------------------------------------------------------
# olika ARMA
rm(list = ls())
nobs<-200
set.seed(86334)
arma_phi<-c(0.5,0.9,-0.5,-0.9)
arma_theta<-c(0.8,0.9,-0.5,-0.9)
y1_para<-c(0.5,0.8) # ARMA(1,1)
y2_para<-c(1.5,-0.75) # AR(2)
y3_para<-c(0.8,0.3) # MA(2)
y4_para<-c(-0.8,-0.1,-0.5,-0.8) # ARMA(2,2)

set.seed(220)
y1<-arima.sim(model = list(ar=y1_para[1],ma=y1_para[2]),n = nobs,rand.gen = rnorm,sd=0.1)
y2<-arima.sim(model = list(ar=y2_para),n = nobs,rand.gen = rnorm,sd=0.1)
y3<-arima.sim(model = list(ma=y3_para),n = nobs,rand.gen = rnorm,sd=0.1)
y4<-arima.sim(model = list(ar=y4_para[1:2],ma=y4_para[3:4]),n = nobs,rand.gen = rnorm,sd=0.1)
time_vect<-1:nobs

main_ma1<-paste("theta =",ma1_theta)
main_arma<-c("ARMA(1,1)","AR(2)","MA(2)","ARMA(2,2)")

p1<-qplot(x=time_vect,y = as.vector(y1),geom = "line",ylab="",xlab="time",main=main_arma[1])+theme_bw()
p2<-qplot(x=time_vect,y = as.vector(y2),geom = "line",ylab="",xlab="time",main=main_arma[2])+theme_bw()
p3<-qplot(x=time_vect,y = as.vector(y3),geom = "line",ylab="",xlab="time",main=main_arma[3])+theme_bw()
p4<-qplot(x=time_vect,y = as.vector(y4),geom = "line",ylab="",xlab="time",main=main_arma[4])+theme_bw()

plot_grid(p1,p2,p3,p4,nrow=2)




# teoretisk autokorrelation för AR:
max_lag<-40
acf_lag<-0:max_lag
pacf_lag<-1:max_lag
ARMAacf(ma = ma1_theta[1],lag.max = max_lag)
acf_mat<-matrix(0,nrow = max_lag+1,ncol = 4)
pacf_mat<-matrix(0,nrow = max_lag,ncol = 4)

# ARMA(1,1)
acf_mat[,1]<-ARMAacf(ar=y1_para[1],ma=y1_para[2],lag.max = max_lag,pacf = FALSE)
pacf_mat[,1]<-ARMAacf(ar=y1_para[1],ma=y1_para[2],lag.max = max_lag,pacf = TRUE)
# AR(2)
acf_mat[,2]<-ARMAacf(ar=y2_para,lag.max = max_lag,pacf = FALSE)
pacf_mat[,2]<-ARMAacf(ar=y2_para,lag.max = max_lag,pacf = TRUE)
# MA(2)
acf_mat[,3]<-ARMAacf(ma=y3_para,lag.max = max_lag,pacf = FALSE)
pacf_mat[,3]<-ARMAacf(ma=y3_para,lag.max = max_lag,pacf = TRUE)
# ARMA(2,2)
acf_mat[,4]<-ARMAacf(ar=y4_para[1:2],ma=y4_para[3:4],lag.max = max_lag,pacf = FALSE)
pacf_mat[,4]<-ARMAacf(ar=y4_para[1:2],ma=y4_para[3:4],lag.max = max_lag,pacf = TRUE)


acf_main_ma1<-paste("TAC, ",main_arma)
pacf_main_ma1<-paste("TPAC, ",main_arma)

sacf_main_ma1<-paste("SAC, ",main_arma)
spacf_main_ma1<-paste("SPAC, ",main_arma)


p1_acf<-qplot(x=acf_lag,y = acf_mat[,1],geom = "line",ylab="",xlab="time",
              main=acf_main_ma1[1],ylim=c(-1,1),color=I('blue'))+geom_point()
p2_acf<-qplot(x=acf_lag,y = acf_mat[,2],geom = "line",ylab="",xlab="time",
              main=acf_main_ma1[2],ylim=c(-1,1),color=I('blue'))+geom_point()
p3_acf<-qplot(x=acf_lag,y = acf_mat[,3],geom = "line",ylab="",xlab="time",
              main=acf_main_ma1[3],ylim=c(-1,1),color=I('blue'))+geom_point()
p4_acf<-qplot(x=acf_lag,y = acf_mat[,4],geom = "line",ylab="",xlab="time",
              main=acf_main_ma1[4],ylim=c(-1,1),color=I('blue'))+geom_point()

p1_pacf<-qplot(x=pacf_lag,y = pacf_mat[,1],geom = "line",ylab="",xlab="time",
               main=pacf_main_ma1[1],ylim=c(-1,1),color=I('blue'))+geom_point()
p2_pacf<-qplot(x=pacf_lag,y = pacf_mat[,2],geom = "line",ylab="",xlab="time",
               main=pacf_main_ma1[2],ylim=c(-1,1),color=I('blue'))+geom_point()
p3_pacf<-qplot(x=pacf_lag,y = pacf_mat[,3],geom = "line",ylab="",xlab="time",
               main=pacf_main_ma1[3],ylim=c(-1,1),color=I('blue'))+geom_point()
p4_pacf<-qplot(x=pacf_lag,y = pacf_mat[,4],geom = "line",ylab="",xlab="time",
               main=pacf_main_ma1[4],ylim=c(-1,1),color=I('blue'))+geom_point()


# sample (empirisk) autokorrelation för AR:

sacf_mat<-matrix(0,nrow = max_lag+1,ncol = 4)
spacf_mat<-matrix(0,nrow = max_lag,ncol = 4)
y_mat<-cbind(y1,y2,y3,y4)
for(i in 1:4){
  sacf_mat[,i]<-acf(x = y_mat[,i],lag.max = max_lag,plot = FALSE)$acf[,,1]
  spacf_mat[,i]<-pacf(x= y_mat[,i],lag.max = max_lag,plot = FALSE)$acf[,,1]
}

p1_sacf<-qplot(x=acf_lag,y = sacf_mat[,1],geom = "line",ylab="",xlab="time",
               main=sacf_main_ma1[1],ylim=c(-1,1),color=I('blue'))+geom_point()
p2_sacf<-qplot(x=acf_lag,y = sacf_mat[,2],geom = "line",ylab="",xlab="time",
               main=sacf_main_ma1[2],ylim=c(-1,1),color=I('blue'))+geom_point()
p3_sacf<-qplot(x=acf_lag,y = sacf_mat[,3],geom = "line",ylab="",xlab="time",
               main=sacf_main_ma1[3],ylim=c(-1,1),color=I('blue'))+geom_point()
p4_sacf<-qplot(x=acf_lag,y = sacf_mat[,4],geom = "line",ylab="",xlab="time",
               main=sacf_main_ma1[4],ylim=c(-1,1),color=I('blue'))+geom_point()

p1_spacf<-qplot(x=pacf_lag,y = spacf_mat[,1],geom = "line",ylab="",xlab="time",
                main=spacf_main_ma1[1],ylim=c(-1,1),color=I('blue'))+geom_point()
p2_spacf<-qplot(x=pacf_lag,y = spacf_mat[,2],geom = "line",ylab="",xlab="time",
                main=spacf_main_ma1[2],ylim=c(-1,1),color=I('blue'))+geom_point()
p3_spacf<-qplot(x=pacf_lag,y = spacf_mat[,3],geom = "line",ylab="",xlab="time",
                main=spacf_main_ma1[3],ylim=c(-1,1),color=I('blue'))+geom_point()
p4_spacf<-qplot(x=pacf_lag,y = spacf_mat[,4],geom = "line",ylab="",xlab="time",
                main=spacf_main_ma1[4],ylim=c(-1,1),color=I('blue'))+geom_point()

plot_grid(p1,p2,p3,p4,nrow=2)
plot_grid(p1_acf,p1_pacf,p1_sacf,p1_spacf)
plot_grid(p2_acf,p2_pacf,p2_sacf,p2_spacf)
plot_grid(p3_acf,p3_pacf,p3_sacf,p3_spacf)
plot_grid(p4_acf,p4_pacf,p4_sacf,p4_spacf)






#-------------------------------------------------------------------------------
# inversen av diff?
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

# inversen av diff:
# xi = första värdet på orginal tidserien
y2<-diffinv(x = z,xi = y[1])
all(y2==y)
# notera att differensen är skillnaden mellan två på varandra följande värden
#-------------------------------------------------------------------------------



# Att göra prognoser med diffade tidserier:
# se 9.8 Forecasting i Hyndman.





 




