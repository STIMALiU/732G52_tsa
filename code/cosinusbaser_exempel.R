
# läser in funktion för cosinusbaser
source(
  "https://raw.githubusercontent.com/STIMALiU/732G57_ML/master/labs/cosine_basis.R"
)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# tittar på exempel på cosinusbaser
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

x<-seq(-3,5,length=300)
# order: högsta ordningen på cosinusbaserna 
cos_list<-cosine_basis(x = x,order = 6) 
dim(cos_list$basis_mat) 
par(mfrow=c(2,3)) 
for(i in 1:6){   
  plot(cos_list$x,cos_list$basis_mat[,i],t="l",xlab="x",ylab="basis function",ylim=c(-1,1),main=colnames(cos_list$basis_mat)[i])
}
par(mfrow=c(1,1)) 

# vad finns i listan?
str(cos_list)
head(cos_list$basis_mat)

dim(cos_list$basis_mat)

round(cor(cos_list$basis_mat),3)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# modellanpassning med cosinusbaser
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# skapar lite data
n <- 500
set.seed(424)
x <- sort(runif(n , min = 0, max = 5  ))
y <- 0.5 + 3*x - 2*x^2 + .2*x^3 + log(2*x+1) + 3*sin(pi * x) + rnorm(n = n, sd = 1)
df <- data.frame(x = x, y = y)
plot(df)
library(ggplot2)


# skapar cosinusbaser

cos_list<-cosine_basis(x = x,order = 20) 
str(cos_list)

# anpassar med lm()

# modell 1: 5 baser
df_temp1<-data.frame(y=y,cos_list$basis_mat[,1:5])
head(df_temp1)
lm_cos1<-lm(y~.,data=df_temp1)
plot(df)
lines(x,fitted(lm_cos1),col="blue",lwd=3)


# modell 2: 10 baser
df_temp2<-data.frame(y=y,cos_list$basis_mat[,1:10])
head(df_temp2)
lm_cos2<-lm(y~.,data=df_temp2)
plot(df)
lines(x,fitted(lm_cos2),col="blue",lwd=3)


# modell 3: 20 baser
df_temp3<-data.frame(y=y,cos_list$basis_mat)
head(df_temp3)
lm_cos3<-lm(y~.,data=df_temp3)
plot(df)
lines(x,fitted(lm_cos3),col="blue",lwd=3)


# modell 4: 20 baser + forward selection + BIC

# Kolla här: https://cran.r-project.org/web/packages/bigstep/vignettes/bigstep.html
library(bigstep)
prepare1 <- prepare_data(y, df_temp3[,-1]) # ta med designmatrisen här

results1 <- stepwise(prepare1, crit = bic)
results1
summary(results1)
?get_model

plot(df)
lines(x,fitted(get_model(results1)),col="blue",lwd=3)

