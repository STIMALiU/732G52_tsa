#-------------------------------------------------------------------------------
rm(list=ls())


# se även https://otexts.com/fpp3/training-test.html
library(fpp3)

#### GOOGLE STOCK PRICE 2018 ----------------
?gafa_stock
google_2018 <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) == 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE)

# dela upp i träning och test
training <- google_2018 %>% slice_head(n = 180)
test <- google_2018 %>% slice_tail(n = 71)

google_2018 %>% autoplot(Close)
training %>% autoplot(Close)
training %>% ACF(Close) %>% autoplot()

# vi behöver en diff:
training %>% autoplot(difference(Close)) +
  labs(y="Google closing stock price", x="Day")

#training %>% ACF(difference(Close)) %>% autoplot()
#training %>% PACF(difference(Close)) %>% autoplot()

training %>%
  gg_tsdisplay(difference(Close, 1),
               plot_type='partial', lag=36)


training_fit <- training %>%model(arima1 = ARIMA(Close,stepwise = FALSE,approximation=FALSE))
# alt modell:
#training_fit <- training %>%model(arima1 = ARIMA(Close~0+pdq(1,1,0)))

training_fit%>%report()

test_fit<-training_fit %>%
  refit(test)# 


# test error for one-step ahead predictions 
test_fit%>%accuracy()
# https://otexts.com/fpp3/training-test.html




#-------------------------------------------------------------------------------
# code for multi-step ahead predictions
#-------------------------------------------------------------------------------
H<-15 # max antal steg för prognoshorisonten

# one-step ahead predictions 
fit_predict<-fitted(test_fit, h = 1)
i<-2
res_mat<-matrix(0,nrow = nrow(fit_predict),ncol = H-1)
colnames(res_mat)<-paste0("h_",2:H)
for(i in 1:(H-1)){
  # multi-step ahead predictions:
  fit_predict_temp<-fitted(test_fit, h = i+1)
  res_mat[,i]<-fit_predict_temp$.fitted
}

pred_all_H<-cbind(fit_predict[,4],as.data.frame(res_mat))
colnames(pred_all_H)[1]<-"h_1"
#head(pred_all_H,20)

mse_h_vect<-rep(0,H)
mae_h_vect<-rep(0,H)
for(i in 1:H){
  mse_h_vect[i]<-mean((test$Close-pred_all_H[,i])^2,na.rm=TRUE)
  mae_h_vect[i]<-mean(abs(test$Close-pred_all_H[,i]),na.rm=TRUE)
}
par(mfrow=c(2,1))
plot(sqrt(mse_h_vect),t="o",xlab="Forecast horizon",ylab="test RMSE",ylim=c(10,70))
plot(mae_h_vect,t="o",xlab="Forecast horizon",ylab="test MAE",ylim=c(10,70))
par(mfrow=c(1,1))

