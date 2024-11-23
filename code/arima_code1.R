


#-------------------------------------------------------------------------------
# Skatta ARMA modeller
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Box-Jenkins
#-------------------------------------------------------------------------------

# base R

# skatta ARIMA:
?arima
# skatta AR:
?ar

# AR(1)
nobs<-300
y<-arima.sim(model = list(ar=0.8),n = nobs,rand.gen = rnorm,sd=0.1)
y<-arima.sim(model = list(ar=c(0.5,0.1,0.1)),n = nobs,rand.gen = rnorm,sd=0.1)

acf(y) # börjar på lag 0
pacf(y) # börjar på lag 1
ar_obj<-ar(x = y,aic = FALSE,order.max = 1)
#ar_obj<-ar(x = y,aic = TRUE,order.max = 10)
?ar
str(ar_obj)
ar_obj$order
ar_obj$ar
# brus varians
ar_obj$var.pred
#jämför med
0.1^2

# ## S3 method for class 'ar'
# predict(object, newdata, n.ahead = 1, se.fit = TRUE, ...)
predict(ar_obj,n.ahead = 5,se.fit = TRUE)

# residualer
hist(ar_obj$resid)
acf(x = na.omit(ar_obj$resid))
pacf(x = na.omit(ar_obj$resid))

# Simulera och skatta
# AR(1), MA(2), AR(2), MA(2)
# kör box jenkins metod

# vissa ARMA:
# ARMA(1,1), ARMA(2,2)

?arima()

set.seed(657)
y<-arima.sim(model = list(ar=c(0.5,0.1,0.1)),n = nobs,rand.gen = rnorm,sd=0.1)
acf(y) # börjar på lag 0
pacf(y) # börjar på lag 1
ar_obj2<-arima(x = y,order = c(2,0,0))
ar_obj2
str(ar_obj2)
pred_ar2<-predict(ar_obj2,n.ahead = 5)
as.vector(pred_ar2$pred)
coef(ar_obj2)


y3_para<-c(0.8,0.3) # MA(2)
y4_para<-c(-0.8,0.1,-0.5,-0.8) # ARMA(2,2)
set.seed(345)
y3<-arima.sim(model = list(ma=y3_para),n = nobs,rand.gen = rnorm,sd=0.1)
y4<-arima.sim(model = list(ar=y4_para[1:2],ma=y4_para[3:4]),n = nobs,rand.gen = rnorm,sd=0.1)

ar_obj2<-arima(x = y3,order = c(0,0,2))
ar_obj2<-arima(x = y4,order = c(2,0,2))
coef(ar_obj2)
ar_obj2
# ARIMA se fable





#-------------------------------------------------------------------------------
# Forecasting: Principles and Practice
# baserat på:
# https://github.com/robjhyndman/ETC3550Slides/blob/fable/9-arima.R 
#-------------------------------------------------------------------------------
rm(list=ls())

library(fpp3)

#### GOOGLE STOCK PRICE 2018 ----------------
?gafa_stock
google_2018 <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) == 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE)

google_2018 %>% autoplot(Close)
google_2018 %>% ACF(Close) %>% autoplot()

# difference = ger diff
google_2018 %>% autoplot(difference(Close)) +
  labs(y="Google closing stock price", x="Day")

google_2018 %>% ACF(difference(Close)) %>% autoplot()


### WWW usage
?WWWusage
wwwusage <- as_tsibble(WWWusage)
wwwusage %>% autoplot(value)
wwwusage %>% autoplot(difference(value))
wwwusage %>% autoplot(difference(value, differences=2))

## A10 drugs
?PBS
a10 <- PBS %>%
  filter(ATC2 == "A10") %>%
  summarise(Cost = sum(Cost)/1e6)

a10 %>% autoplot(Cost)

a10 %>% autoplot(log(Cost))

a10 %>% autoplot(
  log(Cost) %>% difference(12)
)


## H02 drugs
?PBS
h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost)/1e6)

h02 %>% autoplot(Cost)

h02 %>% autoplot(log(Cost))

h02 %>% autoplot(
  log(Cost) %>% difference(12)
)

h02 %>% autoplot(
  log(Cost) %>% difference(12) %>% difference(1)
)


## Australian tourism --------------------------
?tourism
total_trips <- tourism %>%
  summarise(Trips = sum(Trips))

total_trips %>% autoplot(Trips)

total_trips %>%
  autoplot(Trips %>% difference(4))

total_trips %>%
  autoplot(Trips %>% difference(4) %>% difference(1))


## EGYPTIAN EXPORTS

global_economy %>%
  filter(Code == "EGY") %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Egyptian Exports")

fit <- global_economy %>% 
  filter(Code == "EGY") %>%
  model(ARIMA(Exports))
report(fit)

gg_tsresiduals(fit)

augment(fit) %>%
  features(.resid, ljung_box, lag = 10, dof = 4)

fit %>% 
  forecast(h=10) %>%
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian Exports")

global_economy %>% filter(Code == "EGY") %>% ACF(Exports) %>% autoplot()
global_economy %>% filter(Code == "EGY") %>% PACF(Exports) %>% autoplot()

global_economy %>% 
  filter(Code == "EGY") %>%
  gg_tsdisplay(Exports, plot_type='partial')

fit1 <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports ~ pdq(4,0,0)))
report(fit1)

fit2 <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports))
report(fit2)

## CAF EXPORTS

global_economy %>%
  filter(Code == "CAF") %>%
  autoplot(Exports) +
  labs(title="Central African Republic exports",
       y="% of GDP")

global_economy %>%
  filter(Code == "CAF") %>%
  gg_tsdisplay(difference(Exports), plot_type='partial')

caf_fit <- global_economy %>%
  filter(Code == "CAF") %>%
  model(arima210 = ARIMA(Exports ~ pdq(2,1,0)),
        arima013 = ARIMA(Exports ~ pdq(0,1,3)),
        stepwise = ARIMA(Exports),
        search = ARIMA(Exports, stepwise=FALSE))

caf_fit

glance(caf_fit) %>% arrange(AICc) %>% select(.model:BIC)

caf_fit %>%
  select(search) %>%
  gg_tsresiduals()

caf_fit %>%
  select(search) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 10, dof = 3)

caf_fit %>%
  forecast(h=50) %>%
  filter(.model=='search') %>%
  autoplot(global_economy)


## MINKS --------------------------------------------------------------------------

mink <- as_tsibble(fma::mink)
mink %>% autoplot(value) +
  labs(y="Minks trapped (thousands)",
       title = "Annual number of minks trapped")

mink %>% ACF(value) %>% autoplot()
mink %>% PACF(value) %>% autoplot()

fit <- mink %>%
  model(
    ar4 = ARIMA(value ~ pdq(4,0,0)),
    auto = ARIMA(value),
    best = ARIMA(value, stepwise=FALSE, approximation=FALSE)
  )

glance(fit)

fit %>% select(best) %>% report()

fit %>% select(best) %>% gg_tsresiduals()

fit %>% select(best) %>% forecast(h=20) %>% autoplot(mink)

## WWW usage -----------------------------------------------------------------------

web_usage <- as_tsibble(WWWusage)
web_usage %>% gg_tsdisplay(value, plot_type = 'partial')

web_usage %>% gg_tsdisplay(difference(value), plot_type = 'partial')

fit <- web_usage %>%
  model(arima = ARIMA(value ~ pdq(3, 1, 0))) %>%
  report()

web_usage %>%
  model(auto = ARIMA(value ~ pdq(d=1))) %>%
  report()

web_usage %>%
  model(auto2 = ARIMA(value ~ pdq(d=1),
                      stepwise = FALSE, approximation = FALSE)) %>%
  report()

gg_tsresiduals(fit)

augment(fit) %>%
  features(.resid, ljung_box, lag = 10, dof = 3)

fit %>% forecast(h = 10) %>% autoplot(web_usage)


## GDP --------------------------------------------------------------------------

global_economy %>%
  filter(Country=="United States") %>%
  autoplot(log(GDP))

fit <- global_economy %>%
  model(
    ARIMA(log(GDP))
  )

fit

fit %>%
  filter(Country == "Australia") %>%
  report()
fit %>%
  filter(Country == "Australia") %>%
  gg_tsresiduals()
fit %>%
  filter(Country == "Australia") %>%
  augment() %>%
  features(.resid, ljung_box, dof=2, lag=15)
fit %>%
  filter(Country == "Australia") %>%
  forecast(h=10) %>%
  autoplot(global_economy) +
  scale_y_log10()









## h02 drugs ----------------------------------------------------------------------

h02 <- tsibbledata::PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost))

h02 %>% autoplot(Cost)

## Models using logs

h02 %>% autoplot(log(Cost))
h02 %>% gg_tsdisplay(difference(log(Cost),12), lag_max = 36, plot_type='partial')

# My best guess
fit <- h02 %>%
  model(best = ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2)))
report(fit)
gg_tsresiduals(fit, lag_max=36)
augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 6)

# Letting R choose
fit <- h02 %>% model(auto = ARIMA(log(Cost), stepwise = FALSE))
report(fit)
gg_tsresiduals(fit, lag_max=36)
augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 6)

# Letting R work hard to choose
fit <- h02 %>%
  model(best = ARIMA(log(Cost), stepwise = FALSE,
                     approximation = FALSE,
                     order_constraint = p + q + P + Q <= 9))
report(fit)
gg_tsresiduals(fit, lag_max=36)
augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 9)

# The forecasts
fit %>% forecast %>% autoplot(h02) +
  labs(y="H02 Expenditure ($AUD)")


## Models without using logs

h02 %>% gg_tsdisplay(difference(Cost,12), lag_max = 36, plot_type='partial')

# My best guess
fit <- h02 %>%
  model(best = ARIMA(Cost ~ 0 + pdq(3,0,1) + PDQ(0,1,2)))
report(fit)
gg_tsresiduals(fit, lag_max=36)
augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 6)

# Letting R choose
fit <- h02 %>% model(auto = ARIMA(Cost, stepwise = FALSE))
report(fit)
gg_tsresiduals(fit, lag_max=36)
augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 7)

# Letting R work hard to choose
fit <- h02 %>%
  model(best = ARIMA(Cost, stepwise = FALSE,
                     approximation = FALSE,
                     order_constraint = p + q + P + Q <= 9))
report(fit)
gg_tsresiduals(fit, lag_max=36)
augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 8)

# The forecasts
fit %>% forecast %>% autoplot(h02) +
  labs(y="H02 Expenditure ($AUD)")




