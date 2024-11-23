

#-------------------------------------------------------------------------------
# based on:
# https://github.com/robjhyndman/ETC3550Slides/blob/fable/9-arima_c.R
#-------------------------------------------------------------------------------


rm(list=ls())

library(fpp3)

## WWW usage -------------------------------------------------------------------


?WWWusage

web_usage <- as_tsibble(WWWusage)

web_usage %>% gg_tsdisplay(value, plot_type = 'partial')
web_usage %>% gg_tsdisplay(difference(value), plot_type = 'partial')

# välja modell baserat på acf och pcf:
fit <- web_usage %>%
  model(arima = ARIMA(value ~ pdq(3, 1, 0))) 
fit %>%
  report()

gg_tsresiduals(fit)
# https://en.wikipedia.org/wiki/Ljung-Box_test
# degrees of freedom (dof) = (p+q)

augment(fit) %>%
  features(.resid, ljung_box, lag = 10, dof = 3)


# automatiskt söka efter en model
web_usage %>%
  model(auto = ARIMA(value ~ pdq(d=1))) %>%
  report()


# göra utförligare sökning efter en model
web_usage %>%
  model(auto2 = ARIMA(value ~ pdq(d=1),
                      stepwise = FALSE, approximation = FALSE,
                      order_constraint = p + q <= 10 
  )) %>%
  report()

gg_tsresiduals(fit)

augment(fit) %>%
  features(.resid, ljung_box, lag = 10, dof = 3)

fit %>% forecast(h = 10) %>% autoplot(web_usage)
fit %>% forecast(h = 3) %>% autoplot(web_usage)
fit %>% forecast(h = 20) %>% autoplot(web_usage)


#-------------------------------------------------------------------------------
## US leisure employment
# see also: https://otexts.com/fpp3/seasonal-arima.html

?us_employment

# monthly US employment data for leisure and hospitality jobs from January 2000 
# to September 2019
leisure <- us_employment %>%
  filter(Title == "Leisure and Hospitality",
         year(Month) > 2000) %>%
  mutate(Employed = Employed/1000) %>%
  select(Month, Employed)
autoplot(leisure, Employed) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

# diff 12:
leisure %>%
  gg_tsdisplay(difference(Employed, 12),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced", y="")
# verkar vara icke stationär


# diff 12 och diff 1
leisure %>%
  gg_tsdisplay(difference(Employed, 12) %>% difference(),
               plot_type='partial', lag=36) +
  labs(title = "Double differenced", y="")
# ser bättre ut

# vilken modell? Inte helt tydligt...
# icke säsong: pdq(0,1,2) eller pdq(2,1,0)
# inte helt tydligt på säsong...
# ev PDQ(0,1,1)



# tar lite tid:
fit <- leisure %>%
  model(
    arima012011 = ARIMA(Employed ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(Employed),
    best = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )
fit
fit %>% pivot_longer(everything(), names_to = "Model name",
                     values_to = "Orders")

# best verkar vara bäst
glance(fit) %>% arrange(AICc) %>% select(.model:BIC)

fit %>% select(best) %>% gg_tsresiduals(lag=36)

report(fit %>% select(best))

# klarar testet
augment(fit) %>% 
  filter(.model %in% c('best','auto')) %>%
  features(.innov, ljung_box, lag=24, dof=4)

# klarar testet
augment(fit) %>% 
  filter(!(.model %in% c('best','auto'))) %>%
  features(.innov, ljung_box, lag=24, dof=3)



forecast(fit, h=36) %>%
  filter(.model=='best') %>%
  autoplot(leisure) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

# liknande prognos med en annan modell
forecast(fit, h=36) %>%
  filter(.model=='arima012011') %>%
  autoplot(leisure) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")


# liknande prognos med en annan modell
forecast(fit, h=36) %>%
  filter(.model=='arima210011') %>%
  autoplot(leisure) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")



## h02 drugs ----------------------------------------------------------------------
?PBS
# Monthly corticosteroid drug sales in Australia. These are known as H02 drugs 
# under the Anatomical Therapeutic Chemical classification scheme.


h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost))

h02 %>% autoplot(Cost)

## Models using logs

h02 %>% autoplot(log(Cost))

# diff 12
h02 %>% gg_tsdisplay(difference(log(Cost),12), lag_max = 36, plot_type='partial')
# tre spikar i pacf, dör av i acf -> p=3
# spikar på lag 12 och 24 i pcf -> kan vara P=2 

# Gissning
fit <- h02 %>%
  # notera: log(Cost)
  model(arima3000210 = ARIMA(log(Cost) ~ pdq(3,0,0) + PDQ(2,1,0)))
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
# tar lite tid:
fit <- h02 %>%
  model(best = ARIMA(log(Cost), stepwise = FALSE,
                     approximation = FALSE,
                     order_constraint = p + q + P + Q <= 9 & (constant + d + D <= 2)))
report(fit)
gg_tsresiduals(fit, lag_max=36)
augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 9)

# The forecasts
fit %>% forecast %>% 
  autoplot(h02) +
  labs(y="H02 Expenditure ($AUD)")


