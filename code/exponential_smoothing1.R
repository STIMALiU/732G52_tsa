#-------------------------------------------------------------------------------
# Föreläsning 7: Frågestund och demo
#-------------------------------------------------------------------------------




# frågor?







#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Exponential smoothing
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# based on: https://github.com/robjhyndman/ETC3550Slides/blob/fable/8-ets.R

library(fpp3)



#-------------------------------------------------------------------------------
# Simple exponential smoothing
#-------------------------------------------------------------------------------


# Global economic indicators
?global_economy

# Algerian Exports
algeria_economy <- global_economy %>%
  filter(Country == "Algeria") # välj Algeria

algeria_economy %>% autoplot(Exports)
# Exports of goods and services from Algeria from 1960 to 2017. 

?ETS
# ETS {fable}
# formula	    Model specification (see "Specials" section).
#
# opt_crit = c("lik", "amse", "mse", "sigma", "mae")
#
# ic = c("aicc", "aic", "bic"),

# formula: 
# error(method = c("A", "M"))

# Se "8.4 A taxonomy of exponential smoothing methods" för detaljer
# 
# https://otexts.com/fpp3/taxonomy.html

# trend(method = c("N", "A", "Ad"),
#       alpha = NULL, alpha_range = c(1e-04, 0.9999),
#       beta = NULL, beta_range = c(1e-04, 0.9999),
#       phi = NULL, phi_range = c(0.8, 0.98))

# The form of the trend term: either none ("N"), additive ("A"), 
# multiplicative ("M") or damped variants ("Ad", "Md"). All specified methods 
# are tested on the data, and the one that gives the best fit (lowest ic) will 
# be kept.

fit <- algeria_economy %>%
  model(
    ANN = ETS(Exports ~ error("A") + trend("N") + season("N")),
    MNN = ETS(Exports ~ error("M") + trend("N") + season("N")),
    autoNN = ETS(Exports ~ trend("N") + season("N")),
  )

# kolla resultatet
fit %>%
  select(ANN) %>%
  report()
fit %>%
  select(MNN) %>%
  report()
fit %>%
  select(autoNN) %>%
  report()

tidy(fit)
glance(fit)

# plotta
# components(fit): ger de olika delarna av modellen
components(fit) %>% autoplot()

# add fitted values
components(fit) %>%
  left_join(fitted(fit), by = c("Country", ".model", "Year"))

# prediktion 5 steg
fit %>%
  forecast(h = 5) %>%
  autoplot(algeria_economy) +
  ylab("Exports (% of GDP)") + xlab("Year")

# prediktion 10 steg
fit %>%
  forecast(h = 10) %>%
  autoplot(algeria_economy) +
  ylab("Exports (% of GDP)") + xlab("Year")

fit %>%
  forecast(h = 2) %>%
  autoplot(algeria_economy) +
  ylab("Exports (% of GDP)") + xlab("Year")


# välja manuellt:
fit1 <- algeria_economy %>%
  model(
    ANN1 = ETS(Exports ~ error("A") + trend("N",alpha=0.1) + season("N")),
    ANN2 = ETS(Exports ~ error("A") + trend("N",alpha=0.5) + season("N")),
    ANN3 = ETS(Exports ~ error("A") + trend("N",alpha=0.9) + season("N"))
  )

# kolla resultatet
fit1 %>%
  select(ANN1) %>%
  report()

fit1 %>%
  select(ANN2) %>%
  report()

fit1 %>%
  select(ANN3) %>%
  report()


# plotta
components(fit1) %>% autoplot()

# prediktion 5 steg
fit1 %>%
  forecast(h = 5) %>%
  autoplot(algeria_economy) +
  ylab("Exports (% of GDP)") + xlab("Year")

fit1 %>%
  select(ANN1) %>%
  gg_tsresiduals()

fit1 %>%
  select(ANN2) %>%
  gg_tsresiduals()

fit1 %>%
  select(ANN3) %>%
  gg_tsresiduals()

fit1_comp<-components(fit1) %>%
  left_join(fitted(fit1), by = c("Country", ".model", "Year"))


source(file = "/home/joswi05/Dropbox/Josef/732G42_VT2021/labbar/lm_diagnostics.R")

fit_res_fit<-fit1_comp%>% filter(.model=="ANN3")%>% select(remainder,.fitted)
fit_res_fit

fit1_comp$remainder
model_diagnostics(res_vect = as.vector(fit_res_fit$remainder),fit_vect = as.vector(fit_res_fit$.fitted))



#-------------------------------------------------------------------------------
#  Methods with trend
#-------------------------------------------------------------------------------

# Australia’s population, 1960-2017. 

aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Pop = Population/1e6)
aus_economy %>% autoplot(Pop)
aus_economy %>%
  model(auto = ETS(Pop)) %>%
  report()

# anpassar en modell med 
# error("M"):  multiplikativt fel
# trend:       additiv trend
# season("N")  ingen

fit <- aus_economy %>%
  model(AAN = ETS(Pop ~ error("M") + trend("A") + season("N")))
report(fit)

components(fit) %>% autoplot()

components(fit) %>%
  left_join(fitted(fit), by = c("Country", ".model", "Year"))

fit %>%
  forecast(h = 10) %>%
  autoplot(aus_economy, level = c(75,95)) +
  ylab("Population") + xlab("Year")

# trend("Ad") Damped trend methods

aus_economy %>%
  model(holt = ETS(Pop ~ error("A") + trend("Ad") + season("N"))) %>%
  report()

aus_economy %>%
  model(holt = ETS(Pop ~ error("A") + trend("Ad") + season("N"))) %>%
  forecast(h = 10) %>%
  autoplot(aus_economy)

aus_economy %>%
  model(holt = ETS(Pop ~ error("A") + trend("Ad") + season("N"))) %>%
  forecast(h = 200) %>%
  autoplot(aus_economy)



fit <- aus_economy %>%
  filter(Year <= 2010) %>%
  model(
    ses = ETS(Pop ~ error("A") + trend("N") + season("N")),
    holt = ETS(Pop ~ error("A") + trend("A") + season("N")),
    damped = ETS(Pop ~ error("A") + trend("Ad") + season("N"))
  )

tidy(fit)
glance(fit)
accuracy(fit)
forecast(fit) %>% accuracy(aus_economy)

fit %>% forecast(h = 10) %>% autoplot(aus_economy)

# egg prices

egg_prices <- prices %>%
  filter(!is.na(eggs))
egg_prices %>%
  autoplot(eggs)
fit <- egg_prices %>%
  model(
    ses = ETS(log(eggs) ~ trend("N")),
    holt = ETS(log(eggs) ~ trend("A")),
    damped = ETS(log(eggs) ~ trend("Ad"))
  )
fit %>%
  forecast(h=100) %>%
  autoplot(egg_prices, level=NULL)

fit %>% glance()

fit %>%
  select(holt) %>%
  report()

fit %>%
  select(ses) %>%
  report()

fit %>%
  select(damped) %>%
  report()


# residualerna
fit %>%
  select(holt) %>%
  gg_tsresiduals()


#-------------------------------------------------------------------------------
#  Methods with seasonality
#-------------------------------------------------------------------------------


## Aus holidays
# Australian domestic overnight trips
?tourism
aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips))
aus_holidays %>% autoplot(Trips)

fit <- aus_holidays %>%
  model(
    additive = ETS(Trips ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(Trips ~ error("M") + trend("A") + season("M"))
  )
fit %>%
  select(multiplicative) %>%
  report()
fc <- fit %>% forecast()

fc %>%
  autoplot(aus_holidays) + xlab("Year") +
  ylab("Overnight trips (thousands)")

components(fit) %>% autoplot()

components(fit)%>%select(multiplicative) %>% autoplot()

fit %>%
  select(multiplicative) %>%components() %>% autoplot()

fit %>%
  select(additive) %>%components() %>% autoplot()

components(fit) %>% select(multiplicative) %>% autoplot()

fit %>%
  select(multiplicative) %>%
  report()

fit %>%
  select(additive) %>%
  report()



# Gas production

aus_production %>% autoplot(Gas)
fit <- aus_production %>%
  model(
    hw = ETS(Gas ~ error("M") + trend("A") + season("M")),
    hwdamped = ETS(Gas ~ error("M") + trend("Ad") + season("M")),
  )

fit %>% tidy()
fit %>% glance()

fit %>%
  select(hw) %>%
  gg_tsresiduals()

fit %>%
  select(hw) %>%
  report()


fit %>%
  forecast(h=12) %>%
  filter(.model == "hw") %>%
  autoplot(aus_production)

fit %>%
  forecast(h=12) %>%
  filter(.model == "hwdamped") %>%
  autoplot(aus_production)


## H02
# Monthly Medicare Australia prescription data
?PBS
h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost))
h02 %>%
  autoplot(Cost)

h02 %>% model(ETS(Cost)) %>% report()

h02 %>% model(ETS(Cost ~ error("A") + trend("A") + season("A"))) %>%
  report()

h02 %>% model(ETS(Cost)) %>% forecast() %>% autoplot(h02)

fit <- h02 %>%
  model(
    auto = ETS(Cost),
    AAA = ETS(Cost ~ error("A") + trend("A") + season("A")),
    damped = ETS(Cost ~ trend("Ad")),
    forbidden = ETS(Cost ~ error("A") + trend("Ad") + season("M"))
  )

fit %>% accuracy()
fit %>% glance()
fit %>% tidy()
