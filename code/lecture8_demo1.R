
rm(list=ls())
library(fpp3)
library(lubridate)

# baserat på 
# https://github.com/robjhyndman/ETC3550Slides/blob/fable/10-dynamic-regression.R

## US CHANGE -------------------------------------------------------------------
?us_change
data("us_change")
us_change %>%
  gather(key='variable', value='value') %>%
  ggplot(aes(y=value, x=Quarter, group=variable, colour=variable)) +
  geom_line() + facet_grid(variable ~ ., scales='free_y') +
  xlab("Year") + ylab("") +
  ggtitle("Quarterly changes in US consumption and personal income") +
  guides(colour="none")

# responsvariabel: Consumption
# förklarande variabler:
# Income, Production, Savings, Unemployment

# dessa kan anses stokastiska, men ser stationära ut
# även Consumption (=y) ser stationär ut

# kolla om det verkar finnas linjära samband
plot(us_change[,2:6])
round(cor(us_change[,2:6]),2)
library(GGally)
ggpairs(us_change[,c(3:6,2)])

# Vanlig regression
?TSLM
fit <- us_change %>% model(TSLM(Consumption ~ Income + Production +
                                  Savings + Unemployment))


gg_tsresiduals(fit)
fit%>%report() # skattningar mm


# Dynamic regression = regression + ARIMA för feltermen
fit <- us_change %>%
  model(
    ARIMA(Consumption ~ Income + Savings + Unemployment + pdq(d=0) + PDQ(0,0,0),
          stepwise=FALSE, approximation=FALSE, order_constraint = p+q <= 10)
    # vi antar att ARIMA inte har någon säsongskomponenter
    # vi tar inga vanliga diffar: d=0, annars letar funktionen  med hjälp
    # av AICc efter en lämplig modell
  )
report(fit)


gg_tsresiduals(fit)

residuals(fit, type='regression') %>%
  gg_tsdisplay(.resid, plot_type = 'partial') +
  ggtitle("Regression errors")

residuals(fit, type='innovation') %>%
  gg_tsdisplay(.resid, plot_type = 'partial') +
  ggtitle("ARIMA errors")


# vi väljer en fix ordning: ARIMA(2,0,1)-modell nedan:
fit2 <- us_change %>%
  model(
    ARIMA(Consumption ~ Income + Savings + Unemployment + pdq(p=2,d=0,q=1) + PDQ(0,0,0),
          stepwise=FALSE, approximation=FALSE, order_constraint = p+q <= 10)
    # vi antar att ARIMA inte har någon säsongskomponenter
    # vi tar inga vanliga diffar: d=0, annars letar funktionen  med hjälp
    # av AICc efter en lämplig modell
  )


#-------------------------------------------------------------------------------
# prognos:
# efter som x kan anses stokastiska måste vi ha prognos för de olika x
# för att kunna göra prognos in i framtiden
# här antar vi en enkel prognosmodell för x: 
# medelvärdet över historiska data för varje variabel
us_change_future <- new_data(us_change, 11) %>%
  mutate(
    Income = mean(us_change$Income),
    Savings = mean(us_change$Savings),
    Unemployment = mean(us_change$Unemployment)
  )
us_change_future
# med hjälp av ny data så gör vi prognoser för framtida konsumption:
forecast(fit, new_data = us_change_future) %>%
  autoplot(us_change) +
  labs(x = "Year", y = "Percentage change",
       title = "Forecasts from regression with ARIMA(2,0,1) errors")

