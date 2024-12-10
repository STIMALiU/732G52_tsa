

library(fpp3)

trucks0<-read.csv("https://raw.githubusercontent.com/STIMALiU/732G52_tsa/refs/heads/main/data/NyLastbilregistreringar.csv")

head(trucks0)
temp_date<-paste(trucks0$Ar,trucks0$Manad)
trucks<-tsibble(month=yearmonth(temp_date),index=month,count=trucks0$Antal)
trucks

autoplot(trucks)


trucks_cv_tr <- trucks |> stretch_tsibble(.init = 280, .step = 1)

model1_results <- trucks_cv_tr |>
  model(model1=ARIMA(count~pdq(1,1,1)+PDQ(0,1,0))) |>
  forecast(h = 12) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "count", distribution = count)


model2_results <- trucks_cv_tr |>
  model(model2=ARIMA(count~pdq(3,1,0)+PDQ(0,1,1))) |>
  forecast(h = 12) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "count", distribution = count)


# kollar en modell i taget föst:
model1_results |>
  accuracy(trucks, by = c("h", ".model")) |>
  ggplot(aes(x = h, y = RMSE)) +
  geom_point()+geom_line()

model2_results |>
  accuracy(trucks, by = c("h", ".model")) |>
  ggplot(aes(x = h, y = RMSE)) +
  geom_point()+geom_line()



#-------------------------------------------------------------------------------
# Plottar modellerna tillsammans
#-------------------------------------------------------------------------------
error_1<-model1_results |> accuracy(trucks, by = c("h", ".model"))
error_2<-model2_results |> accuracy(trucks, by = c("h", ".model"))

error<-bind_rows(error_1,error_2)

error|> ggplot(aes(x = h, y = RMSE,group = .model)) +
  geom_point(aes(color=.model))+geom_line(aes(color=.model))

error|> ggplot(aes(x = h, y = MAPE,group = .model)) +
  geom_point(aes(color=.model))+geom_line(aes(color=.model))

# vilken modell är bäst här?


# uppdatera koden för fler modeller

