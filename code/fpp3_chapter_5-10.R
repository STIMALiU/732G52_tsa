library(fpp3)

# Kod från: https://otexts.com/fpp3/tscv.html
# (med vissa ändringar)
?gafa_stock
# Re-index based on trading days
google_stock <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2015) |>
  mutate(day = row_number()) |>
  update_tsibble(index = day, regular = TRUE)
# Filter the year of interest
google_2015 <- google_stock |> filter(year(Date) == 2015)


autoplot(google_2015,Close) 

# Time series cross-validation accuracy
google_2015_tr <- google_2015 |>
  stretch_tsibble(.init = 3, .step = 1) |>
  relocate(Date, Symbol, .id)
google_2015_tr


# TSCV accuracy
google_2015_tr |>
  model(RW(Close ~ drift())) |>
  forecast(h = 1) |>
  accuracy(google_2015)
# Training set accuracy
google_2015 |>
  model(RW(Close ~ drift())) |>
  accuracy()


# gör prognoser 8 steg framåt
# h är prognossteg

google_2015_tr <- google_2015 |>
  stretch_tsibble(.init = 3, .step = 1)
# .init = 3 : första delen av data som används i korsvalideringen har 3 observationer
# .step = 1 : vi utökar den första delen med en ny observation i taget, skattar modeller, 
# och sen fortsätter tills alla observationer finns i data


fc <- google_2015_tr |>
  model(RW(Close ~ drift())) |>
  forecast(h = 8) |> # här anger vi att vi vill göra prognos upp till 8 tidsenheter framåt
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Close", distribution = Close)

# Plottar prognossteg (h) mot RMSE för korsvalideringen:
fc |>
  accuracy(google_2015, by = c("h", ".model")) |>
  ggplot(aes(x = h, y = RMSE)) +
  geom_point()+geom_line()



#-------------------------------------------------------------------------------
# Testar att jämföra tre modeller på google_2015-data
#-------------------------------------------------------------------------------
google_2015_tr <- google_2015 |>
  # ökar .init = 100
  stretch_tsibble(.init = 100, .step = 1) 
# .init = 100 : första delen av data som används i korsvalideringen har 100 observationer
# .step = 1 : vi utökar den första delen med en ny observation i taget, skattar modeller, 
# och sen fortsätter tills alla observationer finns i data

?RW
fc <- google_2015_tr |>
  model(rw=RW(Close ~ drift())) |> # Random walk model
  forecast(h = 8) |> # här anger vi att vi vill göra prognos upp till 8 tidsenheter framåt
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Close", distribution = Close)

# skattar en ARIMA modell
?ARIMA
fc2 <- google_2015_tr |>
  model(arima=ARIMA(Close ~ pdq(1,1,1)+PDQ(0, 0, 0)+ 0)) |> # ARIMA model
  forecast(h = 8) |> # <- här anger vi att vi vill göra prognos upp till 8 tidsenheter framåt
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Close", distribution = Close)

# skattar en Exponential smoothing model
?ETS
fc3 <- google_2015_tr |>
  model(ets=ETS(Close ~ error("A") +trend("A") + season("N"))) |> # Exponential smoothing model med trend
  forecast(h = 8) |> # här anger vi att vi vill göra prognos upp till 8 tidsenheter framåt
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Close", distribution = Close)



fc |>
  accuracy(google_2015, by = c("h", ".model")) |>
  ggplot(aes(x = h, y = RMSE)) +
  geom_point()+geom_line()


fc2 |>
  accuracy(google_2015, by = c("h", ".model")) |>
  ggplot(aes(x = h, y = RMSE)) +
  geom_point()+geom_line()


fc3 |>
  accuracy(google_2015, by = c("h", ".model")) |>
  ggplot(aes(x = h, y = RMSE)) +
  geom_point()+geom_line()




#-------------------------------------------------------------------------------
# Plottar modellerna tillsammans
#-------------------------------------------------------------------------------

error_3<-fc3 |> accuracy(google_2015, by = c("h", ".model"))
error_2<-fc2 |> accuracy(google_2015, by = c("h", ".model"))
error_1<-fc |> accuracy(google_2015, by = c("h", ".model"))

error<-bind_rows(error_1,error_2,error_3)

# plottar jämförande kurvor för olika utvärderingsmått 

# RMSE
plot1<-error|> ggplot(aes(x = h, y = RMSE,group = .model)) +
  geom_point(aes(color=.model))+geom_line(aes(color=.model))
plot1

# MAE
plot2<-error|> ggplot(aes(x = h, y = MAE,group = .model)) +
  geom_point(aes(color=.model))+geom_line(aes(color=.model))
plot2

# MAPE
plot3<-error|> ggplot(aes(x = h, y = MAPE,group = .model)) +
  geom_point(aes(color=.model))+geom_line(aes(color=.model))
plot3


library(cowplot)
plot_grid(plot1,plot2,plot3,nrow = 3)
# vilken modell är bäst här?

