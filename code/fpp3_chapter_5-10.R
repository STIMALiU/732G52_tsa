library(fpp3)

# Kod från: https://otexts.com/fpp3/tscv.html
# (med vissa ändringar)

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
google_2015_tr <- google_2015 |>
  stretch_tsibble(.init = 3, .step = 1)
fc <- google_2015_tr |>
  model(RW(Close ~ drift())) |>
  forecast(h = 8) |>
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
# Testar att jämföra två modeller på google_2015-data
#-------------------------------------------------------------------------------
google_2015_tr <- google_2015 |>
  # ökar .init = 100
  stretch_tsibble(.init = 100, .step = 1)

fc <- google_2015_tr |>
  model(rw=RW(Close ~ drift())) |>
  forecast(h = 8) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Close", distribution = Close)

# skattar en ARIMA modell
fc2 <- google_2015_tr |>
  model(arima=ARIMA(Close ~ pdq(2,1,2)+PDQ(0, 0, 0)+ 0)) |>
  forecast(h = 8) |>
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


#-------------------------------------------------------------------------------
# Plottar modellerna tillsammans
#-------------------------------------------------------------------------------

error_2<-fc2 |> accuracy(google_2015, by = c("h", ".model"))
error_1<-fc |> accuracy(google_2015, by = c("h", ".model"))

error<-bind_rows(error_1,error_2)

error|> ggplot(aes(x = h, y = RMSE,group = .model)) +
  geom_point(aes(color=.model))+geom_line(aes(color=.model))

# vilken modell är bäst här?

