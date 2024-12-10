

library(fpp3)
# Kod från: https://otexts.com/fpp3/training-test.html
# (med vissa ändringar)


auscafe <- aus_retail |>
  filter(stringr::str_detect(Industry, "Takeaway")) |>
  summarise(Turnover = sum(Turnover))

autoplot(auscafe)

training <- auscafe |> filter(year(Month) <= 2013)
test <- auscafe |> filter(year(Month) > 2013)
cafe_fit <- training |>
  model(ARIMA(log(Turnover)))
cafe_fit |>
  forecast(h = 60) |>
  autoplot(auscafe) +
  labs(title = "Australian food expenditure",
       y = "$ (billions)")





fits12 <- fitted(cafe_fit, h = 12)
training |>
  autoplot(Turnover) +
  autolayer(fits12, .fitted, col = "#D55E00") +
  labs(title = "Australian food expenditure",
       y = "$ (billions)")


cafe_fit |>
  refit(test) |>
  accuracy()
#> # A tibble: 1 × 10
#>   .model              .type    ME  RMSE   MAE    MPE  MAPE  MASE RMSSE    ACF1
#>   <chr>               <chr> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>   <dbl>
#> 1 ARIMA(log(Turnover… Trai… -2.49  20.5  15.4 -0.169  1.06 0.236 0.259 -0.0502