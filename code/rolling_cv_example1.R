

rm(list=ls())
set.seed(3)
# Simulate AR(1) data

# case 1:
nobs<-20
train_index<-1:10
val_index<-11:20

# case 2
# nobs<-40
# train_index<-1:10
# val_index<-11:40

y <- round(arima.sim(model = list(ar = 0.95),n = nobs,n.start = 100),1)+10
y
plot(y)
# ------------------------------------------------------------
# Fixed model
# Fit once using observations 1--15
# ------------------------------------------------------------



fix_model <- ar(x = y[train_index],order.max = 1,aic = FALSE)
fix_model$ar


# ------------------------------------------------------------
# One-step forecasts for observations 16--20
# Fixed model
# ------------------------------------------------------------

# Fixed AR(1) model estimated using observations in train_index
fix_model <- ar(y[train_index], order.max = 1, aic = FALSE)
fix_phi <- fix_model$ar
fix_mean <- mean(y[train_index])
# Rolling one-step-ahead forecasts with fixed parameters
fix_forecast <- numeric(length(val_index))
last_obs_train<-tail(train_index,1)
for (i in val_index) {
  fix_forecast[i - last_obs_train] <- fix_mean + fix_phi * (y[i - 1] - fix_mean)
}
fix_forecast


# ------------------------------------------------------------
# One-step forecasts for observations 16--20
# Refit model when new observations become available
# ------------------------------------------------------------

refit_phi <- numeric(length(val_index))
refit_forecast <- numeric(length(val_index))
i<-val_index[1]
for (i in val_index) {
  current_data <- y[1:(i - 1)]
  current_model <- ar(current_data, order.max = 1, aic = FALSE)
  refit_phi[i - last_obs_train] <- current_model$ar
  current_mean <- mean(current_data)
  
  refit_forecast[i - last_obs_train] <- current_mean +
  current_model$ar * (y[i - 1] - current_mean)
}


# ------------------------------------------------------------
# Compare results
# ------------------------------------------------------------

results <- data.frame(
  time = 16:20,
  y = y[16:20],
  fixed_phi = fix_phi,
  fixed_forecast = fix_forecast,
  refit_phi = refit_phi,
  refit_forecast = refit_forecast
)

round(results, 3)

# calculate errors
abs_error_fixed<-abs(y[val_index]-results$fixed_forecast)
abs_error_refit<-abs(y[val_index]-results$refit_forecast)
plot(val_index,abs_error_fixed,t="o",col="blue",ylab="Absolute error",
     ylim=c(0,max(abs_error_fixed,abs_error_refit)*1.1),
     main="Absolute error for validation data",xlab="Time")
lines(val_index,abs_error_refit,t="o",col="red")
legend("topright",
       legend = c("Fixed AR(1)", "Re-estimated AR(1)"),
       col = c("red", "blue"),lwd = c(1, 2, 2), bty = "n")

# Average absolute error for 1-step forecasts
mean(abs_error_fixed)
mean(abs_error_refit)

# ------------------------------------------------------------
# Plot observed data and forecasts
# ------------------------------------------------------------

plot(1:nobs, y, type = "o", pch = 19, col = "black",
     xlab = "Time", ylab = "y", xaxt = "n")

axis(1, at = 1:nobs)
abline(v = last_obs_train+0.5, lty = 2, col = "grey50")

# Forecasts
lines(val_index, fix_forecast, type = "o", pch = 17, lwd = 2, col = "blue")

lines(val_index, refit_forecast, type = "o", pch = 15, lwd = 2, col = "red")

legend("topright",
       legend = c("Observed", "Fixed AR(1)", "Re-estimated AR(1)"),
       col = c("black", "blue", "red"),
       pch = c(19, 17, 15), lty = 1, lwd = c(1, 2, 2), bty = "n")

