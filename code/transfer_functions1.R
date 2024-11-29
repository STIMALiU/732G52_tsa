# Load necessary library
library(TSA)
rm(list=ls())
# Simulate data
set.seed(12345)
n <- 100
time <- 1:n
input_series <- as.numeric(time > 50)*3 # Intervention: Step function after time 50
noise <- arima.sim(model = list(ar = c(-0.7)), n = n) # AR(1) noise process
output_series <- filter(input_series, filter = 0.5, method = "recursive") + noise

# Plot input and output time series

par(mfrow = c(2, 1))
plot(input_series, main = "Input Series (Intervention)", ylab = "Input",t="l")
plot(output_series, main = "Output Series", ylab = "Output")
abline(v=50,col="red",lty="dashed")
par(mfrow = c(1, 1))

# Fit a transfer function-noise model
?TSA::arimax
fit1 <- arimax(output_series, 
              xtransf = data.frame(intervention = input_series), # Intervention
              transfer = list(c(1, 0)),                          # Transfer function parameters
              order = c(1, 0, 0))                               # AR(1) noise model

# Model summary
print(fit1)

# Visualize fitted values vs observed
fitted_values1 <- fitted(fit1)
plot(output_series, main = "Observed and Fitted Values")
lines(fitted_values1, col = "blue", lwd = 2)
legend("topright", legend = c("Observed", "Fitted"), col = c("black", "blue"), lty = 1, lwd = 2)






# testing a more advanced model:

# Fit a transfer function-noise model
fit2 <- arimax(output_series, 
              xtransf = data.frame(intervention = input_series), # Intervention
              transfer = list(c(3, 0)),                          # Transfer function parameters
              order = c(3, 0, 0))                               # AR(3) noise model

# Model summary
print(fit2)

# Visualize fitted values vs observed
fitted_values2 <- fitted(fit2)
plot(output_series, main = "Observed and Fitted Values")
lines(fitted_values1, col = "blue", lwd = 2)
lines(fitted_values2, col = "darkgreen", lwd = 2)
legend("topright", legend = c("Observed", "Fitted1","Fitted2"), col = c("black", "blue","darkgreen"), lty = 1, lwd = 2)



# inspect residuals:

source("https://raw.githubusercontent.com/STIMALiU/732G52_tsa/refs/heads/main/code/residual_diagnostics.R")

res_vect1<-output_series-fitted_values1
res_vect2<-output_series-fitted_values2

# model 1:
residual_diagnostics(res_vect = res_vect1,fit_vect = fitted_values1)
# model 2:
residual_diagnostics(res_vect = res_vect2,fit_vect = fitted_values2)

# model 1:
acf(res_vect1)
# model 2:
acf(res_vect2)


