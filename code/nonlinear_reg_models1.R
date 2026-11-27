# ============================================================
# Nonlinear regression models
# ============================================================

rm(list = ls())
set.seed(73252)


# ============================================================
# 1. Simulate nonlinear regression data
# ============================================================

n <- 200
x <- seq(-3, 3, length.out = n)

f_true <- 2 + 0.8 * x - 1.8 * exp(-1.5 * (x + 1)^2) +
  2.2 * exp(-2 * (x - 1.2)^2)

y <- f_true + rnorm(n, mean = 0, sd = 0.6)
data <- data.frame(x = x, y = y, f_true = f_true)

plot(y ~ x, data = data, pch = 19, col = "grey50", xlab = "x", ylab = "y")
lines(data$x, data$f_true, col = "black", lwd = 3)
legend("topleft", legend = c("Data", "True function"),
       col = c("grey50", "black"), pch = c(19, NA), lty = c(NA, 1),
       lwd = c(NA, 3), bty = "n")


# ============================================================
# 2. Split data into training and validation sets
# ============================================================

split_prop <- c(train = 0.70, validation = 0.30)
n_train <- floor(n * split_prop["train"])

index <- sample(seq_len(n))
train_index <- index[1:n_train]
val_index <- index[(n_train + 1):n]

train_data <- data[train_index, ]
val_data <- data[val_index, ]

plot(y ~ x, data = train_data, pch = 19, col = "blue",
     xlab = "x", ylab = "y", xlim = range(data$x), ylim = range(data$y))
points(y ~ x, data = val_data, pch = 17, col = "red")
legend("topleft", legend = c("Training", "Validation"),
       col = c("blue", "red"), pch = c(19, 17), bty = "n")

mse <- function(observed, predicted) mean((observed - predicted)^2)


# ============================================================
# 3. Global polynomial models
# ============================================================

polynomial_1 <- lm(y ~ x, data = train_data)
polynomial_2 <- lm(y ~ x + I(x^2), data = train_data)
polynomial_3 <- lm(y ~ x + I(x^2) + I(x^3), data = train_data)
polynomial_5 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5),
                   data = train_data)
polynomial_9 <- lm(y ~ poly(x, degree = 9, raw = TRUE), data = train_data)

polynomial_models <- list(
  "Degree 1" = polynomial_1,
  "Degree 2" = polynomial_2,
  "Degree 3" = polynomial_3,
  "Degree 5" = polynomial_5,
  "Degree 9" = polynomial_9
)

polynomial_mse <- sapply(polynomial_models, function(model) {
  mse(val_data$y, predict(model, newdata = val_data))
})

round(polynomial_mse, 3)

x_grid <- seq(min(data$x), max(data$x), length.out = 500)
grid_data <- data.frame(x = x_grid)
polynomial_colors <- c("blue", "darkgreen", "red", "purple", "orange")

plot(y ~ x, data = train_data, pch = 19, col = "grey60",
     xlab = "x", ylab = "y", main = "Global polynomial models")

for (i in seq_along(polynomial_models)) {
  lines(x_grid, predict(polynomial_models[[i]], newdata = grid_data),
        col = polynomial_colors[i], lwd = 2)
}

legend("topleft", legend = names(polynomial_models),
       col = polynomial_colors, lty = 1, lwd = 2, bty = "n")


# ============================================================
# 4. Local polynomials using region indicators
# ============================================================

# Two regions separated at x = 0
train_data$region2 <- as.integer(train_data$x > 0)
val_data$region2 <- as.integer(val_data$x > 0)
grid_data$region2 <- as.integer(grid_data$x > 0)

region_linear <- lm(y ~ x + region2 + x:region2, data = train_data)

region_quadratic <- lm(
  y ~ x + I(x^2) + region2 + x:region2 + I(x^2):region2,
  data = train_data
)

# Three regions separated at x = -1 and x = 1
train_data$region3 <- cut(train_data$x, breaks = c(-Inf, -1, 1, Inf))
val_data$region3 <- cut(val_data$x, breaks = c(-Inf, -1, 1, Inf))
grid_data$region3 <- cut(grid_data$x, breaks = c(-Inf, -1, 1, Inf))

region_three_linear <- lm(y ~ x * region3, data = train_data)
region_three_quadratic <- lm(y ~ (x + I(x^2)) * region3, data = train_data)

region_models <- list(
  "2 regions: linear" = region_linear,
  "2 regions: quadratic" = region_quadratic,
  "3 regions: linear" = region_three_linear,
  "3 regions: quadratic" = region_three_quadratic
)

region_mse <- sapply(region_models, function(model) {
  mse(val_data$y, predict(model, newdata = val_data))
})

round(region_mse, 3)

region_colors <- c("blue", "red", "darkgreen", "purple")

plot(y ~ x, data = train_data, pch = 19, col = "grey60",
     xlab = "x", ylab = "y", main = "Local polynomial models")

for (i in seq_along(region_models)) {
  lines(x_grid, predict(region_models[[i]], newdata = grid_data),
        col = region_colors[i], lwd = 2)
}

abline(v = c(-1, 0, 1), lty = 3, col = "grey60")
legend("topleft", legend = names(region_models),
       col = region_colors, lty = 1, lwd = 2, bty = "n")


# ============================================================
# 5. Cosine basis models
# ============================================================

x_min <- min(data$x)
x_max <- max(data$x)
L <- x_max - x_min

cosine_basis <- function(x, k) cos(pi * k * (x - x_min) / L)

for (k in 1:8) {
  train_data[[paste0("cos", k)]] <- cosine_basis(train_data$x, k)
  val_data[[paste0("cos", k)]] <- cosine_basis(val_data$x, k)
  grid_data[[paste0("cos", k)]] <- cosine_basis(grid_data$x, k)
}

par(mfrow=c(3,2))
plot(train_data$x,train_data$cos1,main="cosine basis: k = 1")
plot(train_data$x,train_data$cos2,main="cosine basis: k = 2")
plot(train_data$x,train_data$cos3,main="cosine basis: k = 3")
plot(train_data$x,train_data$cos4,main="cosine basis: k = 4")
plot(train_data$x,train_data$cos5,main="cosine basis: k = 5")
plot(train_data$x,train_data$cos6,main="cosine basis: k = 6")
par(mfrow=c(1,1))

head(train_data)

cosine_1 <- lm(y ~ cos1, data = train_data)
cosine_2 <- lm(y ~ cos1 + cos2, data = train_data)
cosine_3 <- lm(y ~ cos1 + cos2 + cos3, data = train_data)
cosine_5 <- lm(y ~ cos1 + cos2 + cos3 + cos4 + cos5, data = train_data)
cosine_8 <- lm(y ~ cos1 + cos2 + cos3 + cos4 + cos5 + cos6 + cos7 + cos8,
               data = train_data)

cosine_models <- list(
  "K = 1" = cosine_1,
  "K = 2" = cosine_2,
  "K = 3" = cosine_3,
  "K = 5" = cosine_5,
  "K = 8" = cosine_8
)

cosine_mse <- sapply(cosine_models, function(model) {
  mse(val_data$y, predict(model, newdata = val_data))
})

round(cosine_mse, 3)

cosine_colors <- c("blue", "darkgreen", "red", "purple", "orange")

plot(y ~ x, data = train_data, pch = 19, col = "grey60",
     xlab = "x", ylab = "y", main = "Cosine basis models")

for (i in seq_along(cosine_models)) {
  lines(x_grid, predict(cosine_models[[i]], newdata = grid_data),
        col = cosine_colors[i], lwd = 2)
}

legend("topleft", legend = names(cosine_models),
       col = cosine_colors, lty = 1, lwd = 2, bty = "n")


# ============================================================
# 6. LOESS models
# ============================================================

loess_020 <- loess(y ~ x, data = train_data, span = 0.20, degree = 1)
loess_050 <- loess(y ~ x, data = train_data, span = 0.50, degree = 1)
loess_080 <- loess(y ~ x, data = train_data, span = 0.80, degree = 1)
loess_quad <- loess(y ~ x, data = train_data, span = 0.50, degree = 2)

loess_models <- list(
  "span = 0.20, linear" = loess_020,
  "span = 0.50, linear" = loess_050,
  "span = 0.80, linear" = loess_080,
  "span = 0.50, quadratic" = loess_quad
)

loess_mse <- sapply(loess_models, function(model) {
  mse(val_data$y, predict(model, newdata = val_data))
})

round(loess_mse, 3)

loess_colors <- c("blue", "red", "darkgreen", "purple")

plot(y ~ x, data = train_data, pch = 19, col = "grey60",
     xlab = "x", ylab = "y", main = "LOESS models")

for (i in seq_along(loess_models)) {
  lines(x_grid, predict(loess_models[[i]], newdata = grid_data),
        col = loess_colors[i], lwd = 2)
}

legend("topleft", legend = names(loess_models),
       col = loess_colors, lty = 1, lwd = 2, bty = "n")


# ============================================================
# 7. Compare validation errors
# ============================================================

validation_results <- data.frame(
  model = c(
    paste("Polynomial", names(polynomial_mse)),
    paste("Region", names(region_mse)),
    paste("Cosine", names(cosine_mse)),
    paste("LOESS", names(loess_mse))
  ),
  validation_mse = c(
    polynomial_mse,
    region_mse,
    cosine_mse,
    loess_mse
  )
)

validation_results <- validation_results[
  order(validation_results$validation_mse),
]

validation_results$validation_mse <- round(validation_results$validation_mse, 3)
validation_results



# ============================================================
# 8. Compare selected models in one plot
# ============================================================

selected_models <- list(
  "Linear" = polynomial_1,
  "Cubic polynomial" = polynomial_3,
  "Cosine K = 3" = cosine_3,
  "LOESS" = loess_050
)

selected_colors <- c("blue", "red", "darkgreen", "purple")

plot(y ~ x, data = train_data, pch = 19, col = "grey70",
     xlab = "x", ylab = "y", main = "Comparison of nonlinear models")

points(y ~ x, data = val_data, pch = 17, col = "grey30")
lines(x_grid, predict(polynomial_1, newdata = grid_data),
      col = selected_colors[1], lwd = 2)
lines(x_grid, predict(polynomial_3, newdata = grid_data),
      col = selected_colors[2], lwd = 2)
lines(x_grid, predict(cosine_3, newdata = grid_data),
      col = selected_colors[3], lwd = 2)
lines(x_grid, predict(loess_050, newdata = grid_data),
      col = selected_colors[4], lwd = 2)

legend("topleft",
       legend = c("Training data", "Validation data", names(selected_models)),
       col = c("grey70", "grey30", selected_colors),
       pch = c(19, 17, rep(NA, 4)), lty = c(NA, NA, rep(1, 4)),
       lwd = c(NA, NA, rep(2, 4)), bty = "n")


# ============================================================
# 9. Extrapolation outside the observed x interval
# ============================================================

x_extra <- seq(-4.5, 4.5, length.out = 700)
extra_data <- data.frame(x = x_extra)

for (k in 1:3) {
  extra_data[[paste0("cos", k)]] <- cosine_basis(extra_data$x, k)
}

plot(y ~ x, data = data, pch = 19, col = "grey70",
     xlim = range(x_extra), xlab = "x", ylab = "y",
     main = "Extrapolation outside the observed interval")

lines(x_extra, predict(polynomial_1, newdata = extra_data),
      col = "blue", lwd = 2)
lines(x_extra, predict(polynomial_3, newdata = extra_data),
      col = "red", lwd = 2)
lines(x_extra, predict(cosine_3, newdata = extra_data),
      col = "darkgreen", lwd = 2)

abline(v = c(x_min, x_max), lty = 2, col = "grey40")

legend("topleft",
       legend = c("Linear", "Cubic polynomial", "Cosine K = 3"),
       col = c("blue", "red", "darkgreen"), lty = 1, lwd = 2, bty = "n")

