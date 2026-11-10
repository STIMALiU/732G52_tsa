# ------------------------------------------------------------
# Model selection with polynomial regression
# ------------------------------------------------------------
rm(list = ls())
set.seed(12)

# Generate data from a cubic model
n <- 100
x <- seq(-2, 2, length.out = n)

beta_0 <- 20
beta_1 <- -1
beta_2 <- 0.5
beta_3 <- 2
sigma <- 3

y <- beta_0 +
  beta_1 * x +
  beta_2 * x^2 +
  beta_3 * x^3 +
  rnorm(n, mean = 0, sd = sigma)


data <- data.frame(x = x, y = y)
plot(data)

# ------------------------------------------------------------
# Split data: 60% training, 20% validation, 20% test
# ------------------------------------------------------------

# ------------------------------------------------------------
# Split data into training, validation and test sets
# ------------------------------------------------------------

split_prop <- c(
  train = 0.60,
  validation = 0.20,
  test = 0.20
)

stopifnot(sum(split_prop) == 1)

# Number of observations in each set
n_train <- floor(n * split_prop["train"])
n_validation <- floor(n * split_prop["validation"])
n_test <- n - n_train - n_validation

# Random permutation of observations
index <- sample(seq_len(n))

# Indices for each set
train_index <- index[seq_len(n_train)]

val_index <- index[
  (n_train + 1):(n_train + n_validation)
]

test_index <- index[
  (n_train + n_validation + 1):n
]

# Create data sets
train_data <- data[train_index, ]
val_data <- data[val_index, ]
test_data <- data[test_index, ]

# Check sizes
c(
  train = nrow(train_data),
  validation = nrow(val_data),
  test = nrow(test_data)
)


# ------------------------------------------------------------
# Fit five candidate models on training data
# ------------------------------------------------------------


model_1 <- lm(
  y ~ x,
  data = train_data
)

model_2 <- lm(
  y ~ x + I(x^2),
  data = train_data
)

model_3 <- lm(
  y ~ x + I(x^2) + I(x^3),
  data = train_data
)

model_4 <- lm(
  y ~ x + I(x^2) + I(x^3) + I(x^4),
  data = train_data
)

model_5 <- lm(
  y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5),
  data = train_data
)


# ------------------------------------------------------------
# Predictions on training data
# ------------------------------------------------------------

pred1_train <- predict(model_1)
pred2_train <- predict(model_2)
pred3_train <- predict(model_3)
pred4_train <- predict(model_4)
pred5_train <- predict(model_5)



# ------------------------------------------------------------
# Predictions on validation data
# ------------------------------------------------------------

pred1_val <- predict(model_1, newdata = val_data)
pred2_val <- predict(model_2, newdata = val_data)
pred3_val <- predict(model_3, newdata = val_data)
pred4_val <- predict(model_4, newdata = val_data)
pred5_val <- predict(model_5, newdata = val_data)



# ------------------------------------------------------------
# Calculate training MSE
# ------------------------------------------------------------

mse1_train <- mean((train_data$y - pred1_train)^2)
mse2_train <- mean((train_data$y - pred2_train)^2)
mse3_train <- mean((train_data$y - pred3_train)^2)
mse4_train <- mean((train_data$y - pred4_train)^2)
mse5_train <- mean((train_data$y - pred5_train)^2)

train_mse <- c(
  mse1_train,
  mse2_train,
  mse3_train,
  mse4_train,
  mse5_train
)


# ------------------------------------------------------------
# Calculate validation MSE
# ------------------------------------------------------------

mse1_val <- mean((val_data$y - pred1_val)^2)
mse2_val <- mean((val_data$y - pred2_val)^2)
mse3_val <- mean((val_data$y - pred3_val)^2)
mse4_val <- mean((val_data$y - pred4_val)^2)
mse5_val <- mean((val_data$y - pred5_val)^2)

validation_mse <- c(
  mse1_val,
  mse2_val,
  mse3_val,
  mse4_val,
  mse5_val
)



# ------------------------------------------------------------
# collect results in a data.frame:
# ------------------------------------------------------------

results <- data.frame(
  degree = 1:5,
  train_mse=train_mse,
  validation_mse = validation_mse
)

print(results)


# ------------------------------------------------------------
# Select the model with lowest validation MSE
# ------------------------------------------------------------

models <- list(
  model_1,
  model_2,
  model_3,
  model_4,
  model_5
)

best_degree <- which.min(validation_mse)
best_model <- models[[best_degree]]

cat("Selected polynomial degree:", best_degree, "\n")


# ------------------------------------------------------------
# Evaluate selected model on test data
# ------------------------------------------------------------

test_predictions <- predict(
  best_model,
  newdata = test_data
)

test_mse <- mean(
  (test_data$y - test_predictions)^2
)

cat("Final model - Train MSE:",mse3_train,"Validation MSE:",mse3_val ,"Test MSE:", test_mse, "\n")


# ------------------------------------------------------------
# Plot validation MSE
# ------------------------------------------------------------

plot(
  1:5,
  train_mse,
  type = "b",
  pch = 19,
  xlab = "Polynomial degree",
  ylab = "MSE",
  xaxt = "n",ylim=c(min(validation_mse,train_mse),max(validation_mse,train_mse))
)
lines(1:5,validation_mse,col="red",t="b")

axis(1, at = 1:5)

abline(
  v = best_degree,
  lty = 2,
  col = "red"
)
legend(
  "topright",
  legend = c("Training", "Validation"),
  col = c("black", "red"),
  lty = 1,
  pch = 19,
  lwd = 2,
  bty = "n"
)

# ------------------------------------------------------------
# Plot training data and fitted polynomial models
# ------------------------------------------------------------

# Sort training data by x
ord <- order(train_data$x)

# Plot training data
plot(
  train_data$x,
  train_data$y,
  pch = 19,
  col = "grey50",
  xlab = "x",
  ylab = "y",
  main = "Polynomial models fitted to training data",
  ylim=c(0,50)
)

# Add fitted models
lines(
  train_data$x[ord],
  pred1_train[ord],
  col = "blue",
  lwd = 2
)

lines(
  train_data$x[ord],
  pred2_train[ord],
  col = "darkgreen",
  lwd = 2
)

lines(
  train_data$x[ord],
  pred3_train[ord],
  col = "red",
  lwd = 2
)

lines(
  train_data$x[ord],
  pred4_train[ord],
  col = "purple",
  lwd = 2
)

lines(
  train_data$x[ord],
  pred5_train[ord],
  col = "orange",
  lwd = 2
)

# Add legend
legend(
  "topleft",
  legend = paste("Degree", 1:5),
  col = c(
    "blue",
    "darkgreen",
    "red",
    "purple",
    "orange"
  ),
  lty = 1,
  lwd = 2,
  bty = "n"
)
