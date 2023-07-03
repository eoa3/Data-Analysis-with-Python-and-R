# loading all the necessary libraries 
library(tidyverse)
library(lubridate)
library(forecast)
library(gridExtra)
library(ggplot2)
library(e1071)

# import dataset
View(data)

# preprocess dataset
data <- data %>%
  select(time, SMOIS)
View(data)

# checking the head of the dataset
head(data)

# knowing the summary
summary(data$SMOIS)

# converting Datetime to a date-time format R can understand
data$Datetime <- as.POSIXct(data$Datetime)

# making sure the class is numeric
data$SMOIS <- as.numeric(data$SMOIS)

# checking the class
class(data$SMOIS)

# time series plot
ggplot(data, aes(x = Hour, y = SMOIS)) +
  geom_line(color = "red") +
  labs(title = "SMOIS Over Time",
       x = "Date and Time",
       y = "SMOIS") +
  theme_minimal()

# Create a variable to represent time
data <- data %>%
  mutate(time = as.numeric(difftime(Datetime, min(Datetime), units = "hours")))
View(data)

# First Model: Linear Regression

# Split the data into training and test sets
set.seed(123)
train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data1 <- data[train_indices, ]
test_data1 <- data[-train_indices, ]


str(train_data1)
str(test_data1)

# Fit the model on the training set
train_data1$SMOIS <- as.numeric(train_data1$SMOIS)
test_data1$SMOIS <- as.numeric(test_data1$SMOIS)
train_model <- lm(SMOIS ~ time, data = train_data1)
summary(train_model)

# Predict SMOIS values for the test set
predictions <- predict(train_model, newdata = test_data1)

# Calculate the root mean squared error (RMSE)
rmse_LR <- sqrt(mean((test_data1$SMOIS - predictions)^2))
cat("RMSE LR:", rmse_LR)

#plot the actual vs predicted values
p1 <- ggplot() +
  geom_point(data = test_data1, aes(x = SMOIS, y = predictions), color = "orange") +
  geom_abline(slope = 1, intercept = 0, color = "green") +
  labs(title = "Actual vs. Predicted SMOIS",
       x = "Actual SMOIS",
       y = "Predicted SMOIS") +
  theme_minimal()


# Second Model: Support Vector Regression (SVR)

# Fit an SVR model on the training set
svr_model <- svm(SMOIS ~ time, data = train_data1, kernel = "radial")

# Display the SVR model summary
summary(svr_model)

# Predict SMOIS values for the test set using the SVR model
svr_predictions <- predict(svr_model, newdata = test_data1)

# Calculate the root mean squared error (RMSE) for the SVR model
svr_rmse_radial <- sqrt(mean((test_data1$SMOIS - svr_predictions)^2))
cat("SVR RMSE RADIAL:", svr_rmse_radial)

# Fit an SVR model on the training set
svr_model_linear <- svm(SMOIS ~ time, data = train_data1, kernel = "linear")

# Display the SVR model summary
summary(svr_model_linear)

# Predict SMOIS values for the test set using the SVR model
svr_predictions_linear <- predict(svr_model_linear, newdata = test_data1)

# Calculate the root mean squared error (RMSE) for the linear SVR model
svr_rmse_linear <- sqrt(mean((test_data1$SMOIS - svr_predictions_linear)^2))
cat("SVR RMSE LINEAR:", svr_rmse_linear)

# Fit an SVR model on the training set
svr_model_poly <- svm(SMOIS ~ time, data = train_data1, kernel = "poly")

# Display the SVR model summary
summary(svr_model_poly)

# Predict SMOIS values for the test set using the SVR model
svr_predictions_poly <- predict(svr_model_poly, newdata = test_data1)

# Calculate the root mean squared error (RMSE) for the linear SVR model
svr_rmse_poly <- sqrt(mean((test_data1$SMOIS - svr_predictions_poly)^2))
cat("SVR RMSE POLY:", svr_rmse_poly)

# Plot the actual vs. predicted values for the linear regression and SVR models
# linear regression has been plotted above
p2 <- ggplot() +
  geom_point(data = test_data1, aes(x = SMOIS, y = svr_predictions), color = "purple") +
  geom_abline(slope = 1, intercept = 0, color = "brown") +
  labs(title = "SVR: Actual vs. Predicted SMOIS",
       x = "Actual SMOIS",
       y = "Predicted SMOIS") +
  theme_minimal()

grid.arrange(p1, p2, ncol = 2)


# Third Model: Random Forest

# Fit a Random Forest model on the training set using 100, 200, and 500 for ntree
rf_model <- randomForest(SMOIS ~ time, data = train_data1, ntree = 100)

# Display the Random Forest model summary
summary(rf_model)

# Predict SMOIS values for the test set using the Random Forest model
rf_predictions <- predict(rf_model, newdata = test_data1)

# Calculate the root mean squared error (RMSE) for the Random Forest model
rf_rmse_100 <- sqrt(mean((test_data1$SMOIS - rf_predictions)^2))
cat("Random Forest RMSE 100:", rf_rmse_100)

# Fit a Random Forest model on the training set using 100, 200, and 500 for ntree
rf_model_200 <- randomForest(SMOIS ~ time, data = train_data1, ntree = 200)

# Display the Random Forest model summary
summary(rf_model_200)

# Predict SMOIS values for the test set using the Random Forest model
rf_predictions_200 <- predict(rf_model_200, newdata = test_data1)

# Calculate the root mean squared error (RMSE) for the Random Forest model
rf_rmse_200 <- sqrt(mean((test_data1$SMOIS - rf_predictions_200)^2))
cat("Random Forest RMSE_200:", rf_rmse_200)

# Fit a Random Forest model on the training set using 100, 200, and 500 for ntree
rf_model_500 <- randomForest(SMOIS ~ time, data = train_data1, ntree = 500)

# Display the Random Forest model summary
summary(rf_model_500)

# Predict SMOIS values for the test set using the Random Forest model
rf_predictions_500 <- predict(rf_model_500, newdata = test_data1)

# Calculate the root mean squared error (RMSE) for the Random Forest model
rf_rmse_500 <- sqrt(mean((test_data1$SMOIS - rf_predictions_500)^2))
cat("Random Forest RMSE_500:", rf_rmse_500)


# Compare the RMSE values of linear regression, SVR, RF, and Arima.
cat("\nLinear Regression RMSE:", rmse)
cat("SVR RMSE RADIAL:", svr_rmse_radial)
cat("SVR RMSE LINEAR:", svr_rmse_linear)
cat("SVR RMSE POLY:", svr_rmse_poly)
cat("Random Forest RMSE 100:", rf_rmse_100)
cat("Random Forest RMSE_200:", rf_rmse_200)
cat("Random Forest RMSE_500:", rf_rmse_500)
cat("ARIMA Model Accuracy", arima_accuracy[, 'RMSE'])

# Plot the actual vs. predicted values for the Linear Regression, SVR, and Random Forest models
# LR and SVR has been plotted
p3 <- ggplot() +
  geom_point(data = test_data1, aes(x = SMOIS, y = rf_predictions), color = "yellow") +
  geom_abline(slope = 1, intercept = 0, color = "pink") +
  labs(title = "Random Forest: Actual vs. Predicted SMOIS",
       x = "Actual SMOIS",
       y = "Predicted SMOIS") +
  theme_minimal()

grid.arrange(p1, p2, p3, p4, ncol = 4)


# to compare in graphical illustration of RMSE among all models
result <- data.frame(rmse_arima, rmse_LR,  svr_rmse_poly, rf_rmse_200)
barplot(as.matrix(result), beside = TRUE, col = c( "black", "pink", "purple", "yellow"), 
        ylab = "RMSE", xlab = "Model", main = "Comparison of RMSE between Models")


