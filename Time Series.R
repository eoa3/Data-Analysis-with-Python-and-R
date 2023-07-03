# load the neccessary libraries
library(forecast)
library(tidyverse)
library(tseries)
library(ggplot2)
library(dplyr)
library(lubridate)

# import dataset
View(values)

# create a dataframe with 10 columns
new_values <- data.frame(matrix(values, ncol = 10, byrow = TRUE))
View(new_values)

# rename the columns
colnames(new_values) <- c("TSK", "PSFC", "U10", "V10", "Q2", "RAINC", "RAINNC", "SNOW", "TSLB", "SMOIS")
new_values <- as.data.frame(new_values)

# import dataset
View(headers)

# create a dataframe with 10 columns
new_headers <- data.frame(matrix(headers, ncol = 10, byrow = TRUE))
View(new_headers)

# rename the first column as Datetime
names(new_headers)[1] <- "Datetime"
Date_time <- new_headers[,1]
View(Date_time)
Date_time <- data.frame(matrix(Date_time, ncol = 1, byrow = TRUE))
names(Date_time)[1] <- "Datetime"

# to see both table side by side
new_df <- as.data.frame(cbind(Date_time, new_values))
View(new_df)

# handling datetime column

# assuming datetime column is named 'datetime_col'
new_df$Datetime <- dmy_hm(gsub("^X", "", new_df$Datetime))

colSums(is.na(new_df))

# Find maximum datetime without NA values
max_Datetime <- max(new_df$Datetime[!is.na(new_df$Datetime)])

# Fill NA values with maximum datetime plus 3 hours
new_df$Datetime[is.na(new_df$Datetime)] <- max_Datetime + hours(3)

colSums(is.na(new_df))
View(new_df)

detach("package:lubridate", unload = T)

head(new_df)
tail(new_df)
dim(new_df)
str(new_df)

psych::describe(new_df)

# retain columns i need
data <- new_df %>% select(1, 11)

# view new dataset
View(data)

# convert the datetime column to a proper date and time R can understand
# Convert Datetime column to a date/time format
data$Datetime <- as.POSIXct(data$Datetime, format = "%Y-%m-%d %H:%M")

# create separate columns for year, month, day, and hour
data$Year <- as.numeric(format(data$Datetime, "%Y"))
data$Month <- as.numeric(format(data$Datetime, "%m"))
data$Day <- as.numeric(format(data$Datetime, "%d"))
data$Hour <- as.numeric(format(data$Datetime, "%H"))
View(data)

# remove unneccesary columns
data <- data %>% select(-Datetime)
View(data)

# determine the no. of hours in the dataset 
n_hours <- nrow(data)

# convert dataset to a time series object
# to perform univariate time series analysis based on my dataset
set.seed(123)  # for reproducibility 
data_ts <- ts(data$SMOIS, start = c(2018, 1), frequency = 8*31)
View(data_ts)

# split the dataset into training and testing sets using 80/20 model
set.seed(123)
train_end_idx <- which(data$Year == 2018 &
                         data$Month == 5 &
                         data$Day == 24 &
                         data$Hour%% 3 == 0 &
                         data$Hour >= 0 &
                         data$Hour <= 21)[1]


# split the dataset into training and testing
train_data <- data_ts[1:train_end_idx]
test_data <- data_ts[(train_end_idx + 1):n_hours]

# view train and test data to ensure they are both numeric and contain no missing values
train_data <- na.omit(as.numeric(train_data))
test_data <- na.omit(as.numeric(test_data))

# fit an ARIMA model to the training data
arima_model <- auto.arima(train_data)
summary(arima_model)
# print
print(arima_model)

# forecast and evaluate using the ARIMA model
arima_forecast <- forecast(arima_model, h = length(test_data))
# evaluate
arima_accuracy <- accuracy(arima_forecast, test_data)
cat("ARIMA Model Accuracy", arima_accuracy[, 'RMSE'])

# check residuals for model diagnosis 
checkresiduals(arima_model)

# Standard Regression
# i will be evaluating the performance of the ARIMA model using the Mean Absolute Error (MAE) 
# and Root Mean Squared Error (RMSE) metrics
# calculate MAE and RMSE
mae_arima <- mean(abs(test_data - arima_forecast$mean))
rmse_arima <- sqrt(mean((test_data - arima_forecast$mean)^2))

# display the performance metrics
cat("ARIMA MODEL: MAE =", mae_arima, ", RMSE =", rmse_arima, "\n")

# create a dataframe to show the actual and predicted values
arima_df <- data.frame(test_data, arima_forecast$mean, row.names = 1:length(test_data))

# plot actual and predicted values
p4 <- 
  ggplot() +
  geom_point(data = arima_df, aes(x = test_data, y = arima_forecast.mean), color = "orange") +
  geom_abline(slope = 1, intercept = 0, color = "green") +
  labs(title = "Arima Model: Actual vs. Predicted TSK",
       x = "Actual SMOIS",
       y = "Predicted SMOIS") +
  theme_minimal()
