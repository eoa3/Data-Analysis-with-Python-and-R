# Import dataset
View(WRF)

# Examining data structure and summary statistics
str(WRF)

#checking for missing values
sum(is.na(WRF))

# Handling missing values
install.packages("imputeTS")
library(imputeTS)

# fill in missing values using interpolation method
my_data <- na_locf(WRF)

# checking to find if all missing values has been filled
sum(is.na(my_data))

#save clean data to csv
write.csv(my_data, "my_data.csv", row.names = FALSE)

# visualizing outliers using Boxplot
boxplot(my_data$SMOIS,  main = "Boxplot of SMOIS", xlab = 'SMOIS')

boxplot(my_data$TSLB, my_data$SMOIS, main = "Boxplot of TSLB vs SMOIS", xlab = 'TSLB', ylab = 'SMOIS')
plot(my_data$TSLB, my_data$SMOIS, main = "Scatterplot of TSLB vs SMOIS", xlab = 'TSLB', ylab = 'SMOIS')


# checking for outlier using z-score method
data <- my_data$SMOIS
z_scores <- scale(data)
threshold <- 2
outliers <- data[abs(z_scores) > threshold]
print(outliers)

# handling outliers using Transformation technique
data <- my_data$SMOIS
log_transformed_data <- log(data)
print(log_transformed_data)

# Transforming dataset using Scaling Transformation
#min-max scaling
min_SMOIS <- min(my_data$SMOIS)
max_SMOIS <- max(my_data$SMOIS)
my_data$scaled_SMOIS <- (my_data$SMOIS - min_SMOIS) / (max_SMOIS - min_SMOIS)
my_data$scaled_SMOIS

# correlation analysis
corrs <- cor(my_data[, 3:12], use = "complete.obs")
ggcorrplot(corrs, type = "lower", outline.color = "white", lab = TRUE)
print(corrs)


# univariate analysis
# using histogram to visualise
# select margins for proper plotting
par(mfrow = c(3, 2)) 
par(mar = c(4, 4, 2, 2))

par(mfrow = c(3, 2))
# Define a function to generate histograms
hist_fun <- function(col, name) {
  hist(col, main = paste("Histplot of", name), col = "blue")
}

# Use apply to generate list of histograms
histograms <- lapply(names(my_data)[3:8], function(name) hist_fun(my_data[[name]], name))
par(mfrow = c(2, 2))
histograms <- lapply(names(my_data)[9:12], function(name) hist_fun(my_data[[name]], name))

