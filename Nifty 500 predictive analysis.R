# Load necessary libraries
library(ggplot2)
library(lattice)
library(caret)
library(dplyr)

# Load the dataset
data <- read.csv("C:/Users/vishu/Downloads/NIFTY 500 Quarterly Result.csv")

# Preprocess the data: Convert columns to numeric where necessary
data$revenue <- as.numeric(gsub(",", "", data$revenue))
data$operating_expenses <- as.numeric(gsub(",", "", data$operating_expenses))
data$net_profit <- as.numeric(data$net_profit)
data$EPS <- as.numeric(data$EPS)

# Remove rows with missing values
data <- na.omit(data)

# View structure
str(data)

## Set seed for reproducibility
set.seed(123)

# Check for missing or NaN values in EPS
sum(is.na(data$EPS))   # Count of NA values
sum(is.nan(data$EPS))  # Count of NaN values

# Remove rows with missing values in EPS
data <- data[!is.na(data$EPS), ]

data <- data[!is.na(data$EPS), ]
data$EPS <- as.numeric(data$EPS)
nrow(data)

# Split the data into training (80%) and testing (20%) sets
set.seed(123)
trainIndex <- sample(1:nrow(data), size = 0.8 * nrow(data))
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

#Build Predictive Models

# Fit a linear regression model
linear_model <- lm(EPS ~ revenue + net_profit + operating_expenses, data = train_data)

# Summary of the model
summary(linear_model)

# Convert revenue to numeric (remove commas)
data$revenue <- as.numeric(gsub(",", "", as.character(data$revenue)))

# Check for NAs introduced during conversion
sum(is.na(data$revenue))  # If >0, there were invalid values
data <- data[!is.na(data$revenue), ]

# Predict on the test set
linear_predictions <- predict(linear_model, newdata = test_data)

# Create a binary target variable
median_eps <- median(train_data$EPS)
train_data$high_eps <- ifelse(train_data$EPS > median_eps, 1, 0)
test_data$high_eps <- ifelse(test_data$EPS > median_eps, 1, 0)

# Fit a logistic regression model
logistic_model <- glm(high_eps ~ revenue + net_profit + operating_expenses, data = train_data, family = "binomial")

# Summary of the model
summary(logistic_model)

# Predict probabilities on the test set
logistic_probabilities <- predict(logistic_model, newdata = test_data, type = "response")
logistic_predictions <- ifelse(logistic_probabilities > 0.5, 1, 0)

