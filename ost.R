install.packages("plotly")
install.packages("tensorflow")
install.packages("xgboost")
install.packages("pROC")
install.packages("tidyverse")
install.packages("naniar")
install.packages("caTools")
install.packages("ggplot2")
install.packages("superheat")
install.packages("scatterplot3d")
install.packages("ROCR")
install.packages("Metrics")
install.packages("keras")
install.packages("caret")
install.packages("ROSE")
install.packages("reshape2")
install.packages("magrittr")
install.packages("dplyr")



# Importing packages
library(plotly)
library(tensorflow)
library(xgboost)
library(pROC)
library(tidyverse)
library(naniar)
library(caTools) 
library(ggplot2) 
library(superheat) 
library(scatterplot3d) 
library(ROCR)
library(Metrics)
library(keras)
library(caret)
library(ROSE)
library(reshape2)
library(magrittr)
library(dplyr)


#reading the csv file
data = read.csv("stroke_data.csv")
str(data)



# Converting character values to numeric values

clean_data <- data %>% mutate(gender = if_else(gender == "Female", 0, if_else(gender == "Male", 1, 2)), ever_married = if_else(ever_married == "Yes", 1, 0), Residence_type = if_else(Residence_type == "Rural", 0, 1), smoking_status = if_else(smoking_status == "never smoked", 0, if_else(smoking_status == "formerly smoked", 1, if_else(smoking_status == "smokes", 2, 3))))
summary(clean_data)
glimpse(clean_data)

# Handling missing values

miss_scan_count(data = data, search = list("N/A", "Unknown"))

# There are 201 "N/A" values in the bmi column that likely caused this column 
# to be parsed as character, although it should be numerical.   
#  replacing those values with actual NAs. 
# lot of "Unknown" values in smoking_status  
#  We see that we have 1544 unknown values for smoking status and 
# replace those values with 
# NAs.

clean_data <- replace_with_na(data = clean_data, replace = list(bmi = c("N/A"), smoking_status = c(3))) %>% mutate(bmi = as.numeric(bmi))



# Split the data into training and testing sets
set.seed(99)  # Set a seed for reproducible results always producing same random result
split <- sample.split(clean_data$stroke, SplitRatio = 0.8)
train <- subset(clean_data, split == TRUE)
test <- subset(clean_data, split == FALSE)

# Convert data to a format suitable for xgboost
dtrain <- xgb.DMatrix(data = as.matrix(train[, -1]), label = train$stroke)
dtest <- xgb.DMatrix(data = as.matrix(test[, -1]), label = test$stroke)

# Define and train the xgboost model , Parameter Tuning
xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.001491,  # Learning rate
  max_depth = 5,  # Maximum depth of trees
  subsample = 0.7,  # Fraction of training data used for each tree
  colsample_bytree = 0.7  # Fraction of features used for each tree
)
#xgboost Model Training
xgb_model <- xgboost(data = dtrain, params = xgb_params, nrounds = 915, early_stopping_rounds = 50, verbose = 1)

predict_test_xgb <- predict(xgb_model, dtest, type = "response")

predictions <- ifelse(predict_test_xgb > 0.699, 1, 0)
confusion_matrix <- table(test$stroke, predictions)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
# Find the threshold that maximizes accuracy


cat("Best Accuracy:", accuracy*100, "\n")
# Split the data into training and testing sets (e.g., 80% training, 20% testing)
set.seed(99)
index <- createDataPartition(clean_data$stroke, p = 0.8, list = FALSE)
train_data <- clean_data[index, ]
test_data <- clean_data[-index, ]

# Normalize numerical features
scaler <- preProcess(train_data[c("age", "avg_glucose_level", "bmi")], method = c("center", "scale"))
train_data[c("age", "avg_glucose_level", "bmi")] <- predict(scaler, train_data[c("age", "avg_glucose_level", "bmi")])
test_data[c("age", "avg_glucose_level", "bmi")] <- predict(scaler, test_data[c("age", "avg_glucose_level", "bmi")])

# Handle missing values (if any)
train_data[is.na(train_data)] <- 0
test_data[is.na(test_data)] <- 0
# Update the input shape in your LSTM layer to match your data

model <- keras_model_sequential()

model %>%
  layer_lstm(units = 50, input_shape = c(10, 9), return_sequences = TRUE) %>%
  layer_lstm(units = 50, return_sequences = TRUE) %>%
  layer_lstm(units = 50) %>%
  layer_dense(units = 1, activation = "sigmoid")

# Load additional libraries
library(keras)

# Define and train a neural network model using Keras
nn_model <- keras_model_sequential() %>%
  layer_dense(units = 128, activation = "relu", input_shape = n_features) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

# Compile the model
nn_model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adam(lr = 0.001),
  metrics = "accuracy"
)


# Train the model
nn_model %>% fit(
  x = as.matrix(train_data[, -10]),  # Features only
  y = train_data$stroke,
  epochs = 20,
  batch_size = 32
)

# Make predictions using the neural network model
nn_predictions <- nn_model %>% predict(as.matrix(test_data[, -10]))


# Combine predictions from all three models
ensemble_predictions_allthree <- (lstm_predictions + predictions + nn_predictions) / 3

# Calculate accuracy
ensemble_predictions_binary_allthree <- ifelse(ensemble_predictions_allthree > threshold, 1, 0)
true_labels <- test_data[(sequence_length):nrow(test_data), "stroke"]
ensemble_accuracy_allthree <- mean(ensemble_predictions_binary_allthree == true_labels)

# Print the accuracy
cat("Ensemble Model Accuracy (with three models): ", ensemble_accuracy_allthree * 100, "%\n")


library(class)

# Train the KNN model
knn_model <- knn(train = train_data[, -10], test = test_data[, -10], cl = train_data$stroke, k = 100)

# Convert predictions to binary
knn_predictions_binary <- ifelse(knn_model == "Yes", 1, 0)
# Combine predictions from all five models (XGBoost, LSTM, Neural Network, SVM, KNN)
ensemble_predictions_fivemodels <- (lstm_predictions + predictions + nn_predictions + knn_predictions_binary) / 4

# Calculate accuracy
ensemble_predictions_binary_fivemodels <- ifelse(ensemble_predictions_fivemodels > 0.4, 1, 0)
ensemble_accuracy_fivemodels <- mean(ensemble_predictions_binary_fivemodels == true_labels)

# Print the accuracy
cat("Ensemble Model Accuracy (with five models including KNN): ", ensemble_accuracy_fivemodels * 100, "%\n")
