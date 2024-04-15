library(plotly)
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

#reading the csv file
data = read.csv("stroke_data.csv")
str(data)

#taking a glimse of the data to know data points of first few rows
glimpse(data)

# Data Pre-processing

# Checking dataset values
unique(data $ gender)
unique(data $ ever_married) 
unique(data $ Residence_type)
unique(data $ smoking_status)

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
summary(clean_data)

#summarizing data after preprocessing

summary(clean_data)



# Calculate Entropy for Each Column in clean_data (data after preprocessing)

entropy_values <- sapply(clean_data, function(col) entropy(col))


# Calculate Variance for Each Column in clean_data (data after preprocessing)

variance_values <- sapply(clean_data, function(col) var(col))

# Calculate Standard Deviation for Each Column in clean_data (data after preprocessing)

std_deviation_values <- sapply(clean_data, function(col) sd(col))

# Calculate Impurity for Each Column in clean_data (data after preprocessing)

impurity_values <- sapply(clean_data, function(col) entropy(col))


# Print Results

cat("Entropy values for each column:\n")

print(entropy_values)

cat("\nVariance for each column:\n")

print(variance_values)

cat("\nStandard Deviation for each column:\n")

print(std_deviation_values)

cat("\nImpurity values for each column:\n")

print(impurity_values)



# Calculate Quartile Range for Each Column in clean_data

quartile_ranges <- sapply(clean_data, function(col) {
  q <- quantile(col, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  return(iqr)
})

# Print Quartile Ranges

cat("Quartile Ranges for each column:\n")
print(quartile_ranges)


#Information gain

# Remove rows with missing values in 'bmi' and 'smoking_status' columns

clean_data_no_na <- clean_data[!is.na(clean_data$bmi) & !is.na(clean_data$smoking_status), ]

# Function to calculate information gain

calculate_information_gain <- function(data, target_variable) {
  info_gain <- sapply(names(data), function(feature) {
    if (feature != target_variable) {
      # Calculate entropy of the entire dataset
      
      entropy_full <- entropy(data[[target_variable]])
      
      # Calculate entropy after splitting by the current feature
      
      entropy_split <- sapply(unique(data[[feature]]), function(category) {
        subset_data <- data[data[[feature]] == category, ]
        proportion <- nrow(subset_data) / nrow(data)
        entropy(subset_data[[target_variable]]) * proportion
      })
      
      # Calculate information gain
      
      information_gain <- entropy_full - sum(entropy_split, na.rm = TRUE)
      return(information_gain)
    }
  })
  return(info_gain)
}

# Calculate information gain for each feature in clean_data with missing values removed

information_gains_no_na <- calculate_information_gain(clean_data_no_na, "stroke")

# Print information gains for each feature

cat("Information Gain for each feature (after removing NA values):\n")

print(information_gains_no_na)



# Visualizing the input


# BMI Distribution Density Plot

ggplot(clean_data, aes(x = bmi)) + geom_density(color = "black", fill = "lightblue") + labs(title = "Distribution of BMI") 

# Gender Distribution Bar Plot

ggplot(data, aes(x = factor(gender), fill = factor(gender))) + geom_bar() + theme_classic()

# Age and BMI wrt Stroke Scatter Plot

ggplot(clean_data, aes(x = age, y = bmi, color = stroke)) + geom_point() + scale_color_gradient(low = "lightblue", high = "red")



# Avg Glucose Level with stroke boxplot

ggplot(clean_data, aes(x = stroke, y = avg_glucose_level, group = stroke, fill = stroke)) + geom_boxplot()
glimpse(clean_data)



par(las = 2)
par(las=1)
par(mar = c(5, 9, 4, 4))  

correlation_df <- data.frame(variable = names(correlations), correlation = correlations)

correlation_df$variable <- ifelse(correlation_df$variable == "avg_glucose_level", "avg_glucose", correlation_df$variable)
barplot(correlation_df$correlation, names.arg = correlation_df$variable, col = ifelse(correlation_df$correlation > 0, "blue", "lightcoral"), horiz = TRUE, main = "Correlation with Stroke", xlab = "Correlation Coefficient", ylab = "",xlim = c(0, 1),border = NA)
abline(v = 0, col = "black", lty = 1, lwd = 2)

mtext("Variables", side = 2, line = 7.5, at = 5, cex = 1,las = 3)

legend("topright", legend = c("Positive", "Negative"), fill = c("blue", "lightcoral"))

with(clean_data, {
  colors <- ifelse(stroke == 1, "red", "blue")  # Assuming stroke = 1 for stroke cases and 0 for non-stroke cases
  scatterplot3d(
    x = age,  y = hypertension, z = avg_glucose_level,
    color = colors,
    main = "Stroke Prediction Scatterplot",
    xlab = "Age", ylab = "Hypertension", zlab = "Average Glucose Level",
    xlim = c(min(age), max(age)),
    ylim = c(0, 1), # Assuming hypertension is a binary variable (0 or 1)
    zlim = c(min(avg_glucose_level), max(avg_glucose_level))
  )
})


