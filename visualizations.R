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
