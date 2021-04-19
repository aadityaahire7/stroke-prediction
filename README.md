# Stroke Prediction

## Problem Definition

A stroke occurs when a blood vessel in the brain ruptures and bleeds, or when there’s a blockage in the blood supply to the brain. The rupture or blockage prevents blood and oxygen from reaching the brain’s tissues. Without oxygen, brain cells and tissue become damaged and begin to die within minutes.

According to the World Health Organization (WHO) stroke is the 2nd leading cause of death globally, responsible for approximately 11% of total deaths. This project is used to predict whether a patient is likely to get stroke based on the input parameters like gender, age, various diseases, and smoking status.

View the project [live](https://adnanhakim.github.io/stroke-prediction/)

## Technology Stack

- R Language and R markdown

## Dataset

The dataset can be found in the [repository](https://github.com/adnanhakim/stroke-prediction) or can be downloaded from [Kaggle](https://www.kaggle.com/fedesoriano/stroke-prediction-dataset)

The dataset contains 5110 real world observations and 10 different attributes:
1. gender: "Male", "Female" or "Other"
2. age: age of the patient
3. hypertension: 0 if the patient doesn't have hypertension, 1 if the patient has hypertension
4. heart_disease: 0 if the patient doesn't have any heart diseases, 1 if the patient has a heart disease
5. ever_married: "No" or "Yes"
6. Residence_type: "Rural" or "Urban"
7. avg_glucose_level: average glucose level in blood
8. bmi: body mass index
9. smoking_status: "formerly smoked", "never smoked", "smokes" or "Unknown"*
10. stroke: 1 if the patient had a stroke or 0 if not

## Features of System

- The system uses **data pre-processing** to handle character values as well as null values.
- The system uses a **70-30** training-testing split.
- The system uses **Logistic Regression**: Logistic Regression is a regression model in which the response variable (dependent variable) has categorical values such as True/False or 0/1. It actually measures the probability of a binary response as the value of response variable based on the mathematical equation relating it with the predictor variables.
- The system uses efficient and effective **visualization graphs** which help identify and understand important factors for stroke.

## Working of System

Logistic Regression
- Input: The dataset
- Output: Classification into 0 (no stroke) or 1 (stroke)

Steps:
1. Loading the dataset and required packages
2. Pre-processing data to convert character to numeric and to remove null values
3. Dividing the dataset into training set and test set
4. Importing the Logistic Regression classifier and creating its object.
5. Fitting the training data to the classifier
6. Predicting the classifier output against the test data
7. Comparing the predicted results with the test results to get the accuracy

## Developers

-  Adnan Hakim [github.com/adnanhakim](https://github.com/adnanhakim)
-  Arsh Shaikh [github.com/arshshaikh06](https://github.com/arshshaikh06) 
-  Hussain Sadriwalla [github.com/hussainf46](https://github.com/hussainf46)

## LICENSE

> Copyright 2021 Adnan Hakim, Arsh Shaikh, Hussain Sadriwalla
>
>Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
>
>    http://www.apache.org/licenses/LICENSE-2.0
>
>Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
