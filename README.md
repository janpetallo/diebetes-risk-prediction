# Diabetes Risk Prediction

This project uses machine learning models in R to predict the risk of diabetes based on health indicators from survey data.

## Dataset

- **Source:** [Diabetes Health Indicators Dataset (Kaggle)](https://www.kaggle.com/datasets/julnazz/diabetes-health-indicators-dataset/data)  
  Based on the CDC Behavioral Risk Factor Surveillance System (BRFSS) 2021 survey.  
  Features include age, BMI, blood pressure, cholesterol, physical activity, and more.

## Overview

- **Goal:** Predict diabetes (or prediabetes) risk using logistic regression and gradient boosting (GBM).
- **Language:** R

## Workflow

1. **Data Loading & Exploration**
    - Load and inspect the dataset.
    - Visualize variables and check for missing values.
2. **Data Partitioning**
    - Split data into training (70%) and test (30%) sets.
3. **Modeling**
    - Logistic Regression (baseline)
    - Gradient Boosting Machine (GBM, tuned for better accuracy)
4. **Evaluation**
    - Confusion matrices, accuracy, ROC curves, and AUC.
    - Thresholds optimized for sensitivity and specificity.
5. **Feature Importance**
    - Top predictors: High blood pressure, general health, BMI, age, cholesterol, walking difficulty.

## Results

- **Best GBM Model:**  
  - Accuracy: ~74%  
  - AUC: ~0.82
- **Best Logistic Regression:**  
  - Accuracy: ~73%  
  - AUC: ~0.82

## Files

- `diabetes_prediction.R`: Main R script with data analysis, modeling, and evaluation.
- `diabetes_binary_health_indicators_BRFSS2021.csv`: Dataset.
- `Project Report.pdf`: Detailed report.
- `Presentation slides.pdf`: Project presentation.

## How to Run

1. Clone this repository.
2. Open `diabetes_prediction.R` in R or RStudio.
3. Install required packages:
    ```r
    install.packages(c("data.table", "ggplot2", "gbm", "pROC", "ROCR", "dplyr"))
    ```
4. Set your working directory and run the script.

## References

- [Diabetes Health Indicators Dataset (Kaggle)](https://www.kaggle.com/datasets/julnazz/diabetes-health-indicators-dataset/data)
- [CDC BRFSS 2021 Data](https://www.cdc.gov/brfss/)
- [GBM R Package](https://cran.r-project.org/web/packages/gbm/)
- [ROCR R Package](https://cran.r-project.org/web/packages/ROCR/)
