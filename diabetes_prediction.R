

# ---------------------------------
# BTMA 531 Project
# ---------------------------------

# -----------------------
# Load the data
# -----------------------

library(data.table)
data <- fread("diabetes_binary_health_indicators_BRFSS2021.csv", header = TRUE,
            colClasses = c("factor", "factor", "factor", "factor", "numeric", 
                           "factor", "factor", "factor", "factor", "factor", 
                           "factor", "factor", "factor", "factor", "factor", 
                           "numeric", "numeric", "factor", "factor", "factor", 
                           "factor", "factor"))



# -----------------------
# EDA
# -----------------------

str(data)
summary(data)

# Count missing values per column
colSums(is.na(data))  


library(ggplot2)

ggplot(data, aes(x = Diabetes_binary)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Diabetes Cases",
       x = "0 = No Diabetes, 1 = Prediabetes or Diabetes", y = "Count")

## Histograms of key numeric features

# BMI
ggplot(data, aes(x = BMI)) +
  geom_histogram(binwidth = 1, fill = "darkgreen") +
  labs(title = "Distribution of BMI", x = "BMI", y = "Count")

# Mental Health Days
ggplot(data, aes(x = MentHlth)) +
  geom_histogram(binwidth = 1, fill = "purple") +
  labs(title = "Distribution of Mental Health Days", x = "MentHlth", y = "Count")

# Physical Health Days
ggplot(data, aes(x = PhysHlth)) +
  geom_histogram(binwidth = 1, fill = "orange") +
  labs(title = "Distribution of Physical Health Days", x = "PhysHlth", y = "Count")


## Boxplots comparing diabetes vs key variables

# BMI vs. Diabetes
ggplot(data, aes(x = Diabetes_binary, y = BMI, fill = Diabetes_binary)) +
  geom_boxplot() +
  labs(title = "BMI by Diabetes Status", x = "0 = No Diabetes, 1 = Prediabetes or Diabetes", y = "BMI") +
  theme(legend.position = "none")

# Mental Health Days vs Diabetes
ggplot(data, aes(x = Diabetes_binary, y = MentHlth, fill = Diabetes_binary)) +
  geom_boxplot() +
  labs(title = "Mental Health Days by Diabetes Status", x = "0 = No Diabetes, 1 = Prediabetes or Diabetes", y = "MentHlth") +
  theme(legend.position = "none")

# Physical Health Days vs Diabetes
ggplot(data, aes(x = Diabetes_binary, y = PhysHlth, fill = Diabetes_binary)) +
  geom_boxplot() +
  labs(title = "Physical Health Days by Diabetes Status", x = "0 = No Diabetes, 1 = Prediabetes or Diabetes", y = "PhysHlth") +
  theme(legend.position = "none")


# Q-Q Plot for BMI (normality check)
qqnorm(data$BMI, main = "Q-Q Plot of BMI")
qqline(data$BMI, col = "red")

# Diabetes by HighBP (proportions)
ggplot(data, aes(x = HighBP, fill = Diabetes_binary)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Diabetes by High Blood Pressure", y = "Proportion")

# Correlation between numeric variables
numeric_vars <- data[, c("BMI", "MentHlth", "PhysHlth")]
cor(numeric_vars)


# -----------------------
# Data Partitioning
# -----------------------

## Divide the dataset into two:
### 70% for training set and 30% test set

set.seed(2025)
training.index <-  sample(nrow(data), size= floor(0.7 *nrow(data)),replace=FALSE)
training.set <- data[training.index, ]
test.set <- data[-training.index, ]



# -----------------------
# Logistic Regression
# -----------------------



# build the model
logistic.model <- glm(formula = Diabetes_binary ~ ., data = training.set,
                      family = binomial)

summary(logistic.model)


# use the model to predict the test set
# ensure it returns probabilities instead of log-odds
probabilities.test.logistic <- predict(logistic.model, test.set, type = "response")

# convert actual labels to numeric 0/1
actual.test.logistic <- as.numeric(as.character(test.set$Diabetes_binary))


## Find the best threshold

# initialize a data frame to store results
model.performance.logistic <- data.frame(
  Threshold = numeric(),
  Accuracy = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric()
)

# for loop to try thresholds from 0 to 1 by 0.05
for (threshold in seq(0, 1, by = 0.05)) {
  
  # create vector of 0s
  predict.test.set <- rep(0, length(probabilities.test.logistic))
  
  # assign 1 if probability > threshold
  predict.test.set[probabilities.test.logistic > threshold] <- 1
  
  # confusion matrix values
  TP <- sum(predict.test.set == 1 & actual.test.logistic == 1)
  TN <- sum(predict.test.set == 0 & actual.test.logistic == 0)
  FP <- sum(predict.test.set == 1 & actual.test.logistic == 0)
  FN <- sum(predict.test.set == 0 & actual.test.logistic == 1)
  
  # metrics
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  
  # store results
  model.performance.logistic <- rbind(model.performance.logistic, data.frame(
    Threshold = threshold,
    Accuracy = accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity
  ))
}


model.performance.logistic

# we choose threshold 0.15 to balance sensitivity and specificity

# calculate predicted 0 or 1 ( for diabetes_binary) based on the probabilities
predict.test.logistic <- rep(0, nrow(test.set)) # make everything 0, initially

# use threshold 0.15
# if probability > 0.15, then it is 1
predict.test.logistic[probabilities.test.logistic > 0.15] <- 1

## Accuracy
mean(actual.test.logistic == predict.test.logistic) 
## 0.7313507

table(Predicted = predict.test.logistic, Actual = actual.test.logistic)
##              Actual
# Predicted     0     1
#         0 44231  2493
#         1 16558  7632

# when threshold is 0.5
##              Actual
# Predicted     0     1
#         0 59601  8618
#         1  1118  1507



library(ROCR)

# find the tradeoff between accuracy and cutoff
pred.logistic <- prediction(probabilities.test.logistic, actual.test.logistic)
perf.logistic <- performance(pred.logistic, "acc")
plot(perf.logistic)

# Create ROC curve
roc.logisitic <- performance(pred.logistic, "tpr", "fpr")

# Plot ROC curve
plot(roc.logisitic, lwd = 2, main = "ROC Curve for Logistic Regression Model (ROCR)")
abline(a = 0, b = 1)

# Calculate AUC using ROCR
auc.logistic <- performance(pred.logistic, "auc")

# Extract and print AUC value
auc.logistic@y.values[[1]]
# 0.8173404



# -----------------------
# Gradient Boosting Machine (GBM)
# -----------------------

# GBM requires that target variable is numeric
training.set$Diabetes_binary <- as.numeric(as.character(training.set$Diabetes_binary))
test.set$Diabetes_binary <- as.numeric(as.character(test.set$Diabetes_binary))

library(gbm)

## Tune the model (a long time to process)

# based on http://uc-r.github.io/gbm_regression:
# create hyperparameter grid, choosing the parameters that we want to test
hyper_grid <- expand.grid(
  shrinkage = c(0.01, 0.05, 0.1),
  interaction.depth = c(1, 3, 5),
  # n.minobsinnode = c(5, 10, 15),
  # bag.fraction = c(0.65, 0.8, 1),
  optimal_trees = 0,
  max_auc = 0
)


library(pROC) 
set.seed(2025)

for (i in 1:nrow(hyper_grid)) {
  model <- gbm(
    formula = Diabetes_binary ~ .,
    distribution = "bernoulli",
    data = training.set,
    n.trees = 1000,  
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    # n.minobsinnode = hyper_grid$n.minobsinnode[i],
    # bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = 0.8,
    cv.folds = 5,
    verbose = FALSE
  )
  
  # Store best iteration and AUC
  best.iteration <- gbm.perf(model, method = "cv", plot.it = FALSE)
  
  # Predict on validation set (20% left out during training)
  probabilities <- predict(model, training.set, n.trees = best.iteration, type = "response")
  actual <- training.set$Diabetes_binary
  auc.score <- auc(actual, probabilities)
  
  hyper_grid$optimal_trees[i] <- best.iteration
  hyper_grid$max_auc[i] <- auc.score
}


library(dplyr)

hyper_grid %>%
  arrange(desc(max_auc)) %>%
  head(10)

#       shrinkage   interaction.depth  optimal_trees   max_auc
# 1      0.05                 5           587         0.8256935
# 2      0.10                 5           298         0.8253184
# 3      0.05                 3          1000         0.8245348
# 4      0.10                 3           465         0.8240428
# 5      0.01                 5          1000         0.8221401
# 6      0.01                 3          1000         0.8201834
# 7      0.10                 1           847         0.8189938
# 8      0.05                 1           962         0.8188932
# 9      0.01                 1          1000         0.8135349


## Using the best parameters for the final model (ranked first)
  
set.seed(2025)
gbm.model <- gbm(
            formula = Diabetes_binary ~ .,
            distribution = "bernoulli",
            data = training.set,
            n.trees = 587,               # Best number of trees
            interaction.depth = 5,       # Best depth
            shrinkage = 0.05,            # Best learning rate
            cv.folds = 5,
            verbose = FALSE
)

# Adjust plot margins to avoid cutting off labels
par(mar = c(5, 10, 4, 2))  # bottom, left, top, right

# Now plot the variable importance with enough space for labels
summary(gbm.model, las = 2, method = relative.influence)

par(mar = c(5.1, 4.1, 4.1, 2.1))

# feature importance/relevance
summary(gbm.model, las = 2,  method = relative.influence)
#                                       var     rel.inf
# HighBP                             HighBP 25.63757316
# GenHlth                           GenHlth 23.50685756
# BMI                                   BMI 12.91620577
# Age                                   Age 11.54746043
# HighChol                         HighChol  7.01927507
# DiffWalk                         DiffWalk  4.23023423
# HeartDiseaseorAttack HeartDiseaseorAttack  3.79309674
# Income                             Income  3.43391386
# PhysActivity                 PhysActivity  1.52405962
# HvyAlcoholConsump       HvyAlcoholConsump  1.21505889
# Sex                                   Sex  1.12726071
# Education                       Education  0.91493352
# PhysHlth                         PhysHlth  0.74395650
# CholCheck                       CholCheck  0.62981918
# MentHlth                         MentHlth  0.57532416
# Stroke                             Stroke  0.54374172
# Smoker                             Smoker  0.23391249
# NoDocbcCost                   NoDocbcCost  0.11591412
# AnyHealthcare               AnyHealthcare  0.10974113
# Fruits                             Fruits  0.10530955
# Veggies                           Veggies  0.07635158

# use the model to predict the test set
# ensure it returns probabilities instead of log-odds
probabilities.test.gbm <- predict(gbm.model, test.set, n.trees = 587,
                                  type = "response")


# convert actual labels to numeric 0/1
actual.test.gbm <- test.set$Diabetes_binary

# initialize a data frame to store results
model.performance.gbm <- data.frame(
  Threshold = numeric(),
  Accuracy = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric()
)

# for loop to try thresholds from 0 to 1 by 0.05
for (threshold in seq(0, 1, by = 0.05)) {
  
  # create vector of 0s
  predict.test.set <- rep(0, length(probabilities.test.gbm))
  
  # assign 1 if probability > threshold
  predict.test.set[probabilities.test.gbm > threshold] <- 1
  
  # confusion matrix values
  TP <- sum(predict.test.set == 1 & actual.test.gbm == 1)
  TN <- sum(predict.test.set == 0 & actual.test.gbm == 0)
  FP <- sum(predict.test.set == 1 & actual.test.gbm == 0)
  FN <- sum(predict.test.set == 0 & actual.test.gbm == 1)
  
  # metrics
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  
  # store results
  model.performance.gbm <- rbind(model.performance.gbm, data.frame(
    Threshold = threshold,
    Accuracy = accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity
  ))
}


model.performance.gbm
# the balance between sensitivity and specificity is threshold 0.15
# Threshold  Accuracy  Sensitivity Specificity
# 1       0.00 0.1427786 1.0000000000   0.0000000
# 2       0.05 0.5137067 0.9492345679   0.4411653
# 3       0.10 0.6597005 0.8498765432   0.6280248
# 4       0.15 0.7381053 0.7483456790   0.7363997
# 5       0.20 0.7848380 0.6469135802   0.8078106
# 6       0.25 0.8157063 0.5537777778   0.8593331
# 7       0.30 0.8349832 0.4633086420   0.8968892
# 8       0.35 0.8481259 0.3796543210   0.9261544
# 9       0.40 0.8560651 0.3000493827   0.9486749
# 10      0.45 0.8612827 0.2312098765   0.9662274
# 11      0.50 0.8628057 0.1676049383   0.9785981
# 12      0.55 0.8627069 0.1094320988   0.9881722
# 13      0.60 0.8611558 0.0619259259   0.9942753
# 14      0.65 0.8594072 0.0308148148   0.9974173
# 15      0.70 0.8580816 0.0131358025   0.9988156
# 16      0.75 0.8573483 0.0036543210   0.9995394
# 17      0.80 0.8572778 0.0008888889   0.9999177
# 18      0.85 0.8572073 0.0000000000   0.9999835
# 19      0.90 0.8572214 0.0000000000   1.0000000
# 20      0.95 0.8572214 0.0000000000   1.0000000
# 21      1.00 0.8572214 0.0000000000   1.0000000

# calculate predicted 0 or 1 ( for diabetes_binary) based on the probabilities
predict.test.gbm <- rep(0, nrow(test.set))  # placeholder, bunch of 0s (false)

# convert using 0.15 threshold
predict.test.gbm[probabilities.test.gbm > 0.15] <- 1


## Accuracy
mean(actual.test.gbm == predict.test.gbm) 
# 0.7381053

# confusion matrix
table(Predicted = predict.test.gbm, Actual = actual.test.gbm)
#               Actual
# Predicted     0     1
#         0 44765   2548
#         1 16024   7577


# install.packages("ROCR")
library(ROCR)

# Create a prediction object for ROCR
pred.gbm <- prediction(probabilities.test.gbm, actual.test.gbm)
perf.gbm <- performance(pred.gbm, "acc")
plot(perf.gbm)

# Create ROC curve
roc.gbm <- performance(pred.gbm, "tpr", "fpr")

# Plot ROC curve
plot(roc.gbm, lwd = 2, main = "ROC Curve for GBM Model (ROCR)")
abline(a = 0, b = 1)

# Calculate AUC using ROCR
auc.gbm <- performance(pred.gbm, "auc")

# Extract and print AUC value
auc.gbm@y.values[[1]]
# 0.8208132


plot(model.performance.logistic$Threshold, model.performance.logistic$Sensitivity)
plot(model.performance.gbm$Threshold, model.performance.gbm$Sensitivity)


