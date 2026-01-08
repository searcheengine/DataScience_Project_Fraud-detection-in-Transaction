STEP 0: Install & Load Required Libraries
install.packages(c("tidyverse"))
install.packages(c("caret"))
install.packages(c("ROSE"))
# For older R versions
#install.packages("DMwR")
# Or for newer R versions
#install.packages("DMwR2")
install.packages("themis")
install.packages(c("pROC"))
install.packages(c("randomForest"))
install.packages(c("ggplot2"))

#1 Load the dataset
getwd()
data <- read.csv("creditcard.csv")
str(data)
head(data)

#2 Understand the Data
table(data$Class)
prop.table(table(data$Class))

#3 Data Cleaning
colSums(is.na(data))

#3.a Scale Amount & Time
data$Amount <- scale(data$Amount)
data$Time <- scale(data$Time)

#4 Exploratory Data Analysis (EDA)
library(ggplot2)
ggplot(data, aes(x = as.factor(Class))) +
  geom_bar(fill = "steelblue") +
  labs(x = "Class", y = "Count", title = "Fraud vs Legit Transactions")

#4.a Transaction Amount vs Fraud
library(ggplot2)

ggplot(data, aes(x = factor(Class), y = Amount, fill = factor(Class))) +
  geom_boxplot(outlier.colour = "red", outlier.size = 1.5) +
  scale_fill_manual(values = c("skyblue", "orange")) +
  labs(
    title = "Transaction Amount by Class",
    x = "Transaction Class (0 = Legit, 1 = Fraud)",
    y = "Transaction Amount (Scaled)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

#5 rain-Test Split
set.seed(123)
library(caret)
trainIndex <- createDataPartition(data$Class, p = 0.7, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

#6 Handle Class Imbalance
# Remove attributes from numeric columns
num_cols <- sapply(train_clean, is.numeric)
train_clean[num_cols] <- lapply(train_clean[num_cols], function(x) as.numeric(x))
# Apply ROSE again
library(ROSE)
train_balanced <- ROSE(Class ~ ., data = train_clean, seed = 1)$data
# Check the distribution
table(train_balanced$Class)

#7 Logistic Regression Model (optional)
log_model <- glm(Class ~ ., data = train_balanced, family = binomial)
summary(log_model)

str(train)   # see types in training data
str(test)    # see types in test data

test$Time <- as.numeric(test$Time)
test$Amount <- as.numeric(test$Amount)
                                
test$Class <- as.factor(test$Class)  # if needed

#7.a Predictions
log_pred_prob <- predict(log_model, test, type = "response")
log_pred <- ifelse(log_pred_prob > 0.5, 1, 0)

install.packages("caret")
library(caret)
#7.b Evaluation
confusionMatrix(as.factor(log_pred), as.factor(test$Class))

#8 Random Forest Model
install.packages("randomForest")
library(randomForest)

rf_model <- randomForest(
  Class ~ ., 
  data = train_balanced, 
  ntree = 100,
  importance = TRUE
)
                                
# Predictions
rf_pred <- predict(rf_model, test)
confusionMatrix(rf_pred, as.factor(test$Class))

#9 ROC Curve & AUC
roc_obj <- roc(test$Class, as.numeric(log_pred_prob))
plot(roc_obj, col = "blue", main = "ROC Curve")
auc(roc_obj)

#10 Identify Suspicious Transactions
suspicious_txns <- test[log_pred == 1, ]
head(suspicious_txns)




