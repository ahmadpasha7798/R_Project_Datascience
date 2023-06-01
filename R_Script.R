#Reading Data from the file present in the same directory
data <- read.csv("dataset ICT583 2023.csv")
#head(data)


# Remove rows with missing values as they incomplete data cannot be used
data <- na.omit(data)

# Convert necessary columns to appropriate data types
data$Gender <- as.factor(data$Gender)
data$Education_ID <- as.factor(data$Education_ID)
data$Marital_status_ID <- as.factor(data$Marital_status_ID)
data$MMSE_class_binary <- as.factor(data$MMSE_class_binary)

# save the cleaned dataset
#write.csv(data, "data.csv", row.names = FALSE)

# Function to treat outliers using Winsorization so that the outliers does not reduce the model accuracy
treat_outliers <- function(x, threshold = 0.05) {
  q <- quantile(x, probs = c(threshold, 1 - threshold), na.rm = TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  x
}

# Apply outlier treatment for each field
data$Age <- treat_outliers(data$Age)
data$Body_Height <- treat_outliers(data$Body_Height)
data$Body_Weight <- treat_outliers(data$Body_Weight)
data$GDS <- treat_outliers(data$GDS)
data$MNAa_total <- treat_outliers(data$MNAa_total)
data$MNAb_total <- treat_outliers(data$MNAb_total)

# Write the treated data to a new CSV file
#write.csv(data, "treated_data.csv", row.names = FALSE)

#Normalization: To make the data normal so that one attribute does not dominate other
# Function to min-max normalize a vector
min_max_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Min-max normalize the numerical attributes
data$Age <- min_max_normalize(data$Age)
data$Body_Height <- min_max_normalize(data$Body_Height)
data$Body_Weight <- min_max_normalize(data$Body_Weight)
data$GDS <- min_max_normalize(data$GDS)
data$MNAa_total <- min_max_normalize(data$MNAa_total)
data$MNAb_total <- min_max_normalize(data$MNAb_total)

#Removing column which is not required for data processing
data <- data[, -1]


write.csv(data, "cleaned_data.csv", row.names = FALSE)

#Preliminary Investigation

# Histogram of Age
#this histogram shows that the number of people around age 70 are greater that the other ages
hist(data$Age, main = "Histogram of Age", xlab = "Age")

# Histogram of Body_Height
#shows that the height around 151 cm is most common height in the dataset
hist(data$Body_Height, main = "Histogram of Body Height", xlab = "Body Height")

# Histogram of Body_Weight
#weight around 53 kg is highest in numbers in dataset
hist(data$Body_Weight, main = "Histogram of Body Weight", xlab = "Body Weight")

# Bar plot of Gender
#One gender is almost double the amount of other
barplot(table(data$Gender), main = "Bar Plot of Gender", xlab = "Gender", ylab = "Frequency")

# Bar plot of Education_ID
# Education Id 2 is highest in number and 4 is lowest
barplot(table(data$Education_ID), main = "Bar Plot of Education ID", xlab = "Education ID", ylab = "Frequency")

# Bar plot of Marital_status_ID
#Martial status 2 is most common
barplot(table(data$Marital_status_ID), main = "Bar Plot of Marital Status ID", xlab = "Marital Status ID", ylab = "Frequency")

# Bar plot of MMSE_class_binary
# MMSE Class Binary is 1 for 1/8th of the dataset
barplot(table(data$MMSE_class_binary), main = "Bar Plot of MMSE Class Binary", xlab = "MMSE Class Binary", ylab = "Frequency")

##########Applying Models##################
#Logistic Regression: It is a popular linear model used for binary classification problems. It models the relationship between the predictor variables and the binary response variable using the logistic function, which maps the linear combination of predictors to a probability of belonging to a particular class.
#Random Forest: It is an ensemble learning method that combines multiple decision trees to make predictions. Random Forest is effective in handling both classification and regression tasks. It creates a collection of decision trees, where each tree is trained on a random subset of the data and features. The final prediction is determined by aggregating the predictions of all the trees.

#installing Package
#install.packages("caret")

# Load required packages
#Uncomment above line if error occurs
library(caret)

# Set seed for reproducibility
set.seed(123)

# Convert MMSE_class_binary to a factor
data$MMSE_class_binary <- as.factor(data$MMSE_class_binary)

# Split the data into training and testing sets
trainIndex <- createDataPartition(data$MMSE_class_binary, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train and test Logistic Regression
logisticModel <- train(MMSE_class_binary ~ ., data = trainData, method = "glm", family = "binomial")
logisticPred <- predict(logisticModel, newdata = testData)

# Train and test Random Forest
randomForestModel <- train(MMSE_class_binary ~ ., data = trainData, method = "rf")
randomForestPred <- predict(randomForestModel, newdata = testData)

# Evaluate model performance
logistic_metrics <- confusionMatrix(logisticPred, testData$MMSE_class_binary)
randomforest_metrics <- confusionMatrix(randomForestPred, testData$MMSE_class_binary)

logistic_accuracy <- logistic_metrics$overall["Accuracy"]
randomforest_accuracy <- randomforest_metrics$overall["Accuracy"]

logistic_precision <- logistic_metrics$byClass["Pos Pred Value"]
randomforest_precision <- randomforest_metrics$byClass["Pos Pred Value"]

logistic_recall <- logistic_metrics$byClass["Sensitivity"]
randomforest_recall <- randomforest_metrics$byClass["Sensitivity"]

logistic_f1score <- logistic_metrics$byClass["F1"]
randomforest_f1score <- randomforest_metrics$byClass["F1"]

logistic_aucroc <- logistic_metrics$byClass["ROC"]
randomforest_aucroc <- randomforest_metrics$byClass["ROC"]

# Print the performance metrics for comparison
print("Logistic Regression:")
print(paste("Accuracy:", logistic_accuracy))
print(paste("Precision:", logistic_precision))
print(paste("Recall:", logistic_recall))
print(paste("F1 Score:", logistic_f1score))
print(paste("AUC-ROC:", logistic_aucroc))

print("Random Forest:")
print(paste("Accuracy:", randomforest_accuracy))
print(paste("Precision:", randomforest_precision))
print(paste("Recall:", randomforest_recall))
print(paste("F1 Score:", randomforest_f1score))
print(paste("AUC-ROC:", randomforest_aucroc))

#logisticAcc <- confusionMatrix(logisticPred, testData$MMSE_class_binary)$overall["Accuracy"]
#randomForestAcc <- confusionMatrix(randomForestPred, testData$MMSE_class_binary)$overall["Accuracy"]

#print(logisticAcc)
#print(randomForestAcc)

#Compare Performance
#performance <- data.frame(Model = c("Logistic Regression", "Random Forest"),
 #                         Accuracy = c(logisticAcc, randomForestAcc))

#print(performance)


