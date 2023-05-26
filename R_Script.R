#Reading Data from the file present in the same directory
data <- read.csv("dataset ICT583 2023.csv")
head(data)


# Remove rows with missing values
data <- na.omit(data)

# Convert necessary columns to appropriate data types
data$Gender <- as.factor(data$Gender)
data$Education_ID <- as.factor(data$Education_ID)
data$Marital_status_ID <- as.factor(data$Marital_status_ID)
data$MMSE_class_binary <- as.factor(data$MMSE_class_binary)

# save the cleaned dataset
write.csv(data, "data.csv", row.names = FALSE)
# 
# # Identify outliers using a boxplot
# boxplot(data$Body_Weight)
# 
# # Apply Winsorization to treat outliers
# # Define the lower and upper percentile values (e.g., 1st and 99th percentiles)
# lower_percentile <- quantile(data$Body_Weight, 0.01)
# upper_percentile <- quantile(data$Body_Weight, 0.99)
# 
# # Winsorize the outliers
# data$Body_Weight[data$Body_Weight < lower_percentile] <- lower_percentile
# data$Body_Weight[data$Body_Weight > upper_percentile] <- upper_percentile
# 
# # Identify outliers using a boxplot AGE
# boxplot(data$Age)
# 
# # Apply Winsorization to treat outliers
# # Define the lower and upper percentile values (e.g., 1st and 99th percentiles)
# lower_percentile_age <- quantile(data$Age, 0.01)
# upper_percentile_age <- quantile(data$Age, 0.99)
# 
# # Winsorize the outliers
# data$Age[data$Age < lower_percentile_age] <- lower_percentile_age
# data$Age[data$Age > upper_percentile_age] <- upper_percentile_age
# 
# # Identify outliers using a boxplot Height
# boxplot(data$Body_Height)
# 
# # Apply Winsorization to treat outliers
# # Define the lower and upper percentile values (e.g., 1st and 99th percentiles)
# lower_percentile_height <- quantile(data$Body_Height, 0.01)
# upper_percentile_height <- quantile(data$Body_Height, 0.99)
# 
# # Winsorize the outliers
# data$Body_Height[data$Body_Height < lower_percentile_height] <- lower_percentile_height
# data$Body_Height[data$Body_Height > upper_percentile_height] <- upper_percentile_height


# Function to treat outliers using Winsorization
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
write.csv(data, "treated_data.csv", row.names = FALSE)

# Following needed for Transforming Dataset
install.packages("dplyr")
install.packages("tidyverse")
library(dplyr)


