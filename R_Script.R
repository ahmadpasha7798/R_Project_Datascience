#Reading Data from the file present in the same directory
data <- read.csv("dataset ICT583 2023.csv")
#head(data)


# Remove rows with missing values
data <- na.omit(data)

# Convert necessary columns to appropriate data types
data$Gender <- as.factor(data$Gender)
data$Education_ID <- as.factor(data$Education_ID)
data$Marital_status_ID <- as.factor(data$Marital_status_ID)
data$MMSE_class_binary <- as.factor(data$MMSE_class_binary)

# save the cleaned dataset
write.csv(data, "data.csv", row.names = FALSE)

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

#Normalization
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

#Transforming
#data$Education_ID <- as.numeric(data$Education_ID)
#data$MMSE_class_binary <- as.numeric(data$MMSE_class_binary)
#data$Gender <- as.numeric(data$Gender)
#data$Marital_status_ID <- as.numeric(data$Marital_status_ID)

#Scaling
#data$Gender <- scale(data$Gender)
#data$Education_ID <- scale(data$Education_ID)
#data$Financial_status <- scale(data$Financial_status)
#data$GDS <- scale(data$GDS)
#data$Marital_status_ID <- scale(data$Marital_status_ID)


write.csv(data, "transformed_data.csv", row.names = FALSE)

