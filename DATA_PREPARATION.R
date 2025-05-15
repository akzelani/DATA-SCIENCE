data <- read.csv("D:/data.csv", na.strings = c("", "NA"))
View(data)

numberOfCol <- ncol(data)
numberOfRow <- nrow(data)
cat("Number of Column: ", numberOfCol, "\n")
cat("Number of Row: ", numberOfRow, "\n")
str(data)

missing_summary <- colSums(is.na(data))
print(missing_summary)

for (var in names(missing_summary)) { 
  if (missing_summary[var] > 0) { 
    missing_rows <- which(is.na(data[var]))   
    cat(var, ":", paste(missing_rows, collapse = ", "), 
        "\n")  
  } 
} 

naniar::gg_miss_var(data) 

heart_rate_mapping <- c("Low" = 0, "High" = 1)
data$Heart_Rate <- as.numeric(heart_rate_mapping[as.character(data$Heart_Rate)])



total_value <- nrow(data) * ncol(data) 
total_missing <- sum(is.na(data)) 
missing_percentage<-round((total_missing/total_value)*100,2) 
cat("Total Value: ", total_value,  
    "\nTotal Missing: ", total_missing,  
    "\nMissing Percentage: ",missing_percentage,"%",sep="") 

data <- unique(data) 
summary(data)

miss_val_col_deleted_dataset <- na.omit(data) 
sum(is.na(miss_val_col_deleted_dataset)) 
View(miss_val_col_deleted_dataset)

data$BloodPressure <- gsub("[^0-9.]", "", data$BloodPressure)
data$BloodPressure <- as.numeric(data$BloodPressure)


replace_NULL_and_Outlier_value <- function(dataset, attribute, replace_by) {
  column <- dataset[[attribute]]
  if (!is.numeric(column)) {
    column <- as.numeric(as.character(column))
    warning(paste("Column", attribute, "was not numeric. Converted to numeric."))
  }
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR

  if (replace_by == "mean") {
    value <- mean(column[column >= lower_bound & column <= upper_bound], na.rm = TRUE)
  } else if (replace_by == "median") {
    value <- median(column, na.rm = TRUE)
  } else if (replace_by == "sd") {
    value <- sd(column[column >= lower_bound & column <= upper_bound], na.rm = TRUE)
  } else if (replace_by == "mfv"){
    value <- modeest::mfv(column[column >= lower_bound & column <= upper_bound], na.rm = TRUE)
  }
  column[is.na(column) | column < lower_bound | column > upper_bound] <- value
  dataset[[attribute]] <- column
  return(dataset)
}



if ("Age" %in% names(data)) {
  data <- replace_NULL_and_Outlier_value(data, "Age", "mean")
  summary(data$Age)
}
if ("BloodPressure" %in% names(data)) {
  data <- replace_NULL_and_Outlier_value(data, "BloodPressure", "median")
  summary(data$BloodPressure)
}

if ("Gender" %in% names(data)) {
  data <- replace_NULL_and_Outlier_value(data, "Gender", "mfv")
  summary(data$Gender)
}
if ("Heart_Rate" %in% names(data)) {
  data <- replace_NULL_and_Outlier_value(data, "Heart_Rate", "median")
  summary(data$Heart_Rate)
}
if ("Cholesterol" %in% names(data)) {
  data <- replace_NULL_and_Outlier_value(data, "Cholesterol", "mean")
  summary(data$Cholesterol)
}
if ("HeartDisease" %in% names(data)) {
  data <- replace_NULL_and_Outlier_value(data, "HeartDisease", "median")
  summary(data$HeartDisease)
} 
if ("QuantumPatternFeature" %in% names(data)) {
  data <- replace_NULL_and_Outlier_value(data, "QuantumPatternFeature", "mean")
  summary(data$QuantumPatternFeature)
} 
sample_index <- sample(1:nrow(data), size = 0.7 * nrow(data))
train_data <- data[sample_index, ]
test_data <- data[-sample_index, ]
cat("Training Set Dimensions: ", dim(train_data), "\n")
cat("Testing Set Dimensions: ", dim(test_data), "\n")

if (all(c("Age", "Heart_Rate") %in% names(data))) {
  cat("\n--- Central Tendency of Age by Heart_Rate ---\n")
  

  age_mean_by_heart_rate <- tapply(data$Age, data$Heart_Rate, mean, na.rm = TRUE)
  print("Mean Age by Heart_Rate:")
  print(age_mean_by_heart_rate)
  

  age_median_by_heart_rate <- tapply(data$Age, data$Heart_Rate, median, na.rm = TRUE)
  print("Median Age by Heart_Rate:")
  print(age_median_by_heart_rate)
  
  age_mode_by_heart_rate <- tapply(data$Age, data$Heart_Rate, sd, na.rm = TRUE)
  print("Mode Age by Heart_Rate:")
  print(age_mode_by_heart_rate)
  
}
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

data$Heart_Rate <- factor(data$Heart_Rate, levels = c("0", "1"), labels = c("Low", "High"))
data$Gender <- factor(data$Gender, levels = c("0", "1"), labels = c("Female", "Male"))

age_mean_by_gender <- tapply(data$Age, data$Gender, mean, na.rm = TRUE)
cat("Mean Age by Gender:")
print(age_mean_by_gender)
age_median_by_gender <- tapply(data$Age, data$Gender, median, na.rm = TRUE)
cat("\nMedian Age by Gender:")
print(age_median_by_gender)
age_mode_by_gender <- tapply(data$Age, data$Gender, get_mode)
cat("\nMode Age by Gender:")
print(age_mode_by_gender)



age_range <- tapply(data$Age, data$Gender, range, na.rm = TRUE)
cat("Range of Age by Gender:\n")
print(age_range)
age_iqr <- tapply(data$Age, data$Gender, function(x) IQR(x, na.rm = TRUE))
cat("\nIQR of Age by Gender:\n")
print(age_iqr)
age_var <- tapply(data$Age, data$Gender, function(x) var(x, na.rm = TRUE))
cat("\nVariance of Age by Gender:\n")
print(age_var)
age_sd <- tapply(data$Age, data$Gender, function(x) sd(x, na.rm = TRUE))
cat("\nStandard Deviation of Age by Gender:\n")
print(age_sd)


write.csv(data, "D:/updateddataset.csv")