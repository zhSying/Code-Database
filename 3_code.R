rm(list = ls())
install.packages("foreign")
install.packages("reshape2")
install.packages("corrplot")
library(foreign)
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(randomForest)
library(e1071)
library(ggplot2)
library(reshape2)
library(corrplot)
library(gtsummary)
file_path <- "C:/Users/思/Desktop/Dataset/3-Pima Indians Diabetes Database.csv" 
clinical_data <- read_csv(file_path)
print(head(clinical_data))

# Data overview
data_overview <- list(
  dataset_name = "clinical_data",
  sample_count = nrow(clinical_data),
  feature_count = ncol(clinical_data),
  target_variable = "Outcome" 
)
data_overview

# Data type
data_types <- sapply(clinical_data, class)
data_types_summary <- data.frame(
  feature_name = names(data_types),
  data_type = data_types
)
data_types_summary

# List of columns to convert 0 to NA
columns_to_convert <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")

# Loop through each column and replace 0 with NA
clinical_data[columns_to_convert] <- lapply(clinical_data[columns_to_convert], function(x) {
  x[x == 0] <- NA
  return(x)
})

# Missing value analysis
total_missing <- sum(is.na(clinical_data))
total_data_points <- prod(dim(clinical_data))
overall_missing_rate <- total_missing / total_data_points
# Print missing value result
print(paste("Total Missing Values:", total_missing))
print(paste("Total Data Points:", total_data_points))
print(paste("Overall Missing Rate:", round(overall_missing_rate, 4)))


###Categorical Variables 
features <- c("Pregnancies",
              "Outcome"
)
create_bar_plot <- function(feature) {
  ggplot(clinical_data, aes_string(x = feature)) +
    geom_bar(fill = "purple", stat = "count")+  
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) + 
    labs(title = feature, x = NULL, y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(size = 24))
}
plot_list <- lapply(features, create_bar_plot)
print(plot_grid(plotlist = plot_list, ncol = 2))

### Continuous variable
age_distribution <- ggplot(clinical_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal()

age_boxplot <- ggplot(clinical_data, aes(x = "", y = Age)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "Age Boxplot", x = "", y = "Age") +
  theme_minimal()


plot_grid(age_distribution, age_boxplot, ncol = 2)

# BMI
bmi_distribution <- ggplot(clinical_data, aes(x = BMI)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "BMI Distribution", x = "BMI", y = "Count") +
  theme_minimal()


bmi_boxplot <- ggplot(clinical_data, aes(x = "", y = BMI)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "BMI Boxplot", x = "", y = "BMI") +
  theme_minimal()


plot_grid(bmi_distribution, bmi_boxplot, ncol = 2)

# Glucose
Glucose_distribution <- ggplot(clinical_data, aes(x = Glucose)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "Glucose Distribution", x = "Glucose", y = "Count") +
  theme_minimal()


Glucose_boxplot <- ggplot(clinical_data, aes(x = "", y = Glucose)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "Glucose Boxplot", x = "", y = "Glucose") +
  theme_minimal()

plot_grid(Glucose_distribution, Glucose_boxplot, ncol = 2)

# BloodPressure
BloodPressure_distribution <- ggplot(clinical_data, aes(x = BloodPressure)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "BloodPressure Distribution", x = "BloodPressure", y = "Count") +
  theme_minimal()


BloodPressure_boxplot <- ggplot(clinical_data, aes(x = "", y = BloodPressure)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "BloodPressure Boxplot", x = "", y = "BloodPressure") +
  theme_minimal()

plot_grid(BloodPressure_distribution, BloodPressure_boxplot, ncol = 2)

#SkinThickness
SkinThickness_distribution <- ggplot(clinical_data, aes(x = SkinThickness)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "SkinThickness Distribution", x = "SkinThickness", y = "Count") +
  theme_minimal()


SkinThickness_boxplot <- ggplot(clinical_data, aes(x = "", y = SkinThickness)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "SkinThickness Boxplot", x = "", y = "SkinThickness") +
  theme_minimal()


plot_grid(SkinThickness_distribution, SkinThickness_boxplot, ncol = 2)

#Insulin
Insulin_distribution <- ggplot(clinical_data, aes(x = Insulin)) +
  geom_histogram(aes(y = ..density..), binwidth = 80, fill = "purple", color = "black",alpha = 0.5) +
  geom_density(color = "black", size = 0.5) +
  labs(title = "Insulin Distribution", x = "Insulin", y = "Count") +
  theme_minimal()


Insulin_boxplot <- ggplot(clinical_data, aes(x = "", y = Insulin)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "Insulin Boxplot", x = "", y = "Insulin") +
  theme_minimal()

plot_grid(Insulin_distribution, Insulin_boxplot, ncol = 2)

#DiabetesPedigreeFunction
DiabetesPedigreeFunction_distribution <- ggplot(clinical_data, aes(x = DiabetesPedigreeFunction)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(color = "black", size = 0.5) +
  labs(title = "DiabetesPedigreeFunction Distribution", 
       x = "DiabetesPedigreeFunction", 
       y = "Density") +
  theme_minimal()

DiabetesPedigreeFunction_boxplot <- ggplot(clinical_data, aes(x = "", y = DiabetesPedigreeFunction)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "DiabetesPedigreeFunction Boxplot", x = "", y = "DiabetesPedigreeFunction") +
  theme_minimal()

plot_grid(DiabetesPedigreeFunction_distribution, DiabetesPedigreeFunction_boxplot, ncol = 2)

# Plot
plot_grid(age_distribution, age_boxplot,
          bmi_distribution, bmi_boxplot,
          Insulin_distribution, Insulin_boxplot,
          SkinThickness_distribution, SkinThickness_boxplot,
          BloodPressure_distribution, BloodPressure_boxplot, 
          Glucose_distribution, Glucose_boxplot,
          DiabetesPedigreeFunction_distribution, DiabetesPedigreeFunction_boxplot,ncol = 4)

# Data processing
clinical_data$Heredity <- as.factor(clinical_data$Heredity)



##Table
summary_table <- clinical_data %>%
  tbl_summary(
    by = Outcome,
    statistic = all_continuous() ~ "{median} ({IQR})",  
    label = list(
      Pregnancies="Pregnancies",
      Glucose="Glucose",
      BloodPressure="BloodPressure",
      SkinThickness="SkinThickness",
      Insulin="Insulin",
      BMI="BMI",
      DiabetesPedigreeFunction="DiabetesPedigreeFunction",
      Age="Age"
    ),
    missing = "ifany"  
  ) %>%
  add_p() %>%
  add_overall() %>%
  add_n() 

summary_table






