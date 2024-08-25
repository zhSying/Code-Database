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
file_path <- "C:/Users/思/Desktop/Dataset/9-Gestational Diabetes.csv" 
clinical_data <- read_csv(file_path)
print(head(clinical_data))

# Data overview
data_overview <- list(
  dataset_name = "clinical_data",
  sample_count = nrow(clinical_data),
  feature_count = ncol(clinical_data),
  target_variable = "Prediction"  
)
data_overview

# Data type
data_types <- sapply(clinical_data, class)
data_types_summary <- data.frame(
  feature_name = names(data_types),
  data_type = data_types
)
data_types_summary

# Missing value analysis
total_missing <- sum(is.na(clinical_data))
total_data_points <- prod(dim(clinical_data))
overall_missing_rate <- total_missing / total_data_points
# Print missing value result
print(paste("Total Missing Values:", total_missing))
print(paste("Total Data Points:", total_data_points))
print(paste("Overall Missing Rate:", round(overall_missing_rate, 4)))


###Categorical Variables 
features <- c("'Pregnancy No'",
              "Heredity",
              "Prediction"
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
print(plot_grid(plotlist = plot_list, ncol = 3))
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

# Weight
Weight_distribution <- ggplot(clinical_data, aes(x = Weight)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "Weight Distribution", x = "Weight", y = "Count") +
  theme_minimal()


Weight_boxplot <- ggplot(clinical_data, aes(x = "", y = Weight)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "Weight Boxplot", x = "", y = "Weight") +
  theme_minimal()

plot_grid(Weight_distribution, Weight_boxplot, ncol = 2)

# Height
Height_distribution <- ggplot(clinical_data, aes(x = Height)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "Height Distribution", x = "Height", y = "Count") +
  theme_minimal()


Height_boxplot <- ggplot(clinical_data, aes(x = "", y = Height)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "Height Boxplot", x = "", y = "Height") +
  theme_minimal()

plot_grid(Height_distribution, Height_boxplot, ncol = 2)
# Plot
plot_grid(age_distribution, age_boxplot,
          bmi_distribution, bmi_boxplot,
          Height_distribution, Height_boxplot,
          Weight_distribution, Weight_boxplot, ncol = 4)

# Data processing
clinical_data$Heredity <- as.factor(clinical_data$Heredity)



##Table
summary_table <- clinical_data %>%
  tbl_summary(
    by = Prediction,
    statistic = all_continuous() ~ "{median} ({IQR})",  
    label = list(
      Age="Age",
      Pregnancy_No="Pregnancy No",
      Weight="Weight",
      Height="Height",
      BMI="BMI",
      Heredity="Heredity"
    ),
    missing = "ifany"  
  ) %>%
  add_p() %>%
  add_overall() %>%
  add_n() 

summary_table






