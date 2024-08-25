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
file_path <- "C:/Users/思/Desktop/Dataset/1-Diabetes 130-US Hospitals for Years 1999-2008.csv" 
clinical_data <- read_csv(file_path)
print(head(clinical_data))

# Data overview
data_overview <- list(
  dataset_name = "clinical_data",
  sample_count = nrow(clinical_data),
  feature_count = ncol(clinical_data),
  target_variable = "Class"  
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
clinical_data[clinical_data == '?'] <- NA
missing_counts <- sapply(clinical_data, function(x) sum(is.na(x)))
print(missing_counts)
missing_ratios <- sapply(clinical_data, function(x) mean(is.na(x)))
print(missing_ratios)
summary(clinical_data)
total_missing <- sum(is.na(clinical_data))
total_data_points <- prod(dim(clinical_data))
overall_missing_rate <- total_missing / total_data_points
# Print missing value result
print(paste("Total Missing Values:", total_missing))
print(paste("Total Data Points:", total_data_points))
print(paste("Overall Missing Rate:", round(overall_missing_rate, 4)))


###Categorical Variables 
features <- c("race",
              "gender",
              "age",
              "max_glu_serum",
              "A1Cresult",
              "change",
              "diabetesMed",
              "readmitted"             
)
create_bar_plot <- function(feature) {
  ggplot(clinical_data, aes_string(x = feature)) +
    geom_bar(fill = "purple", stat = "count")+  # 创建填充颜色为紫色的柱状图
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) + 
    labs(title = feature, x = NULL, y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16))
}
plot_list <- lapply(features, create_bar_plot)
print(plot_grid(plotlist = plot_list, ncol = 4))

# 24 features for medications
features <- c("metformin",
              "repaglinide",
              "nateglinide",
              "chlorpropamide",
              "glimepiride",
              "acetohexamide",
              "glipizide",
              "glyburide",
              "tolbutamide",
              "pioglitazone",
              "rosiglitazone",
              "acarbose",
              "miglitol",
              "troglitazone",
              "tolazamide",
              "examide",
              "citoglipton",
              "insulin",
              "glyburide-metformin",
              "glipizide-metformin",
              "glimepiride-pioglitazone",
              "metformin-rosiglitazone",
              "metformin-pioglitazone"
)
create_bar_plot <- function(feature) {
  ggplot(clinical_data, aes_string(x = paste0("`", feature, "`"))) +
    geom_bar(fill = "purple", stat = "count") +  
    labs(title = feature, x = NULL, y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))
}

plot_list <- lapply(features, create_bar_plot)
print(plot_grid(plotlist = plot_list, ncol = 6))
### Continuous variable
# weight 
weight_distribution <- ggplot(clinical_data, aes(x = weight)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "Weight Distribution", x = "Weight", y = "Count") +
  theme_minimal()

weight_boxplot <- ggplot(clinical_data, aes(x = "", y = weight)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "Weight Boxplot", x = "", y = "Weight") +
  theme_minimal()

plot_grid(weight_distribution, weight_boxplot, ncol = 2)

# time_in_hospital
time_in_hospital_distribution <- ggplot(clinical_data, aes(x = time_in_hospital)) +
  geom_histogram(binwidth = 2, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "time_in_hospital Distribution", x = "time_in_hospital", y = "Count") +
  theme_minimal()


time_in_hospital_boxplot <- ggplot(clinical_data, aes(x = "", y = time_in_hospital)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "time_in_hospital Boxplot", x = "", y = "time_in_hospital") +
  theme_minimal()


plot_grid(time_in_hospital_distribution, time_in_hospital_boxplot, ncol = 2)

# num_lab_procedures
num_lab_procedures_distribution <- ggplot(clinical_data, aes(x = num_lab_procedures)) +
  geom_histogram(binwidth = 20, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "num_lab_procedures Distribution", x = "num_lab_procedures", y = "Count") +
  theme_minimal()


num_lab_procedures_boxplot <- ggplot(clinical_data, aes(x = "", y = num_lab_procedures)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "num_lab_procedures Boxplot", x = "", y = "num_lab_procedures") +
  theme_minimal()

plot_grid(num_lab_procedures_distribution, num_lab_procedures_boxplot, ncol = 2)

# num_procedures
num_procedures_distribution <- ggplot(clinical_data, aes(x = num_procedures)) +
  geom_histogram(binwidth = 0.5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "num_procedures Distribution", x = "num_procedures", y = "Count") +
  theme_minimal()


num_procedures_boxplot <- ggplot(clinical_data, aes(x = "", y = num_procedures)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "num_procedures Boxplot", x = "", y = "num_procedures") +
  theme_minimal()

plot_grid(num_procedures_distribution, num_procedures_boxplot, ncol = 2)

# num_medications
num_medications_distribution <- ggplot(clinical_data, aes(x = num_medications)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "num_medications Distribution", x = "num_medications", y = "Count") +
  theme_minimal()

num_medications_boxplot <- ggplot(clinical_data, aes(x = "", y = num_medications)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "num_medications Boxplot", x = "", y = "num_medications") +
  theme_minimal()

plot_grid(num_medications_distribution, num_medications_boxplot, ncol = 2)

# number_outpatient
number_outpatient_distribution <- ggplot(clinical_data, aes(x = number_outpatient)) +
  geom_histogram(binwidth = 3, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "number_outpatient Distribution", x = "number_outpatient", y = "Count") +
  theme_minimal()

number_outpatient_boxplot <- ggplot(clinical_data, aes(x = "", y = number_outpatient)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "number_outpatient Boxplot", x = "", y = "number_outpatient") +
  theme_minimal()

plot_grid(number_outpatient_distribution, number_outpatient_boxplot, ncol = 2)

# number_emergency
number_emergency_distribution <- ggplot(clinical_data, aes(x = number_emergency)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "number_emergency Distribution", x = "number_emergency", y = "Count") +
  theme_minimal()

number_emergency_boxplot <- ggplot(clinical_data, aes(x = "", y = number_emergency)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "number_emergency Boxplot", x = "", y = "number_emergency") +
  theme_minimal()

plot_grid(number_emergency_distribution, number_emergency_boxplot, ncol = 2)

# number_inpatient
number_inpatient_distribution <- ggplot(clinical_data, aes(x = number_inpatient)) +
  geom_histogram(binwidth = 3, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "number_inpatient Distribution", x = "number_inpatient", y = "Count") +
  theme_minimal()

number_inpatient_boxplot <- ggplot(clinical_data, aes(x = "", y = number_inpatient)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "number_inpatient Boxplot", x = "", y = "number_inpatient") +
  theme_minimal()

plot_grid(number_inpatient_distribution, number_inpatient_boxplot, ncol = 2)

# number_diagnoses
number_diagnoses_distribution <- ggplot(clinical_data, aes(x = number_diagnoses)) +
  geom_histogram(binwidth = 3, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "number_diagnoses Distribution", x = "number_diagnoses", y = "Count") +
  theme_minimal()

number_diagnoses_boxplot <- ggplot(clinical_data, aes(x = "", y = number_diagnoses)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "number_diagnoses Boxplot", x = "", y = "number_diagnoses") +
  theme_minimal()

plot_grid(number_diagnoses_distribution, number_diagnoses_boxplot, ncol = 2)
# Plot
plot_grid(time_in_hospital_distribution, time_in_hospital_boxplot,
          num_lab_procedures_distribution, num_lab_procedures_boxplot,
          num_procedures_distribution, num_procedures_boxplot,
          num_medications_distribution, num_medications_boxplot,
          number_outpatient_distribution, number_outpatient_boxplot,
          number_emergency_distribution, number_emergency_boxplot,
          number_inpatient_distribution, number_inpatient_boxplot,
          number_diagnoses_distribution, number_diagnoses_boxplot,
          ncol = 4)

# Data processing
clinical_data$Heredity <- as.factor(clinical_data$Heredity)



##Table
summary_table <- clinical_data %>%
  tbl_summary(
    by = readmitted,
    statistic = all_continuous() ~ "{median} ({IQR})",  
    missing = "ifany"  
  ) %>%
  add_p() %>%
  add_overall() %>%
  add_n() 

summary_table






