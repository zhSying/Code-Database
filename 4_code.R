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
file_path <- "C:/Users/思/Desktop/Dataset/4-Multiclass Diabetes Dataset Iraqi.csv" 
clinical_data <- read_csv(file_path)
print(head(clinical_data))

# Data overview
data_overview <- list(
  dataset_name = "clinical_data",
  sample_count = nrow(clinical_data),
  feature_count = ncol(clinical_data),
  target_variable = "CLASS"
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
features <- c("Gender",
              "CLASS"
)
create_bar_plot <- function(feature) {
  ggplot(clinical_data, aes_string(x = feature)) +
    geom_bar(fill = "purple", stat = "count")+  # 创建填充颜色为紫色的柱状图
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) + 
    labs(title = feature, x = NULL, y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(size = 24))
}
plot_list <- lapply(features, create_bar_plot)
print(plot_grid(plotlist = plot_list, ncol = 3))
### Continuous variable
age_distribution <- ggplot(clinical_data, aes(x = AGE)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal()

age_boxplot <- ggplot(clinical_data, aes(x = "", y = AGE)) +
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

# Urea
Urea_distribution <- ggplot(clinical_data, aes(x = Urea)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "Urea Distribution", x = "Urea", y = "Count") +
  theme_minimal()

Urea_boxplot <- ggplot(clinical_data, aes(x = "", y = Urea)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "Urea Boxplot", x = "", y = "Urea") +
  theme_minimal()

plot_grid(Urea_distribution, Urea_boxplot, ncol = 2)

# HbA1c
HbA1c_distribution <- ggplot(clinical_data, aes(x = HbA1c)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "HbA1c Distribution", x = "HbA1c", y = "Count") +
  theme_minimal()

HbA1c_boxplot <- ggplot(clinical_data, aes(x = "", y = HbA1c)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "HbA1c Boxplot", x = "", y = "HbA1c") +
  theme_minimal()

plot_grid(HbA1c_distribution, HbA1c_boxplot, ncol = 2)


# Chol
Chol_distribution <- ggplot(clinical_data, aes(x = Chol)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "Chol Distribution", x = "Chol", y = "Count") +
  theme_minimal()+
  scale_x_continuous(limits = c(min(clinical_data$Chol, na.rm = TRUE) - 1, max(clinical_data$Chol, na.rm = TRUE) + 1)) 

Chol_boxplot <- ggplot(clinical_data, aes(x = "", y = Chol)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "Chol Boxplot", x = "", y = "Chol") +
  theme_minimal()

plot_grid(Chol_distribution, Chol_boxplot, ncol = 2)

# TG
TG_disTGbution <- ggplot(clinical_data, aes(x = TG)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "TG DisTGbution", x = "TG", y = "Count") +
  theme_minimal()+
  scale_x_continuous(limits = c(min(clinical_data$TG, na.rm = TRUE) - 1, max(clinical_data$TG, na.rm = TRUE) + 1)) 

TG_boxplot <- ggplot(clinical_data, aes(x = "", y = TG)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "TG Boxplot", x = "", y = "TG") +
  theme_minimal()

plot_grid(TG_disTGbution, TG_boxplot, ncol = 2)

# HDL
HDL_distribution <- ggplot(clinical_data, aes(x = HDL)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "HDL Distribution", x = "HDL", y = "Count") +
  theme_minimal()+
  scale_x_continuous(limits = c(min(clinical_data$HDL, na.rm = TRUE) - 1, max(clinical_data$HDL, na.rm = TRUE) + 1)) 

HDL_boxplot <- ggplot(clinical_data, aes(x = "", y = HDL)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "HDL Boxplot", x = "", y = "HDL") +
  theme_minimal()

plot_grid(HDL_distribution, HDL_boxplot, ncol = 2)

# LDL
LDL_distribution <- ggplot(clinical_data, aes(x = LDL)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "LDL Distribution", x = "LDL", y = "Count") +
  theme_minimal()+
  scale_x_continuous(limits = c(min(clinical_data$LDL, na.rm = TRUE) - 1, max(clinical_data$LDL, na.rm = TRUE) + 1)) 

LDL_boxplot <- ggplot(clinical_data, aes(x = "", y = LDL)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "LDL Boxplot", x = "", y = "LDL") +
  theme_minimal()

plot_grid(LDL_distribution, LDL_boxplot, ncol = 2)

# VLDL
VLDL_distribution <- ggplot(clinical_data, aes(x = VLDL)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "VLDL Distribution", x = "VLDL", y = "Count") +
  theme_minimal()+
  scale_x_continuous(limits = c(min(clinical_data$VLDL, na.rm = TRUE) - 1, max(clinical_data$VLDL, na.rm = TRUE) + 1)) 

VLDL_boxplot <- ggplot(clinical_data, aes(x = "", y = VLDL)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "VLDL Boxplot", x = "", y = "VLDL") +
  theme_minimal()

plot_grid(VLDL_distribution, VLDL_boxplot, ncol = 2)

# Cr
Cr_distribution <- ggplot(clinical_data, aes(x = Cr)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "Cr Distribution", x = "Cr", y = "Count") +
  theme_minimal() 

Cr_boxplot <- ggplot(clinical_data, aes(x = "", y = Cr)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "Cr Boxplot", x = "", y = "Cr") +
  theme_minimal()+
  scale_x_continuous(limits = c(min(clinical_data$Cr, na.rm = TRUE) - 1, max(clinical_data$Cr, na.rm = TRUE) + 1))

plot_grid(Cr_distribution, Cr_boxplot, ncol = 2)


# plot
plot_grid(age_distribution, age_boxplot,
          bmi_distribution, bmi_boxplot,
          Urea_distribution, Urea_boxplot,
          TG_disTGbution, TG_boxplot,
          HbA1c_distribution, HbA1c_boxplot,
          Chol_distribution, Chol_boxplot,
          HDL_distribution, HDL_boxplot,
          LDL_distribution, LDL_boxplot,
          VLDL_distribution, VLDL_boxplot,
          Cr_distribution, Cr_boxplot,  ncol = 6)
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
    by = CLASS,
    statistic = all_continuous() ~ "{median} ({IQR})",  
    label = list(
      Gender="Gender",
      AGE="AGE",
      Urea="Urea",
      Cr="Cr",
      HbA1c="HbA1c",
      Chol="Chol",
      TG="TG",
      HDL="HDL",
      LDL="LDL",
      VLDL="VLDL",
      BMI="BMI"
    ),
    missing = "ifany"  
  ) %>%
  add_p() %>%
  add_overall() %>%
  add_n() 

summary_table






