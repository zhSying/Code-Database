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
file_path <- "C:/Users/思/Desktop/Dataset/10-diabetes_dataset__2019.csv" 
clinical_data <- read_csv(file_path)
print(head(clinical_data))

# Missing analysis
total_missing <- sum(is.na(clinical_data))
total_data_points <- prod(dim(clinical_data))
overall_missing_rate <- total_missing / total_data_points
print(paste("Total Missing Values:", total_missing))
print(paste("Total Data Points:", total_data_points))
print(paste("Overall Missing Rate:", round(overall_missing_rate, 4)))


###Categorical Variables 
features <- c("Age",
              "Gender",
              "Family_Diabetes",
              "highBP",
              "PhysicallyActive",
              "Smoking",
              "Alcohol",
              "Sleep",
              "SoundSleep",
              "RegularMedicine",
              "JunkFood",
              "Stress",
              "BPLevel",
              "Pregancies",
              "Pdiabetes",
              "UriationFreq",
              "Diabetic"
)
create_bar_plot <- function(feature) {
  ggplot(clinical_data, aes_string(x = feature)) +
    geom_bar(fill = "purple", stat = "count")+ 
    labs(title = feature, x = NULL, y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12))
}
plot_list <- lapply(features, create_bar_plot)
print(plot_grid(plotlist = plot_list, ncol = 5))
###Continuous variable
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

# Data processing
clinical_data$Family_Diabetes <- as.factor(clinical_data$Family_Diabetes)
clinical_data$highBP <- as.factor(clinical_data$highBP)
clinical_data$Smoking <- as.factor(clinical_data$Smoking)
clinical_data$Alcohol <- as.factor(clinical_data$Alcohol)
clinical_data$SoundSleep <- as.factor(clinical_data$SoundSleep)

## Baseline table
summary_table <- clinical_data %>%
  tbl_summary(
    by = Diabetic,  
    statistic = all_continuous() ~ "{median} ({IQR})",  
    label = list(
      Age="Age",
      Gender="Gender",
      Family_Diabetes="Family_Diabetes",
      highBP="highBP",
      PhysicallyActive="PhysicallyActive",
      BMI="BMI",
      Smoking="Smoking",
      Alcohol="Alcohol",
      Sleep="Sleep",
      SoundSleep="SoundSleep",
      RegularMedicine="RegularMedicine",
      JunkFood="JunkFood",
      Stress="Stress",
      BPLevel="BPLevel",
      Pregancies="Pregancies",
      Pdiabetes="Pdiabetes",
      UriationFreq="UriationFreq"
    ),
    missing = "ifany"  
  ) %>%
  add_p(test = all_categorical() ~ "fisher.test", pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  add_overall() %>%
  add_n() 

summary_table

