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
file_path <- "C:/Users/思/Desktop/Dataset/9-china diabetes.csv"  # 替换为你的CSV文件路径
clinical_data <- read_csv(file_path)
print(head(clinical_data))

# Missing analysis
clinical_data[clinical_data == 4.860753] <- NA
missing_counts <- sapply(clinical_data, function(x) sum(is.na(x)))
print(missing_counts)
missing_ratios <- sapply(clinical_data, function(x) mean(is.na(x)))
print(missing_ratios)
summary(clinical_data)

###Categorical feature
features <- c("Gender",
              "smoking",
              "drinking",
              "family_histroy",
              "Diabetes"
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
plot_grid(plotlist = plot_list, ncol = 5)

###Continuous feature
# age
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

# SBP
SBP_distribution <- ggplot(clinical_data, aes(x = SBP)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "SBP Distribution", x = "SBP", y = "Count") +
  theme_minimal()

SBP_boxplot <- ggplot(clinical_data, aes(x = "", y = SBP)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "SBP Boxplot", x = "", y = "SBP") +
  theme_minimal()

plot_grid(SBP_distribution, SBP_boxplot, ncol = 2)

# DBP
DBP_distribution <- ggplot(clinical_data, aes(x = DBP)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "DBP Distribution", x = "DBP", y = "Count") +
  theme_minimal()

DBP_boxplot <- ggplot(clinical_data, aes(x = "", y = DBP)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "DBP Boxplot", x = "", y = "DBP") +
  theme_minimal()

plot_grid(DBP_distribution, DBP_boxplot, ncol = 2)

# FPG
FPG_distribution <- ggplot(clinical_data, aes(x = FPG)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "FPG Distribution", x = "FPG", y = "Count") +
  theme_minimal()+
  scale_x_continuous(limits = c(0, max(clinical_data$FPG, na.rm = TRUE) + 1))  

FPG_boxplot <- ggplot(clinical_data, aes(x = "", y = FPG)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "FPG Boxplot", x = "", y = "FPG") +
  theme_minimal()

plot_grid(FPG_distribution, FPG_boxplot, ncol = 2)

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

# Tri
Tri_distribution <- ggplot(clinical_data, aes(x = Tri)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "Tri Distribution", x = "Tri", y = "Count") +
  theme_minimal()+
  scale_x_continuous(limits = c(min(clinical_data$Tri, na.rm = TRUE) - 1, max(clinical_data$Tri, na.rm = TRUE) + 1)) 

Tri_boxplot <- ggplot(clinical_data, aes(x = "", y = Tri)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "Tri Boxplot", x = "", y = "Tri") +
  theme_minimal()

plot_grid(Tri_distribution, Tri_boxplot, ncol = 2)

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

# ALT
ALT_distribution <- ggplot(clinical_data, aes(x = ALT)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "ALT Distribution", x = "ALT", y = "Count") +
  theme_minimal()+
  scale_x_continuous(limits = c(min(clinical_data$ALT, na.rm = TRUE) - 1, max(clinical_data$ALT, na.rm = TRUE) + 1)) 

ALT_boxplot <- ggplot(clinical_data, aes(x = "", y = ALT)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "ALT Boxplot", x = "", y = "ALT") +
  theme_minimal()

plot_grid(ALT_distribution, ALT_boxplot, ncol = 2)

# BUN
BUN_distribution <- ggplot(clinical_data, aes(x = BUN)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "BUN Distribution", x = "BUN", y = "Count") +
  theme_minimal()+
  scale_x_continuous(limits = c(min(clinical_data$BUN, na.rm = TRUE) - 1, max(clinical_data$BUN, na.rm = TRUE) + 1)) 

BUN_boxplot <- ggplot(clinical_data, aes(x = "", y = BUN)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "BUN Boxplot", x = "", y = "BUN") +
  theme_minimal()

plot_grid(BUN_distribution, BUN_boxplot, ncol = 2)

# CCR
CCR_distribution <- ggplot(clinical_data, aes(x = CCR)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "CCR Distribution", x = "CCR", y = "Count") +
  theme_minimal() 

CCR_boxplot <- ggplot(clinical_data, aes(x = "", y = CCR)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "CCR Boxplot", x = "", y = "CCR") +
  theme_minimal()+
  scale_x_continuous(limits = c(min(clinical_data$CCR, na.rm = TRUE) - 1, max(clinical_data$CCR, na.rm = TRUE) + 1))

plot_grid(CCR_distribution, CCR_boxplot, ncol = 2)

# FFPG
FFPG_distribution <- ggplot(clinical_data, aes(x = FFPG)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "FFPG Distribution", x = "FFPG", y = "Count") +
  theme_minimal()+
  scale_x_continuous(limits = c(min(clinical_data$FFPG, na.rm = TRUE) - 1, max(clinical_data$FFPG, na.rm = TRUE) + 1)) 

FFPG_boxplot <- ggplot(clinical_data, aes(x = "", y = FFPG)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "FFPG Boxplot", x = "", y = "FFPG") +
  theme_minimal()

# plot
plot_grid(age_distribution, age_boxplot,
          bmi_distribution, bmi_boxplot,
          SBP_distribution, SBP_boxplot,
          DBP_distribution, DBP_boxplot,
          FPG_distribution, FPG_boxplot,
          Chol_distribution, Chol_boxplot,
          Tri_distribution, Tri_boxplot,
          HDL_distribution, HDL_boxplot,
          LDL_distribution, LDL_boxplot,
          ALT_distribution, ALT_boxplot,
          BUN_distribution, BUN_boxplot,
          CCR_distribution, CCR_boxplot, 
          FFPG_distribution, FFPG_boxplot, ncol = 6)

# data processing
clinical_data$family_histroy <- as.factor(clinical_data$family_histroy)



## Baseline table
summary_table <- clinical_data %>%
  tbl_summary(
    by = Diabetes,  
    statistic = all_continuous() ~ "{median} ({IQR})",  
    label = list(
      Age="Age",
      Gender="Gender",
      BMI="BMI",
      SBP="SBP",
      DBP="DBP",
      FPG="FPG",
      Chol="Chol",
      Tri="Tri",
      HDL="HDL",
      LDL="LDL",
      ALT="ALT",
      BUN="BUN",
      CCR="CCR",
      FFPG="FFPG",
      smoking="smoking",
      drinking="drinking",
      family_histroy="family_histroy"
    ),
    missing = "ifany"  
  ) %>%
  add_p() %>%
  add_overall() %>%
  add_n() 

summary_table


