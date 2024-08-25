rm(list = ls())
install.packages("foreign")
install.packages("reshape2")
install.packages("corrplot")
install.packages("caret")
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
file_path <- "C:/Users/思/Desktop/Dataset/8.3-diabetes_binary_health_indicators_BRFSS2021.csv"  
clinical_data <- read_csv(file_path)
print(head(clinical_data))

# Data overview
data_overview <- list(
  dataset_name = "clinical_data",
  sample_count = nrow(clinical_data),
  feature_count = ncol(clinical_data),
  target_variable = "Diabetes_binary" 
)
data_overview

# Data type
data_types <- sapply(clinical_data, class)
data_types_summary <- data.frame(
  feature_name = names(data_types),
  data_type = data_types
)
data_types_summary

###Categorical feature
features <- c("Diabetes_binary",
              "HighBP",
              "HighChol",
              "CholCheck",
              "Smoker",
              "Stroke",
              "HeartDiseaseorAttack",
              "PhysActivity",
              "Fruits",
              "Veggies",
              "HvyAlcoholConsump",
              "AnyHealthcare",
              "NoDocbcCost",
              "GenHlth",
              "DiffWalk",
              "Sex",
              "Education",
              "Income")
create_bar_plot <- function(feature) {
  ggplot(clinical_data, aes_string(x = feature)) +
    geom_bar(fill = "purple", stat = "count") +
    labs(title = feature, x = NULL, y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16))
}
plot_list <- lapply(features, create_bar_plot)
plot_grid(plotlist = plot_list, ncol = 4)

###Continuous feature
# Age
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

# MentHlth
MentHlth_distribution <- ggplot(clinical_data, aes(x = MentHlth)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "MentHlth Distribution", x = "MentHlth", y = "Count") +
  theme_minimal()

MentHlth_boxplot <- ggplot(clinical_data, aes(x = "", y = MentHlth)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "MentHlth Boxplot", x = "", y = "MentHlth") +
  theme_minimal()

plot_grid(MentHlth_distribution, MentHlth_boxplot, ncol = 2)


# GenHlth
GenHlth_distribution <- ggplot(clinical_data, aes(x = GenHlth)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "GenHlth Distribution", x = "GenHlth", y = "Count") +
  theme_minimal()+
  scale_x_continuous(limits = c(0,30)) 

GenHlth_boxplot <- ggplot(clinical_data, aes(x = "", y = GenHlth)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "GenHlth Boxplot", x = "", y = "GenHlth") +
  theme_minimal()

plot_grid(GenHlth_distribution, GenHlth_boxplot, ncol = 2)

# Plot
plot_grid(age_distribution, age_boxplot,
          bmi_distribution, bmi_boxplot,
          MentHlth_distribution, MentHlth_boxplot,
          GenHlth_distribution, GenHlth_boxplot, ncol = 4)
# Data processing 
clinical_data$HighBP <- as.factor(clinical_data$HighBP)
clinical_data$HighChol <- as.factor(clinical_data$HighChol)
clinical_data$CholCheck <- as.factor(clinical_data$CholCheck)
clinical_data$Smoker <- as.factor(clinical_data$Smoker)
clinical_data$Stroke <- as.factor(clinical_data$Stroke)
clinical_data$HeartDiseaseorAttack <- as.factor(clinical_data$HeartDiseaseorAttack)
clinical_data$PhysActivity <- as.factor(clinical_data$PhysActivity)
clinical_data$Fruits <- as.factor(clinical_data$Fruits)
clinical_data$Veggies <- as.factor(clinical_data$Veggies)
clinical_data$HvyAlcoholConsump <- as.factor(clinical_data$HvyAlcoholConsump)
clinical_data$AnyHealthcare <- as.factor(clinical_data$AnyHealthcare)
clinical_data$NoDocbcCost <- as.factor(clinical_data$NoDocbcCost)
clinical_data$MentHlth <- as.factor(clinical_data$MentHlth)
clinical_data$Sex <- as.factor(clinical_data$Sex)
clinical_data$Income <- as.factor(clinical_data$Income)


## Summary
summary_table <- clinical_data %>%
  tbl_summary(
    by = Diabetes_binary,  
    statistic = all_continuous() ~ "{median} ({IQR})",  
    label = list(
      Age = "Age",
      BMI = "BMI",
      HighBP = "HighBP",
      HighChol = "HighChol",
      CholCheck = "CholCheck",
      Smoker = "Smoker",
      Stroke = "Stroke",
      HeartDiseaseorAttack = "HeartDiseaseorAttack",
      PhysActivity = "PhysActivity",
      Fruits = "Fruits",
      Veggies = "Veggies",
      HeavyAlcoholConsumption = "HvyAlcoholConsump",
      AnyHealthcare = "AnyHealthcare",
      NoDocbcCost= "NoDocbcCost",
      GenHlth = "GenHlth",
      MentHlth = "MentHlth",
      PhysHlth = "PhysHlth",
      DiffWalk = "DiffWalk",
      Sex = "Sex",
      Education = "Education",
      Income ="Income"
    ),
    missing = "ifany"  
  ) %>%
  add_p() %>%
  add_overall() %>%
  add_n() 

summary_table

describe(clinical_data)

# Heatmap
variables <- c("Diabetes_binary", "HighBP", "HighChol", "CholCheck", "BMI", "Smoker", 
               "Stroke", "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies", 
               "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "GenHlth", "MentHlth", 
               "PhysHlth", "DiffWalk", "Sex", "Age", "Education", "Income")
correlation_matrix <- cor(clinical_data[variables], use = "complete.obs")
melted_cormat <- melt(correlation_matrix)
ggplot(data = melted_cormat, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(label = round(value, 2)), color = "black", size =2) +  # 显示相关系数值
  labs(title = "Correlation Matrix Heatmap", x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# heatmap correlation
numeric_data <- clinical_data[, sapply(clinical_data, is.numeric)]
correlation_matrix <- cor(numeric_data, use="complete.obs")
corrplot(correlation_matrix, method="color", tl.col="black", tl.srt=45)
correlation_long <- as.data.frame(as.table(correlation_matrix))
high_correlations <- correlation_long %>%
  filter(abs(Freq) > 0.7 & abs(Freq) < 1)

# GLM
fit_columns <- c("HighBP", "HighChol", "CholCheck", "BMI", "Smoker", 
                 "Stroke", "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies", 
                 "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "GenHlth", "MentHlth", 
                 "PhysHlth", "DiffWalk", "Sex", "Age", "Education", "Income")
fit.hyt_1 <- glm(clinical_data$Diabetes_binary ~ ., data = clinical_data[, fit_columns], family = 'binomial')
summary(fit.hyt_1)
fit.hyt.null <- glm(clinical_data$Diabetes_binary ~ 1., data = clinical_data[, fit_columns], family = 'binomial')
final_model <- stepAIC(fit.hyt.null, scope=list(lower=fit.hyt.null,upper=fit.hyt_1), direction='back')
summary(final_model)
par(mfrow=c(2,2))
plot(final_model)
# cross validation
train_control <- trainControl(method="cv", number=10)
cv_model <- train(Diabetes_binary ~ ., data=clinical_data, method="glm", family=binomial, trControl=train_control)
print(cv_model)

# roc auc
probs <- predict(final_model, newdata=clinical_data, type="response")
roc_curve <- roc(clinical_data$Diabetes_binary, probs)
plot(roc_curve)
auc(roc_curve)

# calibration curve
calibration_curve <- calibration(Diabetes_binary ~ probs, data=clinical_data)
plot(calibration_curve)


