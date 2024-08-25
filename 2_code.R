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
file_path <- "C:/Users/思/Desktop/Dataset/2-Early Stage Diabetes Risk Prediction.csv" 
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

# Missing value analysis
total_missing <- sum(is.na(clinical_data))
total_data_points <- prod(dim(clinical_data))
overall_missing_rate <- total_missing / total_data_points
# Print missing value result
print(paste("Total Missing Values:", total_missing))
print(paste("Total Data Points:", total_data_points))
print(paste("Overall Missing Rate:", round(overall_missing_rate, 4)))

###Categorical Variables 
features <- c("Gender", "Polyuria", "Polydipsia", "`sudden weight loss`", "weakness", "Polyphagia", "`Genital thrush`", "`visual blurring`")
create_bar_plot <- function(feature) {
  ggplot(clinical_data, aes_string(x = feature)) +
    geom_bar(fill = "purple", stat = "count") +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
    labs(title = feature, x = NULL, y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(size = 20))
}
plot_list <- lapply(features, create_bar_plot)
plot_grid(plotlist = plot_list, ncol = 4)

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

#Data processing
clinical_data <- clinical_data %>%
  mutate(
    Gender = ifelse(Gender == "Male", 1, 0),
    across(everything(), ~ ifelse(. == "Yes", 1, ifelse(. == "No", 0, .)))
  )
clinical_data$Gender <- as.factor(clinical_data$Gender)

## Summary table
summary_table <- clinical_data %>%
  tbl_summary(
    by = class, 
    statistic = all_continuous() ~ "{median} ({IQR})", 
    label = list(
      Age = "Age",
      Gender = "Gender",
      Polyuria = "Polyuria",
      Polydipsia = "Polydipsia",
      sudden_weight_loss = "Sudden Weight Loss",
      weakness = "Weakness",
      Polyphagia = "Polyphagia",
      Genital_thrush = "Genital Thrush",
      visual_blurring = "Visual Blurring",
      Itching = "Itching",
      Irritability = "Irritability",
      delayed_healing = "Delayed Healing",
      partial_paresis = "Partial Paresis",
      muscle_stiffness = "Muscle Stiffness",
      Alopecia = "Alopecia",
      Obesity = "Obesity"
    ),
    missing = "ifany"  
  ) %>%
  add_p() %>%
  add_overall() %>%
  add_n() 

summary_table
clinical_data$class <- ifelse(clinical_data$class == "Positive", 1, 0)
clinical_data <- as.data.frame(lapply(clinical_data, function(x) ifelse(x == "Yes", 1, ifelse(x == "No", 0, x))))
clinical_data$Gender <- ifelse(clinical_data$Gender == "Male", 1, 0)
# GLM
names(clinical_data)
fit_columns <- c("Age", "Gender", "Polyuria", "Polydipsia", "sudden.weight.loss", 
                 "weakness", "Polyphagia", "Genital.thrush", "visual.blurring", 
                 "Itching", "Irritability", "delayed.healing", "partial.paresis", 
                 "muscle.stiffness", "Alopecia", "Obesity")
fit.1 <- glm(clinical_data$class ~ ., data = clinical_data[, fit_columns], family = 'binomial')
summary(fit.1)
fit.null <- glm(clinical_data$class ~ 1., data = clinical_data[, fit_columns], family = 'binomial')
stepwise_model <- stepAIC(fit.null, scope=list(lower=fit.null,upper=fit.1), direction='both')
summary(stepwise_model)

fit.final <- glm(clinical_data$class ~ Polyuria + Polydipsia + Gender + 
                   Itching + Irritability + Genital.thrush + partial.paresis + 
                   Polyphagia + Age + weakness, data = clinical_data, family = 'binomial')
anova(fit.final, fit.1, test='LRT')
summary(fit.final)
par(mfrow=c(2,2))
plot(fit.final)
predicted_probs <- predict(fit.final, type = "response")

# Create and plot the ROC curve
roc_curve <- roc(clinical_data$class, predicted_probs)
plot(roc_curve, col = "blue", main = "ROC Curve")
par(mfrow=c(1,1))
# Calculate and display AUC
auc_value <- auc(roc_curve)
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "blue", lwd = 2)

