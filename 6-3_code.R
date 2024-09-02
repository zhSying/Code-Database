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
file_path <- "C:/Users/思/Desktop/Dataset/6.3-diabetes_binary_health_indicators_BRFSS2021.csv"  
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
clinical_data$Sex <- as.factor(clinical_data$Sex)
clinical_data$Income <- as.factor(clinical_data$Income)
clinical_data$Diabetes_binary <- as.factor(clinical_data$Diabetes_binary)
clinical_data$GenHlth <- as.factor(clinical_data$GenHlth)
clinical_data$DiffWalk <- as.factor(clinical_data$DiffWalk)
clinical_data$Education <- as.factor(clinical_data$Education)

clinical_data <- clinical_data %>%
  mutate(
    # General Health
    GenHlth = case_when(
      GenHlth == 1 ~ "Excellent",
      GenHlth == 2 ~ "Very good",
      GenHlth == 3 ~ "Good",
      GenHlth == 4 ~ "Fair",
      GenHlth == 5 ~ "Poor"
    ),
    
    # Education Level
    Education = case_when(
      Education == 1 ~ "No education",
      Education == 2 ~ "Elementary",
      Education == 3 ~ "Some high school",
      Education == 4 ~ "High school graduate",
      Education == 5 ~ "Some college",
      Education == 6 ~ "College graduate"
    ),
    
    # Income Levels
    Income = case_when(
      Income == 1 ~ "<$10k",
      Income == 2 ~ "<$15k",
      Income == 3 ~ "<$20k",
      Income == 4 ~ "<$25k",
      Income == 5 ~ "<$35k",
      Income == 6 ~ "<$50k",
      Income == 7 ~ "<$75k",
      Income == 8 ~ "<$100k",
      Income == 9 ~ "<$150k",
      Income == 10 ~ "<$200k",
      Income == 11 ~ "$200k+"
    ),
    DiffWalk = case_when(
      DiffWalk == 1 ~ "Yes",
      DiffWalk == 0 ~ "No",
      TRUE ~ as.character(DiffWalk)
    ),
    
    Diabetes_binary = case_when(
      Diabetes_binary == 1 ~ "Yes",
      Diabetes_binary == 0 ~ "No",
      TRUE ~ as.character(Diabetes_binary)
    ),
    
    HighBP = case_when(
      HighBP == 1 ~ "Yes",
      HighBP == 0 ~ "No",
      TRUE ~ as.character(HighBP)
    ),
    
    HighChol = case_when(
      HighChol == 1 ~ "Yes",
      HighChol == 0 ~ "No",
      TRUE ~ as.character(HighChol)
    ),
    
    CholCheck = case_when(
      CholCheck == 1 ~ "Yes",
      CholCheck == 0 ~ "No",
      TRUE ~ as.character(CholCheck)
    ),
    
    Smoker = case_when(
      Smoker == 1 ~ "Yes",
      Smoker == 0 ~ "No",
      TRUE ~ as.character(Smoker)
    ),
    
    Stroke = case_when(
      Stroke == 1 ~ "Yes",
      Stroke == 0 ~ "No",
      TRUE ~ as.character(Stroke)
    ),
    
    HeartDiseaseorAttack = case_when(
      HeartDiseaseorAttack == 1 ~ "Yes",
      HeartDiseaseorAttack == 0 ~ "No",
      TRUE ~ as.character(HeartDiseaseorAttack)
    ),
    
    PhysActivity = case_when(
      PhysActivity == 1 ~ "Yes",
      PhysActivity == 0 ~ "No",
      TRUE ~ as.character(PhysActivity)
    ),
    
    Fruits = case_when(
      Fruits == 1 ~ "Yes",
      Fruits == 0 ~ "No",
      TRUE ~ as.character(Fruits)
    ),
    
    Veggies = case_when(
      Veggies == 1 ~ "Yes",
      Veggies == 0 ~ "No",
      TRUE ~ as.character(Veggies)
    ),
    
    HvyAlcoholConsump = case_when(
      HvyAlcoholConsump == 1 ~ "Yes",
      HvyAlcoholConsump == 0 ~ "No",
      TRUE ~ as.character(HvyAlcoholConsump)
    ),
    
    AnyHealthcare = case_when(
      AnyHealthcare == 1 ~ "Yes",
      AnyHealthcare == 0 ~ "No",
      TRUE ~ as.character(AnyHealthcare)
    ),
    
    NoDocbcCost = case_when(
      NoDocbcCost == 1 ~ "Yes",
      NoDocbcCost == 0 ~ "No",
      TRUE ~ as.character(NoDocbcCost)
    ),
    
    Sex = case_when(
      Sex == 1 ~ "Male",
      Sex == 0 ~ "Female",
      TRUE ~ as.character(Sex)
    )
  )


###Categorical feature
titles <- c("Diabetes Awareness",
            "High Blood Pressure Awareness",
            "High Cholesterol Awareness",
            "Cholesterol Check in Last 5 Years",
            "Smoked at Least 100 Cigarettes in Lifetime",
            "History of Stroke",
            "History of Heart Disease",
            "Engaged in Physical Activity")
features <- c("Diabetes_binary",
              "HighBP",
              "HighChol",
              "CholCheck",
              "Smoker",
              "Stroke",
              "HeartDiseaseorAttack",
              "PhysActivity")
create_bar_plot <- function(feature,title) {
  ggplot(clinical_data, aes_string(x = feature)) +
    geom_bar(fill = "purple", stat = "count") +
    labs(title = title, x = NULL, y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(size = 11))
}
plot_list <- mapply(create_bar_plot, features, titles, SIMPLIFY = FALSE)
plot_grid(plotlist = plot_list, ncol = 4)

titles <- c("Consumes Fruits at Least Once a Day",
            "Consumes Vegetables at Least Once a Day",
            "Heavy Alcohol Consumption",
            "Has Any Form of Health Insurance",
            "Did Not See Doctor Due to Cost",
            "General Health Status",
            "Difficulty Walking or Climbing Stairs",
            "Gender")
features <- c("Fruits",
              "Veggies" ,
              "HvyAlcoholConsump",
              "AnyHealthcare",
              "NoDocbcCost",
              "GenHlth",
              "DiffWalk",
              "Sex")
create_bar_plot <- function(feature,title) {
  ggplot(clinical_data, aes_string(x = feature)) +
    geom_bar(fill = "purple", stat = "count") +
    labs(title = title, x = NULL, y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(size = 11))
}
plot_list <- mapply(create_bar_plot, features, titles, SIMPLIFY = FALSE)
plot_grid(plotlist = plot_list, ncol = 4)
titles <- c("Education Level",
            "Household Income")
features <- c("Education",
              "Income")
create_bar_plot <- function(feature,title) {
  ggplot(clinical_data, aes_string(x = feature)) +
    geom_bar(fill = "purple", stat = "count") +
    labs(title = title, x = NULL, y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(size = 11))
}
plot_list <- mapply(create_bar_plot, features, titles, SIMPLIFY = FALSE)
plot_grid(plotlist = plot_list, ncol = 2)

#chisq
categorical_vars <- c("HighBP", "HighChol", "CholCheck", "Smoker", "Stroke", 
                      "HeartDiseaseorAttack", "PhysActivity", "Fruits", 
                      "Veggies", "HvyAlcoholConsump", "AnyHealthcare", 
                      "NoDocbcCost", "GenHlth", "DiffWalk", "Sex", 
                      "Education", "Income")

chi_square_results <- lapply(categorical_vars, function(var) {
  chisq.test(table(clinical_data[[var]], clinical_data$Diabetes_binary))
})
summary(chi_square_results)



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


# PhysHlth
PhysHlth_distribution <- ggplot(clinical_data, aes(x = PhysHlth)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.5) +
  geom_density(aes(y = ..count.. * 5), color = "black") +
  labs(title = "PhysHlth Distribution", x = "PhysHlth", y = "Count") +
  theme_minimal()+
  scale_x_continuous(limits = c(0,30)) 

PhysHlth_boxplot <- ggplot(clinical_data, aes(x = "", y = PhysHlth)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  labs(title = "PhysHlth Boxplot", x = "", y = "PhysHlth") +
  theme_minimal()

plot_grid(GenHlth_distribution, GenHlth_boxplot, ncol = 2)
# Plot
plot_grid(age_distribution, age_boxplot,
          bmi_distribution, bmi_boxplot,
          MentHlth_distribution, MentHlth_boxplot,
          PhysHlth_distribution, PhysHlth_boxplot, ncol = 4)
# glm
continuous_vars <- clinical_data[, c("Age", "BMI", "MentHlth", "PhysHlth")]
logistic_models <- lapply(names(continuous_vars), function(var) {
  formula <- as.formula(paste("Diabetes_binary ~", var))
  glm(formula, data = clinical_data, family = binomial)
})

lapply(logistic_models, summary)


## Summary
summary_table <- clinical_data %>%
  tbl_summary(
    by = Diabetes_binary,  
    statistic = all_continuous() ~ "{median} ({IQR})",  
    percent = "row", 
    label = list(
      HighBP = "High Blood Pressure Awareness",
      HighChol = "High Cholesterol Awareness",
      CholCheck = "Cholesterol Check(Last 5Y)",
      Smoker = "Smoked Cigarettes(>100) in Lifetime",
      Stroke = "History of Stroke",
      HeartDiseaseorAttack = "History of Heart Disease",
      PhysActivity = "Engaged in Physical Activity",
      Fruits = "Consumes Fruits at Least Once a Day",
      Veggies = "Consumes Vegetables at Least Once a Day",
      HvyAlcoholConsump = "Heavy Alcohol Consumption",
      AnyHealthcare = "Has Any Form of Health Insurance",
      NoDocbcCost = "Did Not See Doctor Due to Cost",
      GenHlth = "General Health Status",
      DiffWalk = "Difficulty Walking or Climbing Stairs",
      Sex = "Gender",
      Age = "Age Group (5-Year Intervals)",
      Education = "Education Level",
      Income = "Household Income"
    ),
    missing = "ifany"  
  ) %>%
  add_p() %>%
  add_overall() %>%
  add_n() 


summary_table

describe(clinical_data)
full_model <- glm(Diabetes_binary ~ Age + BMI + MentHlth + PhysHlth + HighBP + HighChol + 
                    CholCheck + Smoker + Stroke + HeartDiseaseorAttack + PhysActivity + 
                    Fruits + Veggies + HvyAlcoholConsump + AnyHealthcare + NoDocbcCost + 
                    GenHlth + DiffWalk + Sex + Education + Income, 
                  data = clinical_data, family = binomial)

# VIF
vif_values <- vif(full_model)
print(vif_values)

table(clinical_data$Diabetes_binary)

# GLM
# Fit the full model with all predictors
fit_hyt_1 <- glm(clinical_data$Diabetes_binary ~ ., data = clinical_data[, fit_columns], family = binomial())

# Fit the null model with only the intercept
fit_hyt_null <- glm(clinical_data$Diabetes_binary ~ 1, data = clinical_data[, fit_columns], family = binomial())

# Perform stepwise model selection using AIC, starting from the full model
final_model <- stepAIC(fit_hyt_1, 
                       scope = list(lower = fit_hyt_null, upper = fit_hyt_1), 
                       direction = 'backward')

# Display the summary of the final selected model
summary(final_model)

par(mfrow=c(2,2))
plot(final_model)

# cross validation
train_control <- trainControl(method="cv", number=10)
cv_model <- train(Diabetes_binary ~ ., data=clinical_data, method="glm", family=binomial, trControl=train_control)
print(cv_model)
accuracies <- cv_model$resample$Accuracy
kappas<- cv_model$resample$Kappa
accuracy_ci <- quantile(accuracies, probs = c(0.025, 0.975))
kappa_ci <- quantile(kappas, probs = c(0.025, 0.975))
print(paste("Accuracy 95% CI:", accuracy_ci))
print(paste("Kappa 95% CI:", kappa_ci))

# roc auc
probs <- predict(final_model, newdata=clinical_data, type="response")
roc_curve <- roc(clinical_data$Diabetes_binary, probs)
plot(roc_curve)
auc_value <- auc(roc_curve)
auc_ci <- ci.auc(roc_curve)
print(auc_ci)

# Calibration curve
par(mfrow=c(1,1))
predicted_prob <- predict(final_model, type = "response")

calibration_plot <- calibration(as.factor(clinical_data$Diabetes_binary) ~ predicted_prob, 
                                data = clinical_data, 
                                class = "1", 
                                cuts = 10)

ggplot(calibration_plot) + 
  labs(x = "Predicted Probability (%)", 
       y = "Observed Event Percentage") +
  theme_minimal()

#new data
new_patient <- data.frame(
  HighBP = 1,
  HighChol = 0,
  CholCheck = 1,
  BMI = 30,
  Smoker = 0,
  Stroke = 0,
  HeartDiseaseorAttack = 0,
  PhysActivity = 1,
  Fruits = 1,
  Veggies = 1,
  HvyAlcoholConsump = 0,
  AnyHealthcare = 1,
  NoDocbcCost = 0,
  GenHlth = 3,
  MentHlth = 0,
  PhysHlth = 2,
  DiffWalk = 0,
  Sex = 1,
  Age = 50,
  Education = 5,
  Income = 4
)

predicted_risk <- predict(final_model, newdata = new_patient, type = "response")
print(predicted_risk)
