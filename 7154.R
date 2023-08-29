# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the dataset
diabetes <- read.csv("diabetes.csv")

# Data cleaning and restructuring

# Renaming variables for better readability
colnames(diabetes) <- c("Diabetes", "HighBP", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke", "HeartDiseaseorAttack", 
                        "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", 
                        "GenHlth", "MentHlth", "PhysHlth", "DiffWalk", "Sex", "Age", "Education", "Income")

# Check for missing values
missing_values <- sapply(diabetes, function(x) sum(is.na(x)))
print(missing_values)

# Check for duplicate rows
duplicate_rows <- sum(duplicated(diabetes))
print(duplicate_rows)

# Remove duplicate rows
diabetes <- distinct(diabetes)
# Data Analysis using descriptive statistics


# Data Analysis using descriptive statistics

# Summary statistics
summary_stats <- summary(diabetes)
print(summary_stats)

# Visualization of data using histograms
histograms <- diabetes %>%
  gather(variable, value, Diabetes:Income) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20) +
  facet_wrap(~variable, scales = "free_x") +
  labs(title = "Histograms of Various Variables")

print(histograms)



# Create new variables if needed (e.g., categorize BMI into underweight, normal, overweight, obese)
diabetes <- diabetes %>%
  mutate(BMI_Category = ifelse(BMI < 18.5, "Underweight",
                               ifelse(BMI >= 18.5 & BMI < 24.9, "Normal",
                                      ifelse(BMI >= 24.9 & BMI < 29.9, "Overweight",
                                             "Obese"))))



# Hypothesis testing

# Research Question 1: Is there a significant association between BMI category and diabetes status?
contingency_table1 <- table(diabetes$BMI_Category, diabetes$Diabetes)
chi_sq_test1 <- chisq.test(contingency_table1)
print(chi_sq_test1)


# Research Question 2: Is there a significant association between high cholesterol and diabetes status?
contingency_table2 <- table(diabetes$HighChol, diabetes$Diabetes)
chi_sq_test2 <- chisq.test(contingency_table2)
print(chi_sq_test2)

# Research Question 3: Is there a significant association between physical activity and diabetes status?
contingency_table3 <- table(diabetes$PhysActivity, diabetes$Diabetes)
chi_sq_test3 <- chisq.test(contingency_table3)
print(chi_sq_test3)

# Research Question 4: Is there a significant association between smoking and diabetes status?
contingency_table4 <- table(diabetes$Smoker, diabetes$Diabetes)
chi_sq_test4 <- chisq.test(contingency_table4)
print(chi_sq_test4)

# Research Question 5: Is there a significant association between education level and diabetes status?
contingency_table5 <- table(diabetes$Education, diabetes$Diabetes)
chi_sq_test5 <- chisq.test(contingency_table5)
print(chi_sq_test5)

