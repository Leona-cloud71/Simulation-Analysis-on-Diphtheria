Diphtheria



# Load necessary libraries
library(readr)
library(ggplot2)
library(dplyr)
library(corrplot)
library(ggcorrplot)



# Load the dataset

Diphtheria <- read_csv("C:/Users/Lib 003/Desktop/Diphtheria/Diphtheria.csv")

# View the structure and summary
str(Diphtheria)

summary(Diphtheria)

# Descriptive statistics for numeric columns

numeric_summary <- Diphtheria %>%
  select(Suspectec_cases, Deaths, CFR, Lab_confirmed, Epidemiologically_linked, Clinical_compatible, Total_confirmed) %>%
  summary()
print(numeric_summary)


# Bar plot of suspected cases across countries

ggplot(Diphtheria, aes(x = Country, y = Suspectec_cases, fill = Country)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Suspectec_cases), vjust = -0.3, size = 3.5) + # Add labels on top
  theme_minimal() +
  labs(title = "Suspected Cases by Country", x = "Country", y = "Suspected Cases")


# Bar plot of deaths across countries
ggplot(Diphtheria, aes(x = Country, y = Deaths, fill = Country)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Deaths), vjust = -0.3, size = 3.5) + # Add labels on top
  theme_minimal() +
  labs(title = "Deaths by Country", x = "Country", y = "Number of Deaths")


# Bar plot of CFR across countries

ggplot(Diphtheria, aes(x = Country, y = CFR, fill = Country)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = CFR), vjust = -0.3, size = 3.5) + # Add labels on top
  theme_minimal() +
  labs(title = "Case Fatality Rate (CFR) by Country", x = "Country", y = "CFR (%)")



# Calculate the correlation Heap matrix
correlation_matrix <- cor(Diphtheria, use = "complete.obs")

# Create the lower triangular correlation matrix heatmap
ggcorrplot(correlation_matrix, method = "circle", type = "lower", lab = TRUE, lab_size = 3, colors = c("green", "white", "orange"), title = "Correlation Heap Matrix", ggtheme = theme_minimal() )







################Correlation matrix 

selected_columns <- Diphtheria %>% 
  select(Suspectec_cases, Deaths, CFR, Lab_confirmed, Epidemiologically_linked, Clinical_compatible, Total_confirmed)

# Ensure all columns are numeric
selected_columns <- selected_columns %>% mutate_all(as.numeric)

# Calculate the correlation matrix
correlation_matrix <- cor(selected_columns, use = "complete.obs")

# Plot the correlation matrix
corrplot(correlation_matrix, method = "color", 
         tl.col = "green", tl.srt = 45, 
         addCoef.col = "white", number.cex = 0.7, 
         title = "Correlation Matrix", mar=c(0,0,1,0))



# Test the significance of correlations
cor_test <- cor.test(Diphtheria$Suspectec_cases, Diphtheria$Deaths)
print(cor_test)



# Test the significance of correlations
cor_test <- cor.test(Diphtheria$Lab_confirmed, Diphtheria$Deaths)
print(cor_test)




# Calculate proportion

Diphtheria <- Diphtheria %>%
  mutate(Proportion_Confirmed = (Total_confirmed / Suspectec_cases) * 100)

# Bar plot of proportion of confirmed cases across countries

ggplot(Diphtheria, aes(x = Country, y = Proportion_Confirmed, fill = Country)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion_Confirmed, 1)), vjust = -0.3, size = 3.5) + # Add labels on top
  theme_minimal() +
  labs(title = "Proportion of Confirmed Cases relative to Suspected Case by Country", x = "Country", y = "Proportion (%)")



# Multiple linear regression to predict Total Confirmed Cases

regression_model <- lm(Deaths ~ Lab_confirmed + Epidemiologically_linked + Clinical_compatible, data = Diphtheria)
summary(regression_model)

# Visualize the model’s predictions

Diphtheria$Predicted_Confirmed <- predict(regression_model)

ggplot(Diphtheria, aes(x = Deaths, y = Predicted_Confirmed, label = Country)) +
  geom_point(color = "blue") +
  geom_text(vjust = -1, size = 3.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual Confirmed Cases", x = "Actual Confirmed Cases", y = "Predicted Confirmed Cases") +
  theme_minimal()









# Convert CFR into a binary variable (e.g., high CFR if >5%)

Diphtheria$High_CFR <- ifelse(Diphtheria$CFR > 5, 1, 0)

# Logistic regression model
logistic_model <- glm(High_CFR ~ Suspectec_cases + Lab_confirmed + Epidemiologically_linked + Clinical_compatible, data = Diphtheria, family = "binomial")
summary(logistic_model)

# Predict probabilities
Diphtheria$Predicted_Prob <- predict(logistic_model, type = "response")

# Plot predicted probabilities
ggplot(Diphtheria, aes(x = Country, y = Predicted_Prob, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Predicted Probability of High CFR by Country", x = "Country", y = "Predicted Probability") +
  theme_minimal()































