#Load csv file containing viral loads
monkeypox_data <- read.csv("MPXV.csv")

#Checking for any missing values
str(monkeypox_data)

#Displaying data types and summary
summary(monkeypox_data)

#Summarising data based on gender 
library(dplyr)
monkeypox_data %>%
  group_by(Sex) %>%
  summarize(
    mean_viral_load = mean(Monkeypox_Viral_Load.),
    median_viral_load = median(Monkeypox_Viral_Load.),
    sd_viral_load = sd(Monkeypox_Viral_Load.),
    iqr_viral_load = IQR(Monkeypox_Viral_Load.)
  )
#Visualization the data
#loading necessary plots
library(ggplot2) 

ggplot(monkeypox_data, aes(x = Sex, y = Monkeypox_Viral_Load.)) +
  geom_boxplot() +  
  labs(title = "Viral Load Distribution by Gender", x = "Sex", y = "Monkeypox Viral Load (DNA copy/ml)") +
  theme_bw()  

#Statistical Test (assuming normality)
#checking for normality using Shapiro-Wilk Test
library(nortest)
ad_test_result <- ad.test(monkeypox_data$Monkeypox_Viral_Load.)
cat("Normality test (entire data): ", ad_test_result$statistic, ", p-value: ", ad_test_result$p.value, "\n")

#Test for males
shapiro_test_male <- shapiro.test(monkeypox_data$Monkeypox_Viral_Load.[monkeypox_data$Sex == "M"])
cat("Normality test (males): ", shapiro_test_male$statistic, ", p-value: ", shapiro_test_male$p.value, "\n")
#wilcox.test(monkeypox_data$Monkeypox_Viral_Load. ~ monkeypox_data$Sex, alternative = "two.sided")

#Test for females
if (nrow(monkeypox_data[monkeypox_data$Sex == "F"]) >= 3) {
  ad_test_result_female <- ad.test(monkeypox_data$Monkeypox_Viral_Load.[monkeypox_data$Sex == "F"])
  cat("Normality test (females): ", ad_test_result_female$statistic, ", p-value: ", ad_test_result_female$p.value)
} else {
    cat("Sample size for females is too small for Anderson-Darling test. ")
}

#Visual inspection
#Visualization for histogram
hist(monkeypox_data$Monkeypox_Viral_Load.)

#Example for QQ plot
qqnorm(monkeypox_data$Monkeypox_Viral_Load.)
qqline(monkeypox_data$Monkeypox_Viral_Load.)

#Filtering the female data
female_data <- monkeypox_data[monkeypox_data$Sex == "F", ]

#QQ plot for females for checking normality
qqnorm(female_data$Monkeypox_Viral_Load.)
qqline(female_data$Monkeypox_Viral_Load.)
title("QQ Plot of  Monkeypox Viral Load (Females)")
xlab("Quantiles of Standard Normal Distribution")
ylab("Monkeypox Viral Load (DNA copy/ml)")

#Finding the maximum and minimum value
min_val <- min(monkeypox_data$Monkeypox_Viral_Load.)
max_val <- max(monkeypox_data$Monkeypox_Viral_Load.)
print(min_val)
print(max_val)
#Normalize the viral load data(create a new column)
monkeypox_data$Normalized_Viral_Load <- (monkeypox_data$Monkeypox_Viral_Load. - min_val) / (max_val - min_val)

#Checking Normalisation again
hist(monkeypox_data$Normalized_Viral_Load)
title("Normalized Viral Load")
ylab("Frequency")

qqnorm(monkeypox_data$Normalized_Viral_Load)
qqline(monkeypox_data$Normalized_Viral_Load)
title("QQ Plot of Normalized Viral Load")
xlab("Quantiles of Standard Normal Distribution")
ylab("Normalized Viral Load")

#Test for differences in Viral load between genders
t_test_result <- t.test(monkeypox_data$Normalized_Viral_Load ~ monkeypox_data$Sex, var.equal = TRUE, alternative = "two.sided")
print(t_test_result)
summary(t_test_result)

#For Mann-Whitney U test(Non-normal data)
wilcox_test_result <- wilcox.test(monkeypox_data$Normalized_Viral_Load ~ monkeypox_data$Sex, alternative = "two.sided")
print(wilcox_test_result)
summary(wilcox_test_result)
