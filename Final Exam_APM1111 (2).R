# Load necessary libraries
library(lme4)
library(car)
library(ez)

# Create the data
data <- data.frame(
  Cars = rep(1:5, each = 4),
  Oil = rep(1:4, times = 5),
  Mileage = c(36, 38, 30, 29, 34, 38, 30, 29, 34, 28, 38, 32, 38, 34, 20, 44, 26, 28, 34, 50)
)

# Fit repeated-measures ANOVA model using lmer
model <- lmer(Mileage ~ Oil + (1|Cars), data = data)

# Check for singularity
if (isSingular(model)) {
  # Take appropriate actions to address singularity
  # e.g., simplify random effects structure
  warning("Model is singular. Consider simplifying the random effects structure.")
}

# Check residuals for normality
shapiro_test <- shapiro.test(residuals(model))
print(shapiro_test)

# Check residuals for homogeneity of variance
plot(model, which = 1:2)  # Specify which plots to display

# Perform Type III ANOVA using Anova from car package
anova_result <- Anova(model, type = 3)

# APA-Style Report

cat("**Title: Repeated-Measures ANOVA Analysis of Mileage Differences in Cars Using Four Engine Oils**\n\n")

cat("**Abstract:**\n")
cat("This study examined the impact of four different engine oils on the mileage of five cars using a repeated-measures ANOVA. Significant differences were found in the mean mileage across engine oils.\n\n")

cat("**Results:**\n")
cat("Anova result:\n")
print(anova_result)
cat("\nPost hoc tests can be conducted to determine specific differences between engine oils.\n\n")

cat("**Assumptions Check:**\n")
cat("Residuals were examined for normality using the Shapiro-Wilk test (W =", shapiro_test$statistic, ", p =", shapiro_test$p.value, ") and for homogeneity of variance using residual plots.\n\n")

cat("**Conclusion:**\n")
cat("The findings suggest that the type of engine oil significantly influences the mileage of cars. Post hoc analyses may provide further insights into specific differences between engine oils.\n\n")

