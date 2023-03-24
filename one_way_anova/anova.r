# Evaluate the effect of the Cond01 - Cond04. 
library(tidyverse)
library(readr)
df <- read_csv("LipidomicsDBS_PS.csv")

# Anova test tests for significant difference between >=2 categorical variables.
# Summary gives degrees of freedom for condition, residuals and p-value.
anova_results <- aov(df$SumPS ~ df$Condition, data = df)
summary(anova_results)

# Generate a table of means and grand mean for each condition.
means <- aggregate(SumPS ~ Condition, data = df, mean)
means
mean(means$SumPS)

# Boxplot for the four experimental conditions.
library(ggplot2)
colors <- c("#00ab64", "#0047ab", "#ab0047", "#ab6400")
ggplot(df, aes(x = df$Condition, y = df$SumPS, fill = Condition)) +
  geom_boxplot() +
  scale_fill_manual(values = colors) +
  labs(title = "Box plot of SumPS vs. Conditions",
       x = "Conditions", y = "SumPS")

# Tukey HSD test to find the p-value for each pair of conditions.
t_95 <- TukeyHSD(anova_results, conf.level = 0.95)
t_99 <- TukeyHSD(anova_results, conf.level = 0.99)
# post hoc test at 95% confidence interval
plot(t_95, las = 1)
# post hoc test at 99% confidence interval
plot(t_99, las = 1)