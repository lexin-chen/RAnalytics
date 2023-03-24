# In a series of experiments on the determination of tin in foodstuffs,
# samples were boiled with hydrochloric acid under reflux for different times
# (M1 = 30 min and M2 = 75 min). Results are found in file “Question2.csv”.

library(tidyverse)
library(readr)

df <- read_csv("E:\\midterm\\Question2.csv")
M1_df <- subset(df, Method == "M1")
M2_df <- subset(df, Method == "M2")

# 2a use levene test to see if two groups var is equal
leveneTest(M1_df$Tin, M2_df$Tin)

#2c perform independent t-test, var.equal is true because 2a
t.test(M1_df$Tin, M2_df$Tin, var.equal=TRUE)

# 2d mathematical method is shapiro-wilk.
# Shapiro-wilk test is used to test if the data is normal distribution
shapiro.test(M1_df$Tin)
shapiro.test(M2_df$Tin)
qqplot(qqnorm(M1_df$Tin, main="QQ plot for M1"))
qqplot(qqnorm(M2_df$Tin, main="QQ plot for M2"))
par(mfrow=c(1,2))