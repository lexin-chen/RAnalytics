# In a series of paired experiments,
# 10 batches of samples were split between two measurement methods,
# one analyzed using UV and one using near IR.
# Is there significant difference between the two methods “Question3.csv”.

library(tidyverse)
library(readr)
df <- read_csv("Question3.csv")

# 3a shapiro-wilk test to see if the data is normal distribution
shapiro.test(df$UV)
shapiro.test(df$NearIR)

# 3b Finding the degrees of freedom and the p-value.
t.test(df$UV, df$NearIR, paired = TRUE)
