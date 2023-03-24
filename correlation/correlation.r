# Five different PFASs were measured in 39 individuals.
library(tidyverse)
library(corrplot)
library(readr)

df <- read_csv("Question6.csv")
df <- df[-1]

# Run a correlation matrix. Largest number is most correlated.
# Lowest number is least correlated.
corr_matrix <- cor(df)
print(corr_matrix)
corrplot(corr_matrix, type = "upper", method = "circle")

# Run correlation matrix with p-values.
library(Hmisc)
rcorr(as.matrix(df), type = "pearson")

# Make a corrgram.
library(corrgram)
corrgram(corr_matrix, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main="PFAS")