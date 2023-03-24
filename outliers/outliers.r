# Compare PlantGrowth dataset with Grubbs test and Iglewicz-Hoaglin test
library(glue)
PlantGrowth
data_groups <- split(PlantGrowth, PlantGrowth$group)
library(outliers)

# Grubb's test to calculate G', G'', and G'''.
# for each group in 95% and 99% confidence intervals.
for (group in names(data_groups)) {
  print(group)
  print(grubbs.test(data_groups[[group]]$weight, type=10, two.sided = TRUE))
}

for (group in names(data_groups)) {
  print(group)
  print(grubbs.test(data_groups[[group]]$weight, type=10, two.sided = TRUE, opposite=TRUE))
}

for (group in names(data_groups)) {
  print(group)
  print(grubbs.test(data_groups[[group]]$weight, type=11, two.sided = TRUE))
}

for (group in names(data_groups)) {
  print(group)
  print(grubbs.test(data_groups[[group]]$weight, type=20, two.sided = TRUE))
}

for (group in names(data_groups)) {
  print(group)
  print(grubbs.test(data_groups[[group]]$weight, type=20, two.sided = TRUE, opposite=TRUE))
}

library(stats)
library(outliers)

# Find the medium average deviation for small dataset (MADe) for each group.
mad(PlantGrowth$weight)

# Iglewicz-Hoaglin test to calculate the modified Z score (Mi) for each group.
# Function from: rdrr.io/github/skinnider/modern/man/iglewicz_hoaglin.html
iglewicz_hoaglin = function(x, threshold = 3.5, return_scores = F) {
  # check input
  if (!is.numeric(x)) 
    stop("could not identify outliers: input is not numeric")
  # calculate modified Z scores
  med = median(x, na.rm = T)
  MAD = median(abs(x - med), na.rm = T)
  Mi = 0.6745 * (x - med) / MAD
  if (return_scores) {
    return(Mi)
  } else {
    # mask vector
    x[abs(Mi) > threshold] = NA
    return(x)
  }
}
iglewicz_hoaglin(PlantGrowth$weight, threshold = 3.5, return_scores = T)