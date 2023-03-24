# Accessing the relationship between tree height and volume in the trees dataset.
x <- trees$Height
y <- trees$Volume

#7a
# Linear regression
l_model <- lm(y ~ x, data = trees)
summary(l_model)
# Quadratic regression
q_model <- lm(y ~ x + I(x^2), data = df)
summary(q_model)

# Scatter of linear regression because better fit than quadratic from p-value.
library(ggplot2)
df <- trees
x <- trees$Height
y <- trees$Volume
ggplot(data = df, aes(x = x, y = y)) +
  geom_point() +
  labs(title = "Scatter plot of y against x",
       x = "x", y = "y")

ggplot(data = df, aes(x = x, y = y)) +
  geom_point(color = "#0047ab") + 
  geom_smooth(method = "lm", formula = y ~ x + I(x), se = FALSE, color = "#009dab") +
  labs(title = "Trees Height vs Volume | y=1.5433x-87.124",
       x = "Height", y = "Volume")

# gvlma: globally test assumptions.
library(gvlma)
library(car)
gvlma(l_model)
# Influence plot and hat values to check for outliers.
influencePlot(l_model)
hatvalues(l_model)