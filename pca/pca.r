# PCA of the iris dataset in R. 
# The first two principal components explain 96.8% of the variance in the data.

iris_std <- scale(iris[,1:4])
pc <- prcomp(iris_std)
plot(pc, type="l", main="Scree Plot of Iris PC Analysis")

# Make a scores plot with each species with 95% ellipses. 
library(ggplot2)
library(dplyr)
log.iris <- log(iris[, 1:4])
iris.species <- iris[, 5]
pca <- prcomp(log.iris, center = TRUE,scale = TRUE)
scores <- as.data.frame(pca$x[,1:2])
scores$Species <- iris$Species
# I had to find an ggplot alternative because I had trouble installing ggbiplot.
ggplot(scores, aes(x = PC1, y = PC2, color = Species)) +
  geom_point() +
  stat_ellipse(aes(fill = Species), level = 0.95, alpha = 0.2, size = 0.5) +
  xlab("PC1") + ylab("PC2") +
  ggtitle("PCA Scores Plot with 95% Ellipses")

# Create a loadings plot, which shows the correlation between
# the original variables and the principal components,
# and the most influential variables per PC.

pca <- prcomp(log.iris, center = TRUE, scale. = TRUE)
loadings <- pca$rotation
plot(NULL, xlim = c(-1, 1), ylim = c(-1, 1), xlab = "PC1", ylab = "PC2")
arrows(0, 0, loadings[,1], loadings[,2], length = 0.1, col = "#0047ab")
text(loadings[, 1], loadings[, 2], labels = names(iris)[1:4], pos = 3)