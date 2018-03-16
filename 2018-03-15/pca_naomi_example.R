library(rattle.data)
library(ggplot2)
data("wine")

#unscaled
wine_pca <- prcomp(as.matrix(wine[,-1]))
plot(wine_pca)
screeplot(wine_pca, type = "lines")
biplot(wine_pca)

pca_data <- as.data.frame(wine_pca$x)
pca_data$Type = wine$Type

ggplot(pca_data, aes(x=PC1, y=PC2, color=Type)) + geom_point()

#scaled
wine_pca_scaled <- prcomp(as.matrix(wine[,-1]), center = TRUE, scale=TRUE)
plot(wine_pca_scaled)
screeplot(wine_pca_scaled, type = "lines")
biplot(wine_pca_scaled)

pca_data_scaled <- as.data.frame(wine_pca_scaled$x)
pca_data_scaled$Type = wine$Type

library(ggplot2)
ggplot(pca_data_scaled, aes(x=PC1, y=PC2, color=Type)) + geom_point()
