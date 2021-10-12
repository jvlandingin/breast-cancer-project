options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

#DIMENSIONS AND PROPERTIES
#number of samples in the dataset
nrow(brca$x)
#number of predictors
ncol(brca$x)
#proportion of the samples that are malignant
mean(brca$y == "M")
#column number with the highest mean
which.max(colMeans(brca$x))
#column number with the lowest standard deviation
which.min(colSds(brca$x))

#SCALING THE MATRIX
brca$x <- sweep(brca$x, 2, colMeans(brca$x))
brca$x <- sweep(brca$x, 2, FUN = "/", colSds(brca$x))
median(brca$x[,1])

#DISTANCE
d <- dist(brca$x)
d
image(as.matrix(d))
?dist
dim(as.matrix(d))

#benign 
malignant_index <- which(brca$y == "M")
mean(as.matrix(d)[c(-malignant_index),1])
mean(as.matrix(d)[c(malignant_index),1])

#Heatmap
d_featured <- dist(t(brca$x))
heatmap(as.matrix(d_featured), labRow = NA, labCol = NA)

#Hierarchical Clustering
h <- hclust(d_featured)
h <- cutree(h, k = 5)
plot(h)
split(names(h), h)

#PCA
pca <- prcomp(brca$x)
summary(pca)
plot(summary(pca)$importance[3,])

#Plot of first two principal components
data.frame(x = pca$x, y = brca$y) %>%
  select(x.PC1, x.PC2, y) %>%
  ggplot(aes(x.PC1, x.PC2, color = y)) +
  geom_point() +
  geom_abline()

#Boxplot of first 10 PCs
data.frame(pca$x[,1:10], type = brca$y) %>%
  gather(-type, key = "PC", value = "PC_value") %>%
  ggplot(aes(x = PC, y = PC_value, color = type)) +
  geom_boxplot()

x_scaled <- brca$x
save(x_scaled, file = "xscaled.rda")
