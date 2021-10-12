options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

load("xscaled.rda")

# set.seed(1) if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

#K-means clustering
set.seed(3, sample.kind = "Rounding")
k <- kmeans(train_x, centers = 2)

predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

train_y[which(predict_kmeans(train_x, k) == 2)]

kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M")

mean(kmeans_preds == test_y)

confusionMatrix(factor(kmeans_preds), factor(test_y))

#Logistic regression model
set.seed(1, sample.kind = "Rounding")
dat <- data.frame(train_x, y = factor(train_y))
train_glm <- train(y ~ ., method = "glm", data = dat)
mean(predict(train_glm, test_x) == test_y)

#LDA and QDA
set.seed(1, sample.kind = "Rounding")
train_lda <- train(y ~ ., method = "lda", data = dat)
mean(predict(train_lda, test_x) == test_y)

train_qda <- train(y ~ ., method = "qda", data = dat)
mean(predict(train_qda, test_x) == test_y)

#Loess
library(gam)
set.seed(5, sample.kind = "Rounding")
train_loess <- train(y ~ ., method = "gamLoess", data = dat)
mean(predict(train_loess, test_x) == test_y)

#K-nearest neighbors model
set.seed(7, sample.kind = "Rounding")
train_knn <- train(train_x, train_y, method = "knn", tuneGrid = data.frame(k = seq(3,21)))
train_knn$bestTune
mean(predict(train_knn, test_x) == test_y)

set.seed(7, sample.kind = "Rounding") # simulate R 3.5
tuning <- data.frame(k = seq(3, 21, 2))
train_knn <- train(train_x, train_y,
                   method = "knn", 
                   tuneGrid = tuning)
train_knn$bestTune

#Random forest model
set.seed(9, sample.kind = "Rounding")
train_rf <- train(train_x, train_y, method = "rf", tuneGrid = data.frame(mtry = c(3,5,7,9)), importance = TRUE)
train_rf$bestTune
mean(predict(train_rf, test_x) == test_y)
varImp(train_rf)

#Ensemble
ens <- data.frame(rf = predict(train_rf, test_x), 
         knn = predict(train_knn, test_x),
         kmeans = kmeans_preds,
         lda = predict(train_lda, test_x),
         qda = predict(train_qda, test_x),
         loess = predict(train_loess, test_x),
         glm = predict(train_glm, test_x)
         )

pred <- ens %>% mutate(rf = ifelse(rf == "M", 1, 0),
               knn = ifelse(knn == "M", 1, 0),
               kmeans = ifelse(kmeans == "M", 1, 0),
               lda = ifelse(lda == "M", 1, 0),
               qda = ifelse(qda == "M", 1, 0),
               loess = ifelse(loess == "M", 1, 0),
               glm = ifelse(glm == "M", 1, 0)) %>%
  mutate(pred = ifelse(round((rf+knn+kmeans+lda+qda+loess+glm)/7) == 1, "M", "B")) %>%
  .$pred

mean(pred == test_y)

#Accuracy
models <- c("K means", "Logistic regression", "LDA", "QDA", "Loess", "K nearest neighbors", "Random forest", "Ensemble")
accuracy <- c(mean(kmeans_preds == test_y),
              mean(predict(train_glm, test_x) == test_y),
              mean(predict(train_lda, test_x) == test_y),
              mean(predict(train_qda, test_x) == test_y),
              mean(predict(train_loess, test_x) == test_y),
              mean(predict(train_knn, test_x) == test_y),
              mean(predict(train_rf, test_x) == test_y),
              mean(pred == test_y))
data.frame(Model = models, Accuracy = accuracy)

