## Explore Data

df <- read.csv("winequality-red.csv")

boxplot(df$quality)

summary(df$quality)

df$quality_group <- cut(df$quality,
                          breaks=c(2, 4, 6, 8),
                          labels=c('Low', 'Medium', 'High'))

barplot(table(df$quality_group))

df$quality_group <- as.factor(df$quality_group)

set.seed(123)

dt = sort(sample(nrow(df), nrow(df) * 0.8))
train <- df[dt,]
test <- df[-dt,]

## KNN

train_scale <- scale(train[, 1:12])
test_scale <- scale(test[, 1:12])

library(class)

# Fitting KNN Model to training dataset
# K=1
nearest1 <- knn(train = train_scale,
                      test = test_scale,
                      cl = train$quality_group,
                      k = 1)
misClassError <- mean(nearest1 != test$quality_group)
print(paste('Accuracy =', 1-misClassError))

# K=3
nearest3 <- knn(train = train_scale,
                      test = test_scale,
                      cl = train$quality_group,
                      k = 3)
misClassError <- mean(nearest3 != test$quality_group)
print(paste('Accuracy =', 1-misClassError))

# K=5
nearest5 <- knn(train = train_scale,
                      test = test_scale,
                      cl = train$quality_group,
                      k = 5)
misClassError <- mean(nearest5 != test$quality_group)
print(paste('Accuracy =', 1-misClassError))

# We have the highest accuracy when K=3.

# Confusiin Matrix
cm <- table(test$quality_group, nearest3)
table(test$quality_group)
cm


## Multinomial Logistic Regression

train$quality_group <- relevel(train$quality_group, ref = "Low")

library(nnet)

multinom_model <- multinom(quality_group ~ ., data = train)
summary(multinom_model)

exp(coef(multinom_model))
head(round(fitted(multinom_model), 2))

# Fit on the train data
train$qualityPredicted <- predict(multinom_model, newdata = train, "class")
tab <- table(train$quality_group, train$qualityPredicted)
round((sum(diag(tab))/sum(tab))*100,2)

# Test on the test data
test$qualityPredicted <- predict(multinom_model, newdata = test, "class")
tab2 <- table(test$quality_group, test$qualityPredicted)
tab2
round((sum(diag(tab2))/sum(tab2))*100,2)


## K-mean Clustering

train_new <- train[,!names(train) %in% c("quality", "qualityPredicted","quality_group")]
train_quality <- train[,"quality_group"]
train_scale <- scale(train_new) 

k3 <- kmeans(train_scale, centers = 3, nstart = 50)
k3$size

table(k3$cluster,train_quality)









