library(class)
library(caret)


set.seed(123)
trainIndex <- createDataPartition(d10_3, p = 0.1, list = FALSE)
train_d <- iris[trainIndex, ]
test_d <- iris[-trainIndex, ]

k <- 5 
knnModel <- knn(train = train_d, test = test_d, k )



confusion_matrix <- table(testData[, 5], knnModel)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
print(accuracy)


newData <- data.frame(Sepal.Length = 5.1, Sepal.Width = 3.5, Petal.Length = 1.4, Petal.Width = 0.2)
prediction <- knn(train_d[, -5], newData, trainData[, 5], k)
print(prediction)





###################################################

install.packages("kknn")
library(kknn)

trainIndex <- createDataPartition(d10_3$downloads, p = 0.001, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

k <- 5 
knnModel <- kknn(formula = Species ~ ., train = trainData, test = testData, k = k)


confMatrix <- table(testData[, 5], knnModel)
print(confMatrix)
accuracy <- sum(diag(confMatrix))/sum(confMatrix)
print(accuracy)

newData <- data.frame(Sepal.Length = 5.1, Sepal.Width = 3.5, Petal.Length = 1.4, Petal.Width = 0.2)
prediction <- knn(trainData[, -5], newData, trainData[, 5], k)
prediction




####################################################

library(caret)

set.seed(123)
dat.d <- sample(1:nrow(d10_3),size=nrow(d10_3)*0.7,replace = FALSE) 
train_d <- d10_3[dat.d,] 
test_d <- d10_3[-dat.d,] 


train_d_l <- d10_3[dat.d,1]
test_d_l <-d10_3[-dat.d,1]





k <- 5  
knnModel <- train(x = trainData[, -5], y = trainData[, 5], method = "knn", trControl = trainControl(method = "none"), tuneGrid = data.frame(k = k))





###############################################################



library(caret)

library(data.table)

library(FNN)


set.seed(123)
trainIndex <- createDataPartition(d9_4$downloads, p = 0.7, list = FALSE)
trainData <- d9_4[trainIndex, ]
testData <- d9_4[-trainIndex, ]

k <- 5  
#knnModel <- knn(trainData[, !"downloads"], testData[, !"downloads"], trainData$downloads, k = k)


knnModel <- knn(trainData[, -which(names(trainData) == "downloads")], 
                testData[, -which(names(testData) == "downloads")], 
                trainData$downloads, k = k)


confMatrix <- table(testData$downloads, knnModel)
accuracy <- sum(diag(confMatrix)) / sum(confMatrix)

precision <- diag(confMatrix) / rowSums(confMatrix)
recall <- diag(confMatrix) / colSums(confMatrix)
f1 <- 2 * precision * recall / (precision + recall)

print(accuracy)
print(precision)
print(recall)
print(f1)



