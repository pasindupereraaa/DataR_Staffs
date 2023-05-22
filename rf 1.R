library(randomForest)


#train set creation
t_id <- sample(nrow(d10_3), nrow(d10_3)*0.70)
train_d <- d10_3[t_id, ]
testval_d <- d10_3[-t_id, ]

#test and validation set creation
t2_id <- sample(nrow(testval_d), nrow(testval_d)*0.50)
test_d <- testval_d[t2_id, ]
val_d <- testval_d[-t2_id, ]


rf_model <- randomForest(downloads ~ adSupported + free + price + min_ia_price + max_ia_price, data = train_d, )


rf_model <- randomForest(downloads ~ score + ratings + adSupported + reviews + free + price + min_ia_price + max_ia_price, data = train_d, ntree = 600,  mtry = 3.4)

rf_model <- randomForest(downloads ~ free + price + minPriceIn + ratings 
                         + adSupported + containsAds + score + reviews 
                         + maxprice + age, data = train_d)

pred <- predict(rf_model, newdata = test_d)


plot(rf_model)

#plot(pred)


accuracy <- mean(pred == test_d$downloads)


confusion_matrix <- table(pred, test_d$downloads)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
f1 <- 2 * precision * recall / (precision + recall)

print(accuracy)
print(precision)
print(recall)
print(f1)

##########################################################################################################



library(randomForest)


#train set creation
t_id <- sample(nrow(d11_1), nrow(d11_1)*0.70)
train_d <- d11_1[t_id, ]
testval_d <- d11_1[-t_id, ]

#test and validation set creation
t2_id <- sample(nrow(testval_d), nrow(testval_d)*0.50)
test_d <- testval_d[t2_id, ]
val_d <- testval_d[-t2_id, ]

rf_model <- randomForest(downloads ~ free + genreId + minPriceIn + price + ratings + adSupported + containsAds + reviews + score + maxprice + age ,
                         data = train_d)

rf_model <- randomForest(downloads ~ free + genreId + minPriceIn + price + ratings + adSupported + containsAds + reviews + score + maxprice + age ,
                         data = train_d, ntree = 620)

rf_model <- randomForest(downloads ~ free + genreId + minPriceIn + price + ratings + adSupported + containsAds + reviews + score + maxprice + age ,
                         data = train_d, ntree = 610,  mtry = 3.75 )

#rf_model <- randomForest(minInstalls ~  adSupported + free + price + minPriceIn + maxprice + age + releasedYear , data = train_d, )


#rf_model <- randomForest(downloads ~ score + ratings + adSupported + reviews + free + price + min_ia_price + max_ia_price, data = train_d, 
#                         ntree = 600,  mtry = 3.4)

rf_model <- randomForest(downloads ~ free + price + ratings + adSupported + reviews + score + min_ia_price + max_ia_price ,
                         data = train_d, ntree = 600,  mtry = 3.4 )


pred <- predict(rf_model, newdata = test_d)


plot(rf_model)


accuracy <- mean(pred == test_d$downloads)


confusion_matrix <- table(pred, test_d$downloads)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
f1 <- 2 * precision * recall / (precision + recall)

print(accuracy)
print(precision)
print(recall)
print(f1)


###### 1 
rf_model <- randomForest(downloads ~ free + genreId + minPriceIn + price + ratings + adSupported + containsAds + reviews + score + maxprice + age ,
                         data = train_d, ntree = 650  )
pred <- predict(rf_model, newdata = test_d)

plot(rf_model)

accuracy <- mean(pred == test_d$downloads)

confusion_matrix <- table(pred, test_d$downloads)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
f1 <- 2 * precision * recall / (precision + recall)

print(accuracy)
print(precision)
print(recall)
print(f1)

########## 2 
rf_model <- randomForest(downloads ~ free + genreId + minPriceIn + price + ratings + adSupported + containsAds + reviews + score + maxprice + age ,
                         data = train_d, nodesize = 11  )
pred <- predict(rf_model, newdata = test_d)

plot(rf_model)

accuracy <- mean(pred == test_d$downloads)

confusion_matrix <- table(pred, test_d$downloads)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
f1 <- 2 * precision * recall / (precision + recall)

print(accuracy)
print(precision)
print(recall)
print(f1)

########## 3 
rf_model <- randomForest(downloads ~ free + genreId + minPriceIn + price + ratings + adSupported + containsAds + reviews + score + maxprice + age ,
                         data = train_d, nodesize = 13  )
pred <- predict(rf_model, newdata = test_d)

plot(rf_model)

accuracy <- mean(pred == test_d$downloads)

confusion_matrix <- table(pred, test_d$downloads)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
f1 <- 2 * precision * recall / (precision + recall)

print(accuracy)
print(precision)
print(recall)
print(f1)

########## 4 
rf_model <- randomForest(downloads ~ free + genreId + minPriceIn + price + ratings + adSupported + containsAds + reviews + score + maxprice + age ,
                         data = train_d, mtry = 3.3 )
pred <- predict(rf_model, newdata = test_d)

plot(rf_model)

accuracy <- mean(pred == test_d$downloads)

confusion_matrix <- table(pred, test_d$downloads)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
f1 <- 2 * precision * recall / (precision + recall)

print(accuracy)
print(precision)
print(recall)
print(f1)

########## 5
rf_model <- randomForest(downloads ~ free + genreId + minPriceIn + price + ratings + adSupported + containsAds + reviews + score + maxprice + age ,
                         data = train_d, mtry = 3.4 )
pred <- predict(rf_model, newdata = val_d)

plot(rf_model)

accuracy <- mean(pred == val_d$downloads)

confusion_matrix <- table(pred, val_d$downloads)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
f1 <- 2 * precision * recall / (precision + recall)

print(accuracy)
print(precision)
print(recall)
print(f1)
