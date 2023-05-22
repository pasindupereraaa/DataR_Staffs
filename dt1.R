library(rpart)

set.seed(123)
t_id <- sample(nrow(d10_1), nrow(d10_1)*0.75)
train_d <- d10_1[t_id, ]
test_d <- d10_1[-t_id, ]

tree <- rpart(downloads ~ ratings + adSupported + reviews + free + price + min_ia_price + max_ia_price, data = train_d, method = "class")

plot(tree)

predictions <- predict(tree, newdata = test_d)
print(predictions)

accuracy <- mean(predictions == test_d$downloads)
print(accuracy)




####################################




library(partykit)

set.seed(123)

#train set creation
t_id <- sample(nrow(d9_3), nrow(d9_4)*0.70)
train_d <- d9_3[t_id, ]
testval_d <- d9_3[-t_id, ]

#test and validation set creation
t2_id <- sample(nrow(testval_d), nrow(testval_d)*0.50)
test_d <- testval_d[t2_id, ]
val_d <- testval_d[-t2_id, ]


#tree <- ctree(downloads ~ ratings + adSupported + reviews + free + price + min_ia_price + max_ia_price, data = train_d)


ctrl <- ctree_control(maxdepth = 10, minsplit = 17, minbucket = 14, mincriterion = 0.6)
tree <- ctree(downloads ~ ratings + adSupported + reviews + free + price + min_ia_price + max_ia_price,
              data = train_d , control = ctrl)

tree <- ctree(downloads ~ free + price + minPriceIn + ratings 
              + adSupported + containsAds + score + reviews 
              + maxprice + age,
              data = train_d )


predictions <- predict(tree, newdata = test_d)

#plot(predictions)

accuracy <- mean(predictions == test_d$downloads)
##confusion_matrix(predictions, test_d$downloads)

confusion_matrix <- table(predictions, test_d$downloads)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
f1 <- 2 * precision * recall / (precision + recall)

print(accuracy)
print(precision)
print(recall)
print(f1)

#plot(tree)
