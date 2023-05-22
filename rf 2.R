library(randomForest)


#train set creation
t_id <- sample(nrow(d10_4), nrow(d10_4)*0.10)
train_d <- d10_4[t_id, ]
testval_d <- d10_4[-t_id, ]

#test and validation set creation
t2_id <- sample(nrow(testval_d), nrow(testval_d)*0.10)
test_d <- testval_d[t2_id, ]
val_d <- testval_d[-t2_id, ]


rf_model <- randomForest(r_score ~  adSupported + reviews free + price + min_ia_price + max_ia_price, data = train_d, 
                         ntree = 600,  mtry = 3.4)

#rf_model <- randomForest(r_score ~ adSupported + free + price + min_ia_price + max_ia_price, data = train_d, 
#                        )



pred <- predict(rf_model, newdata = test_d)


plot(rf_model)

#plot(pred)

accuracy <- mean(pred == test_d$r_score)


confusion_matrix <- table(pred, test_d$r_score)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
f1 <- 2 * precision * recall / (precision + recall)

print(accuracy)

print(precision)
print(recall)
print(f1)


