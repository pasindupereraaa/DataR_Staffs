library(e1071)

set.seed(123) 

#train set creation
t_id <- sample(nrow(d10_2), nrow(d10_2)*0.70)
train_d <- d10_2[t_id, ]
testval_d <- d10_2[-t_id, ]

#test and validation set creation
t2_id <- sample(nrow(testval_d), nrow(testval_d)*0.50)
test_d <- testval_d[t2_id, ]
val_d <- testval_d[-t2_id, ]



svm_model <- svm(r_score ~ ratings + adSupported + reviews + free + price + min_ia_price + max_ia_price, data = train_d, kernel = "linear")


pred <- predict(svm_model, test_d)
plot(predictions)

confusion_matrix <- table(pred, test_d$r_score)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste0("Accuracy: ", accuracy))
