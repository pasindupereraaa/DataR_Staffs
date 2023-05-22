library(e1071)



set.seed(123) 
train_index <- sample(nrow(d11), 0.5 * nrow(d10))
train_d <- d11[train_index, ]
test_d <- d11[-train_index, ]


svm_model <- svm(downloads ~ ratings + adSupported + reviews + free + price + min_ia_price + max_ia_price, data = train_d, kernel = "sigmoid")

svm_model <- svm(downloads ~ free + price + minPriceIn + ratings 
                 + adSupported + containsAds + score + reviews 
                 + maxprice + age, data = train_d)


pred <- predict(svm_model, test_d)
plot(svm_model)


  confusion_matrix <- table(pred, test_d$downloads)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
  recall <- diag(confusion_matrix) / colSums(confusion_matrix)
  f1 <- 2 * precision * recall / (precision + recall)
  
  print(accuracy)
  print(precision)
  print(recall)
  print(f1)

  
######
  
  svm_model <- svm(downloads ~ free + price + minPriceIn + ratings 
                   + adSupported + containsAds + score + reviews 
                   + maxprice + age, data = train_d, kernel = "linear")
  
  
  pred <- predict(svm_model, test_d)
  plot(svm_model)
  
  
  confusion_matrix <- table(pred, test_d$downloads)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
  recall <- diag(confusion_matrix) / colSums(confusion_matrix)
  f1 <- 2 * precision * recall / (precision + recall)
  
  print(accuracy)
  print(precision)
  print(recall)
  print(f1)
  
