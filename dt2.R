library(rpart)

set.seed(123)
#train set creation
t_id <- sample(nrow(d10_2), nrow(d10_2)*0.70)
train_d <- d10_2[t_id, ]
testval_d <- d10_2[-t_id, ]

#test and validation set creation
t2_id <- sample(nrow(testval_d), nrow(testval_d)*0.50)
test_d <- testval_d[t2_id, ]
val_d <- testval_d[-t2_id, ]

tree <- rpart(r_score ~ ratings + adSupported + reviews + free + price + min_ia_price + max_ia_price, data = train_d, method = "anova")

plot(tree)

predictions <- predict(tree, newdata = test_d)
plot(predictions)

accuracy <- mean(predictions == test_d$r_score)
plot(accuracy)
accuracy






########################################################


library(partykit)

set.seed(123)

#train set creation
t_id <- sample(nrow(d10_2), nrow(d10_2)*0.70)
train_d <- d10_2[t_id, ]
testval_d <- d10_2[-t_id, ]

#test and validation set creation
t2_id <- sample(nrow(testval_d), nrow(testval_d)*0.50)
test_d <- testval_d[t2_id, ]
val_d <- testval_d[-t2_id, ]



tree <- ctree(r_score ~ ratings + adSupported + reviews + free + price + min_ia_price + max_ia_price, data = train_d)

plot(tree)

predictions <- predict(tree, newdata = test_d)

plot(predictions)

accuracy <- mean(predictions == test_d$r_score)
##confusion_matrix(predictions, test_d$downloads)

plot(accuracy)
  