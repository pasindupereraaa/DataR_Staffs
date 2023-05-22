library(rpart)

set.seed(123)
t_id <- sample(nrow(data6_3), nrow(data6_3)*0.75)
train_d <- data6_3[t_id, ]
test_d <- data6_3[-t_id, ]

tree <- rpart(score ~ free + price + adSupported, data = train_d, method = "class" , control = rpart.control(minsplit = 2))

plot(tree)

predictions <- predict(tree, newdata = test_d)
accuracy <- mean(predictions == test_d$ratings)


plot(predictions)
