library(partykit)

set.seed(123)

t_id <- sample(nrow(data6_3), nrow(data6_3)*0.75)
train_d <- data6_3[t_id, ]
test_d <- data6_3[-t_id, ]

tree <- ctree(score  ~ free + price + adSupported, data = train_d)

plot(tree)

predictions <- predict(tree, newdata = test_d)

plot(predictions)

accuracy <- mean(predictions == test_d$score)
plot(accuracy)


##########################################



library(partykit)

set.seed(123)

t_id1 <- sample(nrow(data6_4), nrow(data6_4)*0.75)
train_d1 <- data6_4[t_id1, ]
test_d1 <- data6_4[-t_id1, ]

tree1 <- ctree(minInstalls  ~ free + price + adSupported + maxprice, data = train_d1)

plot(tree1)

predictions1 <- predict(tree1, newdata = test_d1)

plot(predictions1)

accuracy1 <- mean(predictions1 == test_d$minInstalls)
plot(accuracy1)

