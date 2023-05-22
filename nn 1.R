
library(caret)
set.seed(123)
trainIndex <- createDataPartition(d10_1$downloads, p = 0.10, list = FALSE)
train <- d10_1[trainIndex, ]
test <- d10_1[-trainIndex, ]


library(keras)
model <- keras_model_sequential()
insmodel %>% 
  layer_dense(units = 32, activation = "relu", input_shape = ncol(train_d[, 1:10] )) %>% 
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")


model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adam(),
  metrics = c("accuracy")
)


history <- model %>% fit(
  x = train_d[, -which(names(train_d) == "score")],
  y = train$score,
  epochs = 100,
  batch_size = 32,
  validation_data = list(test[, -which(names(test) == "score")], test$score)
)



model %>% evaluate(
  x = test[, -which(names(test) == "score")],
  y = test$score
)


#########################################################################

install.packages("mxnet")


library(mxnet)
library(dplyr)
library(caret)



data_norm <- d10_1 %>%
  mutate_all(scale)


set.seed(123)
train_idx <- createDataPartition(data$downloads, p = 0.7, list = FALSE)
train_data <- data_norm[train_idx, ]
test_data <- data_norm[-train_idx, ]

#mxnet
train_x <- as.matrix(train_data[, -1])
train_y <- as.numeric(train_data$target) - 1
test_x <- as.matrix(test_data[, -1])
test_y <- as.numeric(test_data$target) - 1


#architecture
net <- mx.model.FeedForward.create(
  symbol = mx.symbol.Variable("d10_1"),
  ctx = mx.cpu(),
  dim = c(5, 10, 2),
  layout = "colmajor",
  # Define the layers
  top = list(
    mx.symbol.FullyConnected(name = "fc1", num_hidden = 10),
    mx.symbol.Activation(name = "relu1", act_type = "relu"),
    mx.symbol.FullyConnected(name = "fc2", num_hidden = 2),
    mx.symbol.SoftmaxOutput(name = "softmax")
  )
)


# Train
model <- mx.model.FeedForward.train(
  net,
  X = train_x,
  y = train_y,
  ctx = mx.cpu(),
  num.round = 10,
  array.batch.size = 50,
  learning.rate = 0.1,
  eval.metric = mx.metric.accuracy
)


# Obtain predictions for the testing data
pred <- mx.model.FeedForward.predict(model, test_x)

# Calculate the accuracy
acc <- mx.metric.accuracy(pred, test_y)
cat(sprintf("Accuracy: %.2f%%\n", acc$metric*100))




########################################################################################


install.packages("neuralnet")
library(neuralnet)

set.seed(123)  
train_indices <- sample(1:nrow(d9_4), 0.001 * nrow(d9_4)) 

train_data <- d9_4[train_indices, ]
t_data <- d9_4[-train_indices, ]

train_indices <- sample(1:nrow(t_data), 0.001 * nrow(t_data)) 

test_data <- t_data[train_indices, ]
val_data <- t_data[-train_indices, ]



formula <- as.formula(downloads ~ free  + minPriceIn + price + ratings + adSupported + containsAds + reviews + score + maxprice + age )
neural_network <- neuralnet(formula, data = train_d1, hidden = c(8, 3)) 
neural_network <- neuralnet(formula, data = train_data, hidden = c(5, 3))

neural_network1 <- neuralnet(formula, data = train_d1, hidden = c(8, 5)) 


predictions <- compute(neural_network, test_d[, c("adSupported", "free", "price", "min_ia_price", "max_ia_price")])

predictions <- compute(neural_network, test_data[, c("free", "minPriceIn", "price", "ratings", "adSupported", "containsAds", "reviews", "score", "maxprice", "age")])
#predicted_outcomes <- predictions$net.result

#mse <- mean((predicted_outcomes, test_data)^2)

binary_predictions <- round(predictions)

accuracy <- sum(binary_predictions == test_data) / length(test_data)

#print(paste("Accuracy:", accuracy))


# Assuming you have the testing dataset stored in 'test_data'

# Extract the independent variables from the testing dataset
input_data <- test_d1[, c("free", "minPriceIn", "price", "ratings", "adSupported", "containsAds", "reviews", "score", "maxprice", "age")]

# Make predictions using the trained neural network
predictions <- compute(neural_network, input_data)$net.result

# Convert the predictions to binary values if necessary
binary_predictions <- round(predictions)

# Extract the dependent variable from the testing dataset
actual_values <- test_data1$downloads

# Calculate accuracy
accuracy <- sum(binary_predictions == actual_values) / length(actual_values)

# Print accuracy
print(paste("Accuracy:", accuracy))





predictions <- predict(neural_network, newdata = test_data)

accuracy <- sum(predictions == test_data) / length(test_data)

accuracy <- mean(predictions == test_data$downloads)

print(paste("Accuracy:", accuracy))


confusion_matrix <- table(predictions, test_d$downloads)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
f1 <- 2 * precision * recall / (precision + recall)

print(accuracy)
print(precision)
print(recall)
print(f1)








##################################

library(keras)

# Prepare your dataset and split into training and testing sets

# nn
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 64, activation = "relu", input_shape = ncol(train_d)) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1)  # Assuming you're predicting a single variable

#
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(),
  metrics = c('mean_squared_error')
)

# 
history <- model %>% fit(
  train_d, train_labels,
  epochs = 100,
  batch_size = 32,
  validation_split = 0.2
)

#
loss_metrics <- model %>% evaluate(test_data, test_labels)
print(paste("Mean Squared Error:", loss_metrics$mean_squared_error))

#
predictions <- model %>% predict(new_data)










#################################################


install.packages("nnet")
library(nnet)

#model <- nnet(downloads ~ score + ratings + adSupported + reviews + free + price + min_ia_price + max_ia_price, data = train_d, size = 20, maxit = 100000)
model <- nnet(downloads ~ free  + minPriceIn + price + ratings + adSupported + containsAds + reviews + score + maxprice + age, data = train_data, size = 50)


plot(model)

predictions <- predict(model, newdata = test_data)

accuracy <- sum(predictions == test_data) / length(test_data)

accuracy <- mean(predictions == test_data$downloads)

print(paste("Accuracy:", accuracy))


confusion_matrix <- table(predictions, test_d$downloads)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
f1 <- 2 * precision * recall / (precision + recall)

print(accuracy)
print(precision)
print(recall)
print(f1)
###############################################################



