

library(caret)
set.seed(123)
#trainIndex <- createDataPartition(d10_1$downloads, p = 0.10, list = FALSE)
#train <- d10_1[trainIndex, ]
#test <- d10_1[-trainIndex, ]


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
  x = train_d[, -which(names(train_d) == "downloads")],
  y = train_d$downloads,
  epochs = 100,
  batch_size = 32,
  validation_data = list(test_d[, -which(names(test_d) == "downloads")], test_d$downloads)
)



model %>% evaluate(
  x = test_d[, -which(names(test_d) == "downloads")],
  y = test_d$downloads
)











########################################################################################

install.packages("tensorflow")


library(tensorflow)
library(dplyr)
library(caret)


data_norm <- d10_3 %>%
  mutate_all(scale)


set.seed(123)
train_idx <- createDataPartition(d10_3$downloads, p = 0.75, list = FALSE)
train_data <- data_norm[train_idx, ]
test_data <- data_norm[-train_idx, ]


train_x <- as.matrix(train_data[, -1])
train_y <- as.numeric(train_data$downloads) - 1
test_x <- as.matrix(test_data[, -1])
test_y <- as.numeric(test_data$downloads) - 1




model <- tf$model(
  tf$keras$layers$Dense(units = 10, activation = "relu", input_shape = ncol(train_x)),
  tf$keras$layers$Dense(units = 2, activation = "softmax")
)





  top = list(
    mx.symbol.FullyConnected(name = "fc1", num_hidden = 10),
    mx.symbol.Activation(name = "relu1", act_type = "relu"),
    mx.symbol.FullyConnected(name = "fc2", num_hidden = 2),
    mx.symbol.SoftmaxOutput(name = "softmax")
  )




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
