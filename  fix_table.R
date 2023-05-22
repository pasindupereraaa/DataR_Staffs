# data6_3 fix


data6_3 <- data6[c(15,3,8,10)]




data6_3$score <- as.numeric(data6_3$score)
data6_3$free <- as.numeric(data6_3$free)
data6_3$price <- as.numeric(data6_3$price)

data6_3$adSupported <- as.numeric(data6_3$adSupported)

##############################################################

data6_4 <- data6[c(6,3,8,10,18)]

data6_4$minInstalls <- as.numeric(data6_4$minInstalls)
data6_4$free <- as.numeric(data6_4$free)
data6_4$price <- as.numeric(data6_4$price)
data6_4$adSupported <- as.numeric(data6_4$adSupported)
data6_4$maxprice <- as.numeric(data6_4$maxprice)



########################################

str(d10)

d10$downloads <- as.factor(d10$downloads)
d10$ratings <- as.numeric(d10$ratings)
d10$free <- as.numeric(d10$free)
d10$max_ia_price <- as.integer(d10$max_ia_price)
d10$min_ia_price <- as.integer(d10$min_ia_price, nsmall = 2)
d10$price <- as.numeric(d10$price)
d10$adSupported <- as.numeric(d10$adSupported)
d10$reviews <- as.numeric(d10$reviews)
d10_3$r_score <- as.factor(d10_3$r_score)
d10$minInstalls <- as.numeric(d10$minInstalls)
d10$score <- as.numeric(d10$score)


d10_3$r_score <- as.numeric(d10_3$r_score)




#########################################



#train set creation
t_id <- sample(nrow(d10_3), nrow(d10_3)*0.0010)
train_d <- d10_3[t_id, ]
testval_d <- d10_3[-t_id, ]

#test and validation set creation
t2_id <- sample(nrow(testval_d), nrow(testval_d)*0.0010)
test_d <- testval_d[t2_id, ]
val_d <- testval_d[-t2_id, ]

################################################

#train set creation
t_id <- sample(nrow(d10_3), nrow(d10_3)*0.70)
train_d <- d10_3[t_id, ]
testval_d <- d10_3[-t_id, ]

#test and validation set creation
t2_id <- sample(nrow(testval_d), nrow(testval_d)*0.50)
test_d <- testval_d[t2_id, ]
val_d <- testval_d[-t2_id, ]

