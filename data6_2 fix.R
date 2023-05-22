# data6_2 fix


data6_2 <- data6[c(15,3,8,5,10)]


data6_2$inAppProductPrice[data6_2$inAppProductPrice =="None"] <- "0"
data6_2$inAppProductPrice[data6_2$inAppProductPrice =="$"] <- "0"

data6_2$score <- as.numeric(data6_2$score)
data6_2$free <- as.numeric(data6_2$free)
data6_2$price <- as.numeric(data6_2$price)
data6_2$inAppProductPrice <- as.numeric(data6_2$inAppProductPrice)
data6_2$adSupported <- as.numeric(data6_2$adSupported)