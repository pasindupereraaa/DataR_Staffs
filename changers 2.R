d1 <- data1[c(1,6,8,9,10,12,13,14,16,17,18,21,31,32,34,35 )]

d2 <- d1


#remove null
d2$minprice <- gsub("None", "0", d2$minprice)
d2$minprice <- gsub(" per item", "0", d2$minprice)
d2$minprice <- gsub(" $", "", d2$minprice)

d3 <- d2[data2$genreId %in% c("ART_AND_DESIGN","ENTERTAINMENT","PERSONALIZATION","TRAVEL_AND_LOCAL","PHOTOGRAPHY","TOOLS","EVENTS","AUTO_AND_VEHICLES","SPORTS","EDUCATION","BEAUTY","BOOKS_AND_REFERENCE","MUSIC_AND_AUDIO","SOCIAL","BUSINESS","LIFESTYLE","COMMUNICATION","PRODUCTIVITY","FINANCE","FOOD_AND_DRINK","SHOPPING","MAPS_AND_NAVIGATION","MEDICAL","COMICS","VIDEO_PLAYERS","HOUSE_AND_HOME","DATING","LIBRARIES_AND_DEMO","NEWS_AND_MAGAZINES","HEALTH_AND_FITNESS","PARENTING","WEATHER"),]

d4 <- d3[!rem_null1, ]

d4$age <- ifelse(d4$releasedYear %in% c("2022"), "0", 
             ifelse(d4$releasedYear %in% c("2021"), "1",  
               ifelse(d4$releasedYear %in% c("2020"), "2", 
                 ifelse(d4$releasedYear %in% c("2019"), "3",
                   ifelse(d4$releasedYear %in% c("2018"), "4",  
                     ifelse(d4$releasedYear %in% c("2017"), "5",  
                      ifelse(d4$releasedYear %in% c("2016"), "6",     
                        ifelse(d4$releasedYear %in% c("2015"), "7",     
                          ifelse(d4$releasedYear %in% c("2014"), "8",     
                           ifelse(d4$releasedYear %in% c("2013"), "9",  
                              ifelse(d4$releasedYear %in% c("2012"), "10",  
                                ifelse(d4$releasedYear %in% c("2011"), "11",     
                                 ifelse(d4$releasedYear %in% c("2010"), "12",     
                                  ifelse(d4$releasedYear %in% c("2009"), "13", "ff" ))))))))))))))         

d4$inAppProductPrice <- gsub(" per item", "", d4$inAppProductPrice)                            
d4$inAppProductPrice <- gsub("None", "0", d4$inAppProductPrice)         
#d4$inprice <- substr(d4$inAppProductPrice, 2, 1)
#price_parts <- strsplit(d4$inAppProductPrice, "$")
#d4$minprice <- sapply(price_parts, `[`, 2)

subset_list <- split(d4, d4$releasedMonth)
for (i in seq_along(subset_list)) {
  assign(paste0("d4_", i), subset_list[[i]])
}


library(openxlsx)
write.xlsx(d4_4, "d4_4.xlsx")
write.xlsx(d4_5, "d4_5.xlsx")
write.xlsx(d4_6, "d4_6.xlsx")
write.xlsx(d4_7, "d4_7.xlsx")
write.xlsx(d4_8, "d4_8.xlsx")
write.xlsx(d4_9, "d4_9.xlsx")
write.xlsx(d4_10, "d4_10.xlsx")
write.xlsx(d4_11, "d4_11.xlsx")
write.xlsx(d4_12, "d4_12.xlsx")

d5_1 <- read.xlsx("d4_1.xlsx")
d5_2 <- read.xlsx("d4_2.xlsx")
d5_3 <- read.xlsx("d4_3.xlsx")
d5_4 <- read.xlsx("d4_4.xlsx")
d5_5 <- read.xlsx("d4_5.xlsx")
d5_6 <- read.xlsx("d4_6.xlsx")
d5_7 <- read.xlsx("d4_7.xlsx")
d5_8 <- read.xlsx("d4_8.xlsx")
d5_9 <- read.xlsx("d4_9.xlsx")
d5_10 <-  read.xlsx("d4_10.xlsx")
d5_11 <- read.xlsx("d4_11.xlsx")
d5_12 <- read.xlsx("d4_12.xlsx")  


install.packages("data.table")
library(data.table)
d6_1 <- data.frame(d5_1)
d6_2 <- data.frame(d5_2)
d6_3 <- data.frame(d5_3)
d6_4 <- data.frame(d5_4)
d6_5 <- data.frame(d5_5)
d6_6 <- data.frame(d5_6)
d6_7 <- data.frame(d5_7)
d6_8 <- data.frame(d5_8)
d6_9 <- data.frame(d5_9)
d6_10 <- data.frame(d5_10)
d6_11 <- data.frame(d5_11)
d6_12 <- data.frame(d5_12)


d6 <- merge(d6_1, d6_2, d6_3, d6_4, d6_5, d6_6, d6_7, d6_8, d6_9, d6_10, d6_11, d6_12, all = TRUE, sort = FALSE, no.dups = FALSE)

d6 <- merge(d6_1, d6_2, d6_3, d6_4, d6_5, d6_6, d6_7, d6_8, d6_9, d6_10, d6_11, d6_12, by = "ID", all = TRUE)
d6 <- merge(d6_1, d6_2, all = TRUE)
d6 <- merge(d6, d6_3, all = TRUE)
d6 <- merge(d6, d6_4, all = TRUE)
d6 <- merge(d6, d6_5, all = TRUE)
d6 <- merge(d6, d6_6, all = TRUE)
d6 <- merge(d6, d6_7, all = TRUE)
d6 <- merge(d6, d6_8, all = TRUE)
d6 <- merge(d6, d6_9, all = TRUE)
d6 <- merge(d6, d6_10, all = TRUE)
d6 <- merge(d6, d6_11, all = TRUE)
d6 <- merge(d6, d6_12, all = TRUE)

d7 <- d6

library(tidyr)
d8 <- separate(d7, inAppProductPrice, into = c("minPriceIn", "maxPriceIn"), sep = "-")

d9 <- d8

d9 <- d9[, -5]
d9 <- d9[, -6]
d9 <- d9[, -14]
d9 <- d9[, -16]


d9$minInstalls <- as.numeric(d9$minInstalls)

d9$downloads <- ifelse(d9$minInstalls %in% c("100", "500", "50", "10", "5", "1", "5000", "1000"), "v_low", 
                          ifelse(d9$minInstalls %in% c("10000"), "medium",  
                                 ifelse(d9$minInstalls %in% c("50000","1e+05" ), "high", 
                                        ifelse(d9$minInstalls %in% c("5e+05", "1e+06", "5e+06", "1e+07", "5e+07","5e+08", 
                                                                        "1e+08","1e+09", "5e+09","1e+10"), "v_high", 
                                               ifelse(d9$minInstalls %in% c("0"), "non", "no" )))))


d9$genreId <- as.factor(d9$genreId)
d9$minPriceIn  <- as.numeric(d9$minPriceIn )
d9$releasedMonth <- as.factor(d9$releasedMonth)

d9$age <- as.numeric(d9$age)
d9$downloads <- as.factor(d9$downloads)



d9_1 <- d9[c(16,2,3,4,5,6,7,8,9,10,11,14,15)]

d9_1[is.na(d9_1)] <- 0

d9_2 <- d9_1[d9_1$ratings != "0", ]

d9_3 <- d9_2[c(1,2,3,4,6,7,8,9,10,11,12,13)]

columns_to_normalize <- c("free", "price", "minPriceIn", "ratings", "adSupported", "containsAds", "score", "reviews", "maxprice", "age")
d9_3[, columns_to_normalize] <- scale(d9_3[, columns_to_normalize])

d9_4 <- d9_3[c(1,2,4,5,6,7,8,9,10,11,12)]


#train set creation
t_id <- sample(nrow(d9_3), nrow(d9_4)*0.70)
train_d <- d9_3[t_id, ]
testval_d <- d9_3[-t_id, ]

#test and validation set creation
t2_id <- sample(nrow(testval_d), nrow(testval_d)*0.50)
test_d <- testval_d[t2_id, ]
val_d <- testval_d[-t2_id, ]



#train set creation
t_id1 <- sample(nrow(d9_4), nrow(d9_4)*0.70)
train_d1 <- d9_4[t_id1, ]
testval_d1 <- d9_4[-t_id1, ]

#test and validation set creation
t2_id1 <- sample(nrow(testval_d1), nrow(testval_d1)*0.50)
test_d1 <- testval_d1[t2_id1, ]
val_d1 <- testval_d1[-t2_id1, ]
