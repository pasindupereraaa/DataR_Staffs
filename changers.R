

d10_2 <- d10[c(2,5,6,7,8,9,10,11)]

library(dplyr)
d10_3 <- d10 %>% filter(score != "5")

##z <- c(589,     1,     1,     1,    3,     7,     4,     7,    17,     9,    10,    18,    22,     3,    42,
##       8,    15,    68,    14,    10,   140,  9,    20,     8,    17,    38,    12,    31,    10,    61,    28,
##       19,    20,   134,    12,    23,    47,    12,    51,     8,   151,    21, )


d10_3$downloads <- ifelse(d10_3$minInstalls %in% c("100", "500", "50", "10", "5", "1"), "v_low", 
                           ifelse(d10_3$minInstalls %in% c("5000", "1000"), "low", 
                             ifelse(d10_3$minInstalls %in% c("10000"), "medium",  
                               ifelse(d10_3$minInstalls %in% c("50000","1e+05" ), "high", 
                                ifelse(d10_3$minInstalls %in% c("5e+05", "1e+06", "5e+06", "1e+07", "5e+07","5e+08", 
                                                                "1e+08","1e+09", "5e+09","1e+10"), "v_high", 
                                  ifelse(d10_3$minInstalls %in% c("0"), "non", "no" ))))))


d10_3$downloads <- ifelse(d10_3$minInstalls %in% c("100", "500", "50", "10", "5", "1", "5000", "1000"), "v_low", 
                                     ifelse(d10_3$minInstalls %in% c("10000"), "medium",  
                                        ifelse(d10_3$minInstalls %in% c("50000","1e+05" ), "high", 
                                               ifelse(d10_3$minInstalls %in% c("5e+05", "1e+06", "5e+06", "1e+07", "5e+07","5e+08", 
                                                                               "1e+08","1e+09", "5e+09","1e+10"), "v_high", 
                                                      ifelse(d10_3$minInstalls %in% c("0"), "non", "no" )))))

d10_3$downloads <- as.factor(d10_3$downloads)



d10_3$r_score <- ifelse(d10_3$score >= 0 & d10_3$score <= 3.7, "low", 
                     ifelse(d10_3$score > 3.7 & d10_3$score <= 4.2, "medium",
                            ifelse(d10_3$score > 4.2 & d10_3$score <= 4.6, "high", "v_high")))


d10_3$r_score <- ifelse(d10_3$score >= 0 & d10_3$score <= 3.6, "1_low", 
                        ifelse(d10_3$score > 3.6 & d10_3$score <= 4.15, "2_medium",
                               ifelse(d10_3$score > 4.15 & d10_3$score <= 4.5, "3_high", "4_Vhigh")))

d10_3$r_score <- ifelse(d10_3$score >= 0 & d10_3$score <= 3.6, "1", 
                        ifelse(d10_3$score > 3.6 & d10_3$score <= 4.15, "2",
                               ifelse(d10_3$score > 4.15 & d10_3$score <= 4.5, "3", "4")))


d10_4 <- d10_3[c(2,3,4,5,6,7,8,9,10,11)]
d11<- d10_3[c(1,4,5,6,7,8,9,10,11)]

columns_to_normalize <- c("ratings", "reviews", "price", "min_ia_price", "max_ia_price", "score", "adSupported", "free")
d11[, columns_to_normalize] <- scale(d11[, columns_to_normalize])

d11_1 <- d11[c(1,4,5,6,7,8,9,10,11)]
d11_1$score <- as.numeric(d11_1$score)
d11_1[, columns_to_normalize] <- scale(d11_1[, columns_to_normalize])




d10_4$downloads <- ifelse(d10_4$minInstalls %in% c("100", "500", "50", "10", "5", "1", "1000"), "low", 
                          ifelse(d10_4$minInstalls %in% c("10000", "5000"), "medium",  
                                 ifelse(d10_4$minInstalls %in% c("1e+05","50000" ), "high", 
                                        ifelse(d10_4$minInstalls %in% c( "5e+05", "1e+06", "5e+06", "1e+07", "5e+07","5e+08", 
                                                                        "1e+08","1e+09", "5e+09","1e+10"), "v_high", 
                                               ifelse(d10_4$minInstalls %in% c("0"), "non", "no" )))))

d10_4$downloads <- as.factor(d10_4$downloads)
plot(d10_4$downloads)

d11_1$downloads <- ifelse(d11_1$minInstalls %in% c("100", "500", "50", "10", "5", "1", "1000"), "low", 
                          ifelse(d11_1$minInstalls %in% c("10000", "5000"), "medium",  
                                 ifelse(d11_1$minInstalls %in% c("1e+05","50000" ), "high", 
                                        ifelse(d11_1$minInstalls %in% c( "5e+05", "1e+06", "5e+06", "1e+07", "5e+07","5e+08", 
                                                                         "1e+08","1e+09", "5e+09","1e+10"), "v_high", 
                                               ifelse(d11_1$minInstalls %in% c("0"), "non", "no" )))))

