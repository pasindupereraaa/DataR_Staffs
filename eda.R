library(dplyr)


f1 <- filter(d9, d9$minInstalls > 1e+06)
g1 <- filter(d9, d9$minInstalls < 5000)
h1 <- filter(d9, d9$minInstalls > 5000)
h2 <- filter(h1, h1$minInstalls < 1e+06)

plot(f1$minInstalls, f1$genreId)

table(f1$minInstalls, f1$genreId)

table(f1$minInstalls, f1$free)
pie(table(f1$free, f1$minInstalls))

pairs(~minInstalls + free, data = f1,
      col.axis = "darkgreen")

pairs(~minInstalls + free, data = g1,
      col.axis = "darkgreen")

table(g1$minInstalls, g1$free)


########################## downloads and genere

table(f1$genreId)

f2 <- table(f1$genreId)
 
pie(f2)

piepercent<- round(100 * f2 / sum(f2), 1)

# Plot the chart.
pie(f2, labels = piepercent,
    main = "high download genre", col = rainbow(length(f2)))
legend("bottomright", c("ART_AND_DESIGN","ENTERTAINMENT","PERSONALIZATION","TRAVEL_AND_LOCAL","PHOTOGRAPHY","TOOLS","EVENTS","AUTO_AND_VEHICLES","SPORTS","EDUCATION","BEAUTY","BOOKS_AND_REFERENCE","MUSIC_AND_AUDIO","SOCIAL","BUSINESS","LIFESTYLE","COMMUNICATION","PRODUCTIVITY","FINANCE","FOOD_AND_DRINK","SHOPPING","MAPS_AND_NAVIGATION","MEDICAL","COMICS","VIDEO_PLAYERS","HOUSE_AND_HOME","DATING","LIBRARIES_AND_DEMO","NEWS_AND_MAGAZINES","HEALTH_AND_FITNESS","PARENTING","WEATHER"),
       cex = 0.5, fill = rainbow(length(f2)))

g2 <- table(g1$genreId)
pie(g2)


#########################################

f1 <- filter(d9, d9$free == 1)
g1 <- filter(d9, d9$free == 0)

plot(f1$free, f1$genreId)

 f2 <- table(f1$genreId)
pie(f2$genreId)

pie(f1, labels = piepercent,
    main = "high download genre", col = rainbow(length(f1)))
legend("bottomright", c("ART_AND_DESIGN","ENTERTAINMENT","PERSONALIZATION","TRAVEL_AND_LOCAL","PHOTOGRAPHY","TOOLS","EVENTS","AUTO_AND_VEHICLES","SPORTS","EDUCATION","BEAUTY","BOOKS_AND_REFERENCE","MUSIC_AND_AUDIO","SOCIAL","BUSINESS","LIFESTYLE","COMMUNICATION","PRODUCTIVITY","FINANCE","FOOD_AND_DRINK","SHOPPING","MAPS_AND_NAVIGATION","MEDICAL","COMICS","VIDEO_PLAYERS","HOUSE_AND_HOME","DATING","LIBRARIES_AND_DEMO","NEWS_AND_MAGAZINES","HEALTH_AND_FITNESS","PARENTING","WEATHER"),
       cex = 0.5, fill = rainbow(length(f1)))

f2

g2 <- g1$genreId
g2
table(g2)
plot(g2)
plot(table(g2))


##################################


f1 <- filter(d9, d9$adSupported == 1)
g1 <- filter(d9, d9$adSupported == 0)

table(f1$genreId)
plot(table(f1$genreId))
