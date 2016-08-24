# Copyright 2016 Packt Publishing

# Introduction to R for Business Intelligence
# Chapter 5, Part 1 - Cluster Analysis: K-means Clustering

message("Introduction to R for Business Intelligence
Chapter 5 - Data Mining with Cluster Analysis
Copyright (2016) Packt Publishing \n
Welcome. Let's learn k-means clustering")

#
# Partitioning Using K-means Clustering

# Exploring the Data
stations <- read.csv("./data/Ch5_bike_station_locations.csv")
summary(stations)

par(mfrow = c(1, 3))
hist(stations$latitude, col = 'gray')
hist(stations$longitude, ylim = c(0, 60), col = 'gray')
plot(stations$longitude, stations$latitude, asp = 1)
par(mfrow = c(1, 1))

# Running the kmeans() Function
set.seed(123)
two <- kmeans(stations, 2)
three <- kmeans(stations, 3)

# Interpreting the Model Output
three

two$centers
two$size

clus <- cbind(stations, clus2 = two$cluster,
              clus3 = three$cluster)
head(clus)

# Developing a Business Case
par(mfrow = c(1, 2))
plot(clus$longitude, clus$latitude, col = two$cluster, asp = 1,
     pch = two$cluster, main = "Sites for two kiosks",
     xlab = "Longitude",  ylab = "Latitude")
points(two$centers[ ,2], two$centers[ ,1], pch = 23,
       col = 'maroon', bg = 'lightblue', cex = 3)
text(two$centers[ ,2], two$centers[ ,1], cex = 1.1, 
     col = 'black', attributes(two$centers)$dimnames[[1]])

plot(clus$longitude, clus$latitude, col = three$cluster, asp = 1,
     pch = three$cluster, main = "Sites for three kiosks",
     xlab = "Longitude",  ylab = "Latitude")
points(three$centers[ ,2], three$centers[ ,1],
       pch = 23, col = 'maroon', bg = 'lightblue', cex = 3)
text(three$centers[ ,2], three$centers[ ,1], cex = 1.1,
     col = 'black', attributes(three$centers)$dimnames[[1]])
par(mfrow = c(1, 1))

hybrid <- cbind(clus, hybrid_shape = rep(0, dim(clus)[1]))

for (e in 1:dim(hybrid[1])[1]) {
     if (hybrid[e, 3] == hybrid[e, 4]) {
          hybrid[e, 5] <- hybrid[e, 3]
     }
     if (hybrid[e, 3] != hybrid[e, 4]) {
          hybrid[e, 5] <- hybrid[e ,3] + 15
     }
}     

plot(hybrid$longitude, hybrid$latitude, col = two$cluster,
     main = "Hybrid: Two-cluster kiosks in three-cluster locations", pch = hybrid$hybrid_shape, cex = 1.1,
     xlab = "Longitude",  ylab = "Latitude", asp = 1)
points(three$centers[1:2, 2], three$centers[1:2, 1],
       pch = 23, col = 'maroon', bg = 'lightblue', cex = 3)
text(three$centers[1:2, 2], three$centers[1:2, 1], cex = 1.1,
     col = 'black', attributes(two$centers)$dimnames[[1]])

rm(hybrid, stations, e)

# ----------------------------------------------------
# Additional calculations to support the business case
# ----------------------------------------------------

# create dataframe with columns for distances
compare <- cbind(clus, dist2 = rep(0, dim(clus)[1]),
                 dist3 = rep(0, dim(clus)[1]))

# -------
# functions to find distance based on Spherical Law of Cosines

distance2 <- function(lat, long, clus_id) {
     acos(sin(lat * pi / 180) * sin(two$centers[clus_id, 1] * pi / 180) +
          cos(lat * pi / 180) * cos(two$centers[clus_id, 1] * pi / 180) *
          cos(two$centers[clus_id, 2] * pi / 180 - long * pi / 180)) *
          6371  #in km
}

distance3 <- function(lat, long, clus_id) {
     acos(sin(lat * pi / 180)*sin(three$centers[clus_id, 1] * pi / 180) +
          cos(lat * pi / 180) * cos(three$centers[clus_id, 1] * pi / 180) *
          cos(three$centers[clus_id, 2] * pi / 180 - long * pi / 180)) *
          6371  #in km
}

# -------

for (e in 1:dim(compare[1])[1]) {
     compare[e, 5] <- distance2(compare[e, 1], compare[e, 2], compare[e, 3])
     compare[e, 6] <- distance3(compare[e, 1], compare[e, 2], compare[e, 4])
}

if(!require("dplyr")) install.packages("dplyr")
suppressMessages(suppressWarnings(library(dplyr)))
compare <- cbind(compare, hybrid = rep(0, dim(compare)[1]))

for (e in 1:dim(compare[1])[1]) {
     compare[e, 7] <- distance3(compare[e, 1], compare[e, 2], compare[e, 3])
}

compare <- mutate(compare, temp_increase = (hybrid - dist3))

# -------

par(mfrow = c(1, 3))
hist(compare[ ,5], ylim = c(0, 80), xlim = c(0, 8), col = "lightgray", 
     xlab = "Dist (km)", main = "Two Kiosks")
abline(v = mean(compare[ ,5]), lty = "dashed")
hist(compare[ ,6], ylim = c(0, 80), xlim = c(0, 8), col = "lightgray",
     xlab = "Dist (km)", main = "Three Kiosks")
abline(v = mean(compare[ ,6]), lty = "dashed")
hist(compare[ ,7], ylim = c(0, 80), xlim = c(0, 8), col = "lightgray",
     xlab = "Dist (km)", main = "Two (Hybrid Solution)")
abline(v = mean(compare[ ,7]), lty = "dashed")
par(mfrow = c(1, 1))

summary(compare)  #to indicate max

hist(compare$temp_increase, breaks = 4, xlab = "Distance (km)",
     main = "Distance Increase: Building Two Kiosks at Future Locations", col = "gray")

bins <- as.data.frame(table(cut(compare$temp_increase, breaks = c(-1:5))))
text(seq(-.5, 4.5, 1), 50, cex = 1.1, col = 'black', bins[ ,2])

increase <- filter(compare, temp_increase > 0)
increase <- increase[ ,-c(1:7)]

summary(increase)

text(2, 80, paste("Average increase:", round(mean(increase) *
                    0.62137119, 2), "miles")) # .62 mi/km