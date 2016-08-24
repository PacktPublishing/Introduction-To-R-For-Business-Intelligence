# Copyright 2016 Packt Publishing

# Introduction to R for Business Intelligence
# Chapter 5, Part 2 - Cluster Analysis: Hierarchical Clustering

message("Introduction to R for Business Intelligence
Chapter 5 - Data Mining with Cluster Analysis
Copyright (2016) Packt Publishing \n
Let's continue to learn hierarchical cluster analysis")

#
# Clustering Using Hierarchical Techniques

# Cleaning and Exploring the Data
market <- read.csv("./data/Ch5_age_income_data.csv")
str(market)
summary(market)

par(mfrow = c(1, 2))
boxplot(market$age ~ market$bin, main = "Explore Age")
boxplot(market$income ~ market$bin, main = "Explore Income")
par(mfrow = c(1, 1))

cor.test(market$age, market$income)

set.seed(789)
three <- kmeans(market[, 2:3], 3)
plot(market$age, market$inc, col = three$cluster, xlab = 'age',
     ylab = 'income', main = 'K-means without Scaling')
points(three$centers[, 1], three$centers[, 2], 
       pch = 23, col = 'maroon', bg = 'lightblue', cex = 3)
text(three$centers[, 1], three$centers[, 2], cex = 1.1,
     col = 'black', attributes(three$centers)$dimnames[[1]])
rm(three)

market$age_scale <- as.numeric(scale(market$age))
market$inc_scale <- as.numeric(scale(market$income))

set.seed(789)
three <- kmeans(market[, 4:5], 3)
plot(market$age_scale, market$inc_scale, col=three$cluster,
     xlab = 'age', ylab = 'income',
     main = 'K-means with Scaling')
points(three$centers[, 1], three$centers[, 2], 
       pch = 23, col = 'maroon', bg = 'lightblue', cex = 3)
text(three$centers[, 1], three$centers[, 2], cex = 1.1,
     col = 'black', attributes(three$centers)$dimnames[[1]])
rm(three)

# Running the hclust() Function
set.seed(456)
hc_mod <- hclust(dist(market[, 4:5]), method = "ward.D2")

# Visualizing the Model Output
dend <- as.dendrogram(hc_mod)

if(!require("dendextend")) install.packages("dendextend")
suppressMessages(suppressWarnings(library(dendextend)))
dend_six_color <- color_branches(dend, k = 6)
plot(dend_six_color, leaflab = "none", horiz = TRUE,
     main = "Age and Income Dendrogram", xlab = "Height")
abline(v = 37.5, lty = 'dashed', col = 'blue')

str(cut(dend, h = 37.5)$upper)

rm(dend_six_color)

set.seed(456)
two <- kmeans(market[, 4:5], 2)
three <- kmeans(market[, 4:5], 3)
four <- kmeans(market[, 4:5], 4)
five <- kmeans(market[, 4:5], 5)
six <- kmeans(market[, 4:5], 6)
seven <- kmeans(market[, 4:5], 7)
eight <- kmeans(market[, 4:5], 8)
nine <- kmeans(market[, 4:5], 9)
ten <- kmeans(market[, 4:5], 10)

# Evaluting the Models
optimize <- data.frame(clusters = c(2:10), wss = rep(0, 9))
optimize[1, 2] <- as.numeric(two$tot.withinss)
optimize[2, 2] <- as.numeric(three$tot.withinss)
optimize[3, 2] <- as.numeric(four$tot.withinss)
optimize[4, 2] <- as.numeric(five$tot.withinss)
optimize[5, 2] <- as.numeric(six$tot.withinss)
optimize[6, 2] <- as.numeric(seven$tot.withinss)
optimize[7, 2] <- as.numeric(eight$tot.withinss)
optimize[8, 2] <- as.numeric(nine$tot.withinss)
optimize[9, 2] <- as.numeric(ten$tot.withinss)

plot(optimize$wss ~ optimize$clusters, type = "b", 
     ylim = c(0, 12000), ylab = 'Within Sum of Square Error',
     main = 'Finding Optimal Number of Clusters Based on Error',
     xlab = 'Number of Clusters', pch = 17, col = 'black')
rm(optimize)

three$size; four$size; five$size; six$size; seven$size
rm(two, three, four, seven, eight, nine, ten)

market$clus5 <- five$cluster
dend_five <- cutree(dend, k = 5)
market$dend5 <- dend_five

market$clus6 <- six$cluster
dend_six <- cutree(dend, k = 6)
market$dend6 <- dend_six

# Choosing a Model
par(mfrow = c(2, 2), mar = c(3, 4, 4, 2) + 0.1)
plot(market$age, market$income, col = five$cluster,
     pch = five$cluster, xlab = '', main = '5-means Clustering')
plot(market$age, market$income, col = six$cluster, xlab = '',
     ylab = '', pch = six$cluster, main = '6-means Clustering')
par(mar = c(5, 4, 2, 2) + 0.1)
plot(market$age, market$income, col = market$dend5,
     pch = market$dend5, main = 'k = 5 Hierarchical')
plot(market$age, market$income, col = market$dend6, ylab = '', 
     pch = market$dend6, main = 'k = 6 Hierarchical')
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

# Preparing the Results
if(!require("dplyr")) install.packages("dplyr")
suppressMessages(suppressWarnings(library(dplyr)))
labels <- as.data.frame(market %>% 
     group_by(dend6) %>% 
     summarise(avg_age = median(age), avg_inc = median(income)))

plot(market$age, market$income, col = market$dend6,
     pch = market$dend6 - 1, xlab = "Age", ylab = "Income",
     main = 'Marketing Clusters from Hierarchical Clustering \n (Labels show medians of age and income for cluster)')
points(labels[ ,2], labels[ ,3], pch = 21, col = 'maroon',
       bg = 'white', cex = 3)
text(labels[, 2], labels[, 3], cex = 1.1, col = 'black',
     labels[, 1])

market %>% group_by(dend6) %>% summarise(ClusterSize = n())

market %>% group_by(dend6) %>% 
     summarise(min_age = min(age), med_age = median(age),
               max_age = max(age), med_inc = median(income), 
               min_inc = min(income), max_inc = max(income))