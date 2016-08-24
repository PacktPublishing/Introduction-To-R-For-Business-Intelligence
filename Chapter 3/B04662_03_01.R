# Copyright 2016 Packt Publishing

# Introduction to R for Business Intelligence
# Chapter 3 - Exploratory Data Analysis

message("Introduction to R for Business Intelligence
        Chapter 3 - Exploratory Data Analysis
        Copyright (2016) Packt Publishing \n
        This is your introduction to data exploration")

#
# Analyzing a Single Data Variable
marketing <- read.csv("./data/Ch3_marketing.csv",
                      stringsAsFactors = TRUE)
str(marketing)

marketing$pop_density <- factor(marketing$pop_density,
                                ordered = TRUE,
                                levels = c("Low", "Medium",
                                           "High"))

# Tabular exploration

summary(marketing$google_adwords)
fivenum(marketing$google_adwords)

mean(marketing$google_adwords) 
sd(marketing$google_adwords) 
var(marketing$google_adwords)

summary(marketing$pop_density)

# Graphical exploration

data("anscombe")
anscombe

sapply(anscombe, mean)
sapply(anscombe, sd)
sapply(anscombe, var)

plot(marketing$pop_density)

par(mfrow = c(1, 2))
boxplot(marketing$google_adwords, ylab = "Expenditures")
hist(marketing$google_adwords, main = NULL)

summary(marketing$twitter) 

boxplot(marketing$twitter, ylab = "Expenditures", col = "gray")
hist(marketing$twitter, main = NULL, col = "blue")
par(mfrow = c(1, 1), lty = 1)

#
# Analyzing Two Variables Together

# What does the data look like?

marketing$emp_factor <- cut(marketing$employees, 2)

summary(marketing)

# Is there any relationship between two variables?

table(marketing$emp_factor, marketing$pop_density)

par(mfrow = c(1, 3))
mosaicplot(table(marketing$pop_density, marketing$emp_factor),
           col=c("gray","black"), main = "Factor / Factor")
boxplot(marketing$marketing_total ~ marketing$pop_density,
        main = "Factor / Numeric")
plot(marketing$google_adwords, marketing$revenues,
     main = "Numeric / Numeric")

# Is there any correlation between the two?

cor(marketing$google_adwords, marketing$revenues)
cor(marketing$google_adwords, marketing$facebook)

# Is the correlation significant?

cor.test(marketing$google_adwords, marketing$revenues)

cor.test(marketing$twitter, marketing$revenues)
cor.test(marketing$facebook, marketing$revenues)

cheese <- c(9.3, 9.7, 9.7, 9.7, 9.9, 10.2, 10.5, 11, 10.6, 10.6)
degrees <- c(480, 501, 540, 552, 547, 622, 655, 701, 712, 708)
cor(cheese, degrees)
cor.test(cheese, degrees)

cor.test(marketing$google_adwords, marketing$facebook)

cor.test(marketing$revenues, marketing$marketing_total)

plot(marketing$google_adwords, marketing$revenues,
     main = "Cor: 0.766")
plot(marketing$google_adwords, marketing$facebook,
     main = "Cor: 0.076")
plot(marketing$marketing_total, marketing$revenues,
     main = "Cor: 0.853")
par(mfrow = c(1, 1))

marketing$emp_factor <- NULL

#
# Exploring Multiple Variables Simultaneously

# Look

summary(marketing)

# Relationships

pairs(marketing)

# Correlations

cor(marketing[ ,1:6])

# Significance

if(!require("psych")) install.packages("psych")
suppressMessages(suppressWarnings(library(psych)))
corr.test(marketing[ ,1:6])

if(!require("corrgram")) install.packages("corrgram")
suppressMessages(suppressWarnings(library(corrgram)))
corrgram(marketing[ ,1:6], order = FALSE,
         main = "Correlogram of Marketing Data, Unordered",
         lower.panel = panel.conf, upper.panel = panel.ellipse,
         diag.panel = panel.minmax, text.panel = panel.txt)

corrgram(marketing[ ,1:6], order = TRUE,
         main = "Correlogram of Marketing Data, Ordered",
         lower.panel = panel.shade, upper.panel = panel.pie,
         diag.panel = panel.minmax, text.panel = panel.txt)

#
# Further Practice

# Read in data and reconvert data
bike <- read.csv("./data/Ch2_clean_bike_sharing_data.csv",
                 stringsAsFactors = TRUE)

bike$season <- factor(bike$season, ordered = TRUE,
                      levels = c("spring", "summer",
                                 "fall", "winter"))
bike$weather <- factor(bike$weather, ordered = TRUE,
                       levels = c("clr_part_cloud", 
                                  "mist_cloudy",
                                  "lt_rain_snow",
                                  "hvy_rain_snow"))

if(!require("lubridate")) install.packages("lubridate")
suppressMessages(suppressWarnings(library(lubridate)))
bike$datetime <- ymd_hms(bike$datetime)

# Question 1
str(bike)

# Question 2
table(bike$season)

# Question 3
mean(bike$temp)
sd(bike$temp)

# Question 4
hist(bike$temp)

# Question 5
plot(bike$workingday)
hist(bike$casual)

# Question 6
nbike <- bike[ ,-c(1:5, 13)]
cor(nbike)

# Question 7
png('bikePairs.png', width = 15, height = 15,
    units = 'in', res = 300) #creates file in working dir
pairs(bike) #this could take a couple minutes
dev.off()

corr.test(nbike)

corrgram(nbike, order = FALSE,
         main = "Correlogram of Bike Data",
         lower.panel = panel.conf, upper.panel = panel.ellipse,
         diag.panel = panel.minmax, text.panel = panel.txt)

corrgram(nbike, order = TRUE,
         main = "Correlogram of Bike Data",
         lower.panel = panel.shade, upper.panel = panel.pie,
         diag.panel = panel.minmax, text.panel = panel.txt)