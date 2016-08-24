# Copyright 2016 Packt Publishing

# Introduction to R for Business Intelligence
# Chapter 1 - Extract, Transform, and Load

message("Introduction to R for Business Intelligence
        Chapter 1 - Extract, Transform, and Load
        Copyright (2016) Packt Publishing \n
        This is your introduction to ETL")

#
# Extracting Data from Sources

getwd()

bike <- read.csv("./data/Ch1_bike_sharing_data.csv")
str(bike)

bike <- read.table("./data/Ch1_bike_sharing_data.csv",
                   sep = ",", header = TRUE)

#
# Transforming Data to Fit Analytic Needs

if(!require("dplyr")) install.packages("dplyr")
suppressMessages(suppressWarnings(library(dplyr)))
extracted_rows <- filter(bike, registered == 0, season == 1 | season == 2)
dim(extracted_rows)

using_membership <- filter(bike, registered == 0, season %in% c(1, 2))

identical(extracted_rows, using_membership)

extracted_columns <- select(extracted_rows, season, casual)

add_revenue <- mutate(extracted_columns, revenue = casual * 5)

grouped <- group_by(add_revenue, season)
report <- summarise(grouped, sum(casual), sum(revenue))

#
# Loading Data into Business Systems for Analysis

write.csv(report, "revenue_report.csv", row.names = FALSE)

write.table(report, "revenue_report.txt", row.names = FALSE, sep = "\t")