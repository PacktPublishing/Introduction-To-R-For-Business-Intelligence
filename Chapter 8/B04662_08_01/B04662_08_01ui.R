# Copyright 2016 Packt Publishing

# Introduction to R for Business Intelligence
# Chapter 8, ui.R file - Web Dashboards with Shiny

if(!require("shiny")) install.packages("shiny")
suppressMessages(suppressWarnings(library(shiny)))

shinyUI(fluidPage(
     
     titlePanel("Revenue Prediction from Marketing Expenditures"),
     
     sidebarLayout(
          sidebarPanel(
               sliderInput("spend", "Expenditure Level in $K:",
                           min = 54, max = 481, value = 250)
               ),
          mainPanel(
               plotOutput("prediction_plot")
               )
          )
     ))