# Copyright 2016 Packt Publishing

# Introduction to R for Business Intelligence
# Chapter 8, server.R file - Web Dashboards with Shiny

if(!require("shiny")) install.packages("shiny")
suppressMessages(suppressWarnings(library(shiny)))

revenue <- read.csv("./data/Ch8_marketing.csv")
model <- lm(revenues ~ marketing_total, data = revenue)

shinyServer(function(input, output) {
     output$prediction_plot <- renderPlot({
          
          plot(revenue$marketing_total, revenue$revenues, 
               xlab = "Marketing Expenditures ($K)",
               ylab = "Revenues ($K)")
          abline(model, col = "blue")
          
          newdata <- data.frame(marketing_total = input$spend)
          pred <- predict.lm(model, newdata, interval = "predict")
          
          points(c(rep(input$spend, 2)), c(pred[2], pred[3]),
                 pch = "-", cex = 2, col = "orange")
          segments(input$spend, pred[2], input$spend, pred[3],
                   col = "orange", lty = 2, lwd = 2)
          points(input$spend, pred[1], pch = 19, col = "blue",
                 cex = 2)
          text(54, 55, pos = 4, cex = 1.0,
               paste0("Predicted revenues of $",
                      round(pred[1], 2) * 1000,
                      " range of {", round(pred[2], 2) * 1000,
                      " to ", round(pred[3], 2) * 1000, "}"))
          })
     })