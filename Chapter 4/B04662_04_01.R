# Copyright 2016 Packt Publishing

# Introduction to R for Business Intelligence
# Chapter 4 - Linear Regression for Business

message("Introduction to R for Business Intelligence 
        Chapter 4 - Linear Regression for Business
        Copyright (2016) Packt Publishing \n
        Welcome. Let's learn linear regression.")

#
# Understanding Linear Regression

# The lm() function

adverts <- read.csv("./data/Ch4_marketing.csv"); str(adverts)

pairs(adverts)
plot(adverts$marketing_total, adverts$revenues, ylab = "Revenues", 
     xlab = "Marketing Total", main = "Revenues and Marketing")

# Simple linear regression (SLR)

model1 <- lm(revenues ~ marketing_total, data = adverts)
model1

# Residuals

plot(adverts$marketing_total, adverts$revenues, 
     ylab = "Revenues", xlab = "Marketing",
     main = "Revenues versus Marketing", 
     xlim = c(50, 150), xaxt = "n")
abline(model1)
segments(x0 = 53.6, y0 = 31.0, x1 = 53.6, y1 = 34.68, lwd = 2,
         lty = 3)
text(x = 57.5, y = 30.5, labels = "A")
text(x = 54.0, y = 36.3, labels = "P")
axis(1, at = c(53.6, 100, 150, 200, 250)) 

#
# Checking Model Assumptions

# Normality

par(mfrow = c(1, 2))
hist(model1$residuals, xlab = "Residuals", col = "gray",
     main = "Residuals Distribution")

qqnorm(model1$residuals, main = "Q-Q Plot of Residuals")
qqline(model1$residuals) 

# Equal variance

par(mfrow = c(1, 1))
plot(model1$fitted.values, model1$residuals, ylab = "Residuals",
     xlab = "Fitted Values", main = "Residuals Distribution")
abline(0, 0, lwd = 3); abline(h = c(-6.5, 6.5), lwd = 3, lty = 3)

#
# Using a Simple Linear Regression

# Interpreting model output

summary(model1)

# Predicting unknown outputs with an SLR

range(adverts$marketing_total)

if(!require("dplyr")) install.packages("dplyr")
suppressMessages(suppressWarnings(library(dplyr)))
select(adverts, marketing_total) %>% filter(marketing_total > 430)

newdata <- data.frame(marketing_total = 460)
predict.lm(model1, newdata, interval = "predict")

predict.lm(model1, newdata, level = 0.99, interval = "predict")
predict.lm(model1, newdata, level = 0.90, interval = "predict")

newdata = data.frame(marketing_total = c(450, 460, 470))
predict.lm(model1, newdata, interval = "predict")

# Working with big data using confidence intervals

set.seed(4510)
market_sample <- sample_frac(adverts, 0.30, replace = FALSE)
samp_model <- lm(revenues ~ marketing_total, 
                 data = market_sample)
samp_model
confint(samp_model)

#
# Refining Data for Simple Linear Regression

x0 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
y0 <- c(1.00, 1.41, 1.73, 2.00, 2.24,
        2.45, 2.65, 2.83, 3.00, 3.16)
fit0 <- lm(y0 ~ x0)

par(mfrow = c(1, 3))
plot(x0, y0, pch = 19, main = "Linearity?"); abline(fit0)
hist(fit0$residuals, main = "Normality?", col = "gray")
plot(fit0$fitted.values, fit0$residuals, 
     main = "Equal Variance?", pch = 19); abline(h = 0)

# Transforming data

y0_t <- y0^2
fit0_t <- lm(y0_t ~ x0)

plot(x0, y0_t, pch = 19, main = "Linear"); abline(fit0_t)
hist(fit0_t$residuals, main = "Normal", col = "gray")
plot(fit0_t$fitted.values, fit0_t$residuals, 
     main = "Equal Variance", pch = 19); abline(h = 0)
par(mfrow = c(1, 1))

if(!require("MASS")) install.packages("MASS")
suppressMessages(suppressWarnings(library(MASS)))
boxcox(fit0)

x1 <- c(1, 5, 15, 30, 60, 120, 240, 480,
        720, 1440, 2880, 5760, 10080)
y1 <- c(0.84, 0.71, 0.61, 0.56, 0.54, 0.47,
        0.45, 0.38, 0.36, 0.26, 0.2, 0.16, 0.08)
fit1 <- lm(y1 ~ x1)

par(mfrow = c(1, 3))
plot(x1, y1, pch = 19, main = "Linearity?"); abline(fit1)
hist(fit1$residuals, main = "Normality?", col = "gray")
plot(fit1$fitted.values, fit1$residuals, 
     main = "Equal Variance?", pch = 19); abline(h = 0)

x1_t <- log(x1)
fit1_t <- lm(y1 ~ x1_t)

plot(x1_t, y1, pch = 19, main = "Linear"); abline(fit1_t)
hist(fit1_t$residuals, main = "Normal", col = "gray")
plot(fit1_t$fitted.values, fit1_t$residuals, 
     main = "Equal Variance", pch = 19); abline(h = 0)

# Handling outliers and influential points

x2 <- 1:20
y2 <- c(1:10, 4, 12:20)
fit2 <- lm(y2 ~ x2)

x3 <- c(1:20, 30)
y3 <- c(0.4, 2.2, 2.2, 5.6, 5.3, 5.2, 7.5, 8.7, 9.6, 9.7, 12.5,
        12.4, 12.4, 11.8, 16.1, 16, 17, 18.9, 19.8, 20.6, 30.0)
fit3 <-lm(y3 ~ x3)

par(mfrow = c(1, 2))
plot(x2, y2, pch = 19, main = "Outlier is Influential")
abline(fit2)
plot(x3, y3, pch = 19, main = "Outlier is not Influential")
abline(fit3)

#------------
# You practice

x4 <- c(1:20)
y4 <- c(0.4, 2.2, 2.2, 5.6, 5.3, 5.2, 7.5, 8.7,
        9.6, 9.7, 12.5, 12.4, 12.4, 12.8, 16.1,
        16.0, 17.0, 11.5, 19.8, 20.6)
fit4 <- lm(y4 ~ x4)

par(mfrow = c(1, 1))
plot(x4, y4, pch = 19, col = "blue")
abline(fit4)
summary(fit4)

x4_t <- x4[-18]
y4_t <-y4[-18]
fit4_t <- lm(y4_t ~ x4_t)
summary(fit4_t)

par(mfrow = c(2, 2))
plot(fit4)
#------------

#
# Introduction to Multiple Linear Regression

model2 <- lm(revenues ~ google_adwords + facebook + twitter,
             data = adverts)

plot(model2)
summary(model2)
