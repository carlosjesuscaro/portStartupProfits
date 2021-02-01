# Project: Startup profit analysis
# Algorithm: ANCOVA

# Setting the working directory
setwd('~/OneDrive - Data ScienceTech Institute/Datasets/startup/')

# Loading the required libraries
library(dplyr)

# Loading the data sets
data = read.csv('50_Startups.csv')
glimpse(data)
head(data)
attach(data)

# Data pre-analysis
summary(data)
sapply(data, class)
sapply(data, sum(is.na(data)))
data$State <- as.factor(data$State)

# Plotting







model = lm(Profit ~ R.D.Spend + Marketing.Spend + Administration + State)
boxplot(Profit~State)
summary(model)=
anova(model)
plot(model)
plot(Profit ~ R.D.Spend)
shapiro.test(model$residuals)
bartlett.test(Profit, State)
