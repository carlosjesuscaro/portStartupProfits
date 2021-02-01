# Project: Startup profit analysis
# Algorithm: ANCOVA

# Setting the working directory
setwd('~/OneDrive - Data ScienceTech Institute/Datasets/startup/')

# Loading the required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Loading the data sets
data = read.csv('50_Startups.csv')
glimpse(data)
head(data)
attach(data)

# Data pre-analysis
summary(data)
sapply(data, class)
data$State <- as.factor(data$State)
# Checking for NAs
lapply(data, is.na)

# Summarize information
data %>% group_by(State) %>%
  summarize(Avg_RD = mean(R.D.Spend), Avg_Admin = mean(Administration),
            Avg_Mark = mean(Marketing.Spend), Avg_Profit = mean(Profit))

# Plotting
plot(Profit, R.D.Spend)








model = lm(Profit ~ R.D.Spend + Marketing.Spend + Administration + State)
boxplot(Profit~State)
summary(model)=
anova(model)
plot(model)
plot(Profit ~ R.D.Spend)
shapiro.test(model$residuals)
bartlett.test(Profit, State)
