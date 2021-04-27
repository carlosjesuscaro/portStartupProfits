# Project: Startup profit analysis
# Algorithm: ANCOVA

# Setting the working directory
setwd('~/OneDrive - Data ScienceTech Institute/Datasets/startup/')

# Loading the required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggcorrplot)
library(lmtest)

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
sum(is.na(data))

# Summarize information
data %>% group_by(State) %>%
  summarize(Avg_RD = mean(R.D.Spend), Avg_Admin = mean(Administration),
            Avg_Mark = mean(Marketing.Spend), Avg_Profit = mean(Profit))

# Boxplot: Profit ~ State
ggplot(data, aes(x=State, y=Profit, fill=State)) + 
  geom_boxplot(alpha=0.5) +
  theme(legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid")) +
  scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "K")) +
  ggtitle('Boxplot: Profit ~ State') + theme(plot.title = element_text(hjust = 0.5))

# Plotting 
df <- gather(data, key = Department, value = Budget, c("R.D.Spend","Marketing.Spend","Administration"))
ggplot(df, aes(x=Budget, y=Profit, group = Department, color = Department)) + geom_point() +
  scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "K")) +
  scale_x_continuous(labels = scales::dollar_format(prefix="$", suffix = "K")) +
  ggtitle('Profit vs RD - Marketing - Administration') + 
    theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    legend.position = c(.84, .16),
    legend.justification = c("left", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

# Heatmap and correlation analysis among all quantitative variables
data_corr <- cor(data %>% select(-State))      
ggcorrplot(data_corr, type = "lower") + ggtitle('Correlation among variables')

# Modeling
model = lm(Profit ~ R.D.Spend + Marketing.Spend + Administration  + State)
summary(model)
anova(model)

# Verifying the pre-conditions
hist(model$residuals, freq = FALSE)
shapiro.test(model$residuals)
plot(model)

# Plotting real vs predicted
data_caret <- data # To be used with the caret library
Predicted <- model$fitted.values
data <- cbind(data, Predicted)
data <- data %>% rename(Real = Profit)

df <- gather(data, key = Value, value = Startup_Profit, c("Real","Predicted"))
ggplot(df, aes(x=R.D.Spend, y=Startup_Profit, group = Value, color = Value)) + geom_point() +
  scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "K")) +
  scale_x_continuous(labels = scales::dollar_format(prefix="$", suffix = "K")) +
  ggtitle('Profit: Real vs Predicted') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    legend.position = c(.84, .16),
    legend.justification = c("left", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

# Using the caret library
lm1 <- train(Profit~., data = data_caret, method="lm")
lm1$results$Rsquared

