library(dplyr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(cowplot)
library(boot)

alc <- read.csv("~/IODS-project/data/alc.csv", header = T, sep = ",")

alc <- alc[,-1]

dim(alc)
colnames(alc)

#We have 382 observations and 35 variables.
#The variables and observations are from two different datasets that can be accessed from:
#https://archive.ics.uci.edu/ml/machine-learning-databases/00320/

#Meta text can be can accessed here: https://archive.ics.uci.edu/ml/datasets/Student+Performance

#From the metatext:
#"This data approach student achievement in secondary education of two Portuguese schools. The data attributes include student grades, demographic, social and school related features) and it was collected by using school reports and questionnaires."

#We are going to study high/low alcohol consumption and its relationships to "extra-curricular activities", "romantic relationship", "quality of family relationships" (from 1 to 5) and "going out with friends"
#Next we take subset of the variables chosen

variables <- c("activities", "romantic", "famrel", "goout", "high_use")
new_alc <- select(alc,one_of(variables))
str(new_alc)

#Hypothesis is that social activities and other social attributes correlate negatively with high use of alcohol
#Or in other words we hypothise that better social life correlates with less alcohol consumption

#Next we plot the variables with the alcohol use

alc_act <- ggplot(data = new_alc, aes(x = activities, fill = high_use)) + geom_bar() + ggtitle("Consumption of alcohol and activities")
alc_act

alc_rom <- ggplot(data = new_alc, aes(x = romantic, fill = high_use)) + geom_bar() + ggtitle("Consumption of alcohol and romantic relationship")
alc_rom

alc_family <- ggplot(data = new_alc, aes(x = famrel, fill = high_use)) + geom_bar() + ggtitle("Consumption of alcohol and quality of family relationship")
alc_family

alc_goout <- ggplot(data = new_alc, aes(x = goout, fill = high_use)) + geom_bar() + ggtitle("Consumption of alcohol and going out with friends")
alc_goout

#Some findings from these plots:
#1. Extra-curricular activities correlate with less alcohol consumptions (There is more observations for those who have activities but about same amount for high_use observations)
#2. It looks like good family relations correlate with less alcohol consumptions.
#3. People who go out with friends look like to be more probable to be high users of alcohol.

#Next we take a look at the numbers:

new_alc %>% group_by(activities,high_use) %>% summarise(count = n())

#1. Extra-curricular activities correlate with less alcohol consumptions

new_alc %>% group_by(romantic,high_use) %>% summarise(count = n())                      

#2. Those who are not in a romantic relationship are more likely to be high users

new_alc %>% group_by(famrel,high_use) %>% summarise(count = n())

new_alc %>% group_by(goout,high_use) %>% summarise(count = n())

#We make the same observations from the numbers than from the plots

#We proceed to make the statistical model

m1 <- glm(high_use ~ activities + romantic + famrel + goout, data = new_alc, family = "binomial")

#Summary of the model
summary(m1)

#Only "quality of family relationships" and "going out with friends" are statistically significant
#Let's check if the model is better with only the significant variables. 

step(m1, direction = "backward")

#AIC is smallest with the model with only the significant variables, so we need to remove the others.

m2 <- glm(high_use ~famrel + goout, data = new_alc, family = "binomial")

#Summary of the model
summary(m2)

#Coefficients of the model (all are statistically significant at 0.01 risk level)
coef(m2)

#Odds ratios (OR)
OR <- coef(m2) %>% exp


#Confidence intervals (CI)
CI <- confint(m2) %>% exp

# print out the odds ratios with their confidence intervals
cbind(OR, CI)

#Our result is that one unit positive change in family relations means that you are 0.69 "more likely" to be a high drinker.
#Similarly one unit positive change in going out with friends means that you are 2.2 more likely to be a high drinker.
#Family relations supports our initial hypothesis.

#Next we make predictions on the basis of our model

# prediction for the probability of high_use
probabilities <- predict(m2, type = "response")

new_alc <- mutate(new_alc, probability = probabilities)

# using the probabilities to make a prediction of high_use
new_alc <- mutate(new_alc, prediction = probability > 0.5)

# tabulate the target variable versus the predictions
table(high_use = new_alc$high_use, prediction = new_alc$prediction)

table(high_use = new_alc$high_use, prediction = new_alc$prediction) %>% prop.table %>% addmargins()

#We can see from the table below that the model is predicting correctly High use 243+46 times (out of 243+68+25+46) and wrongly 68+25 times.
#Model predicts about 25 % of the times wrong -> less than simple guessing.

#BONUS questions: 10-fold cross-validation

loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# Average number of wrong predictions in the (training) data
loss_func(class = new_alc$high_use, prob = new_alc$probability)

# K-fold cross-validation
library(boot)
cv <- cv.glm(data = new_alc, cost = loss_func, glmfit = m2, K = 10)

# average number of wrong predictions in the cross validation
cv$delta[1]

#The model introduced has similar prediction rate compared to datacamp exercise (0.26)