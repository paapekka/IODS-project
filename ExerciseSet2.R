library(dplyr)
library(GGally)
library(ggplot2)

dim(learning2014)
str(learning2014)


#We have 7 variables which have 166 observations
#We have variables for student's age and gender, attitude towards statistics, deep- , strategic- and surface learning AND exam points.
#Metatext which explains the variables can be found here (linkki)

#Next we take a look at the graphical overview of the data
#Different genders are highlighted with different colours
p <- ggpairs(learning2014, mapping = aes(col = gender, alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))
p

#We can see correlations between variables written in black text.
#For example highest correlation is between attitude and points.
#In red we have correlations for women and in blue correlations for men.
#We have also distributions for men and women for all the variables, we see some slight differences between genders but not anything significant.

#Here have summary of the data
summary(learning2014)

#We can see that we have more female participants than men (110 vs. 56).

#Next we choose model with three explanatory variables to exaplain exam points.
#We choose variables with biggest correlation (attitude, strategic- and surface learning)
my_model <- lm(Points ~ attitude + stra + surf, data = learning2014)
summary(my_model)

#We can see that intercept and attitude are statistically significant at confidennce 0.001 level.
#Estimate for attitude is 3.39, which means that change in unit in attitude changes points by 3.39
#Other variables in this model are statistically insignificant.
#Adjusted R-squared is 0,19 which means that the model explains around 19 % of variation in exam points.


#We continue to remove strategic- and surface learning variables as they were statistically insignificant.
#New model is:
my_model2 <- lm(Points ~ attitude, data = learning2014)
summary(my_model2)

#Both the intercept and variable attitude are now statistically significant under 0,001 level.
#The estimate for attitude is slightly bigger in this second model (One unit change in attitude results to 3.52 change in exam points).
#Adjusted R-squared is now 0.1856 which means that the model explains around 18 % of the variation of points.

#Finally we produce diagnostics plots for Residuals vs Fitted values, Normal QQ-plot and Residuals vs Leverage.
par(mfrow = c(2,2))
plot(my_model2, which = c(1,2,5))

#From the first plot on the top-left we can see that residuals are looking to have mean zero.
#In the plot at top-right we can see that residuals are normally distributed.
#There is not significant leverage at the plot at bottom-left.