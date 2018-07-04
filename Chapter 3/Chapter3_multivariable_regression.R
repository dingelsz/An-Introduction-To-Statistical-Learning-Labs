# Load data
library(MASS)
library(ISLR)
library(car)

# Linear regression with two predictors
lm.fit <- lm(medv ~ lstat + age, data=Boston)

summary(lm.fit)

# Linear regression with all the predictors
lm.fit <- lm(medv ~ ., data=Boston)
summary(lm.fit)

# Lets look at VIFs
vif(lm.fit)

# Lets create a model without age:
lm.fit <- lm(medv ~ . -age, data=Boston)
summary(lm.fit)
