# Load data
library(MASS)
library(ISLR)

names(Boston)

# Create the linear model
lm.fit <- lm(Boston$medv ~ Boston$lstat)
attach(Boston)
lm.fit <- lm(medv ~ lstat)

# Review the linear model
summary(lm.fit)
names(lm.fit)
coef(lm.fit)

# Confidence interval for the parameter
confint(lm.fit)

# Confidence and prediction intervals
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "confidence")
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "prediction")

# Lets plot our data and regression line
plot(medv ~ lstat)
abline(lm.fit, col="red")

# The residual plot does not look random so there might be a non
# linear relationship between the two predictors
par(mar=c(5,5,5,5), mfrow = c(1,1))
plot(lm.fit)

# Lets look at residual plots to find outliers
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

# Lets look at leverage points
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
