# Load data
library(MASS)
library(ISLR)

names(Boston)

# Create the linear model with an interaction term
# lstat * age is shorthand for lstat+age+lstat:age
lm.fit <- lm(medv ~ lstat * age, data=Boston)
summary(lm.fit)

# Non linear transformations:
lm.fit1 <- lm(medv ~ lstat, data = Boston)
lm.fit2 <- lm(medv ~ lstat + I(lstat^2), data=Boston)
summary(lm.fit2)

# Run an anova analysis to perform a hypothesis test
anova(lm.fit1, lm.fit2)
# This suggests that the model with the added term lstat^2 is better
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plot(lm.fit2)

# We can continue this with higher order polynomials:
lm.fit3 <- lm(medv ~ poly(lstat, 6), data = Boston) 
summary(lm.fit3)
# This shows that adding terms up to the power of 5 is significant 
# We can use other transformations:

lm.fitlog <- lm(medv ~ log(lstat), data = Boston)
summary(lm.fitlog)
plot(lm.fitlog)

lm.fitcos <- lm(medv ~ cos(lstat), data=Boston)
summary(lm.fitcos)
plot(lm.fitcos)
