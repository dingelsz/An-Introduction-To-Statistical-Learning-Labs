library(ISLR)

# 9 
plot(Auto)
correlations <- cor(Auto[,1:8])
lm.fit <- lm(mpg ~ .-name, data = Auto)

# There appears to be a relationship between some of the predictors
# and the response. Cylinders, Displacement, Weight, year and origin all
# appear to be statistically significant. The coefficient for the year
# variable suggests that new models tend to gain 0.75 mpg over previous 
# models. 
summary(lm.fit)

# Diagnostic plots - There tend to be a few outliers, mainly volkswagens 
plot(predict(lm.fit), rstudent(lm.fit))
Auto[which(abs(rstudent(lm.fit)) > 3),]

# Lets look at leverage points - There is a leverage point at 14. 
plot(hatvalues(lm.fit))
Auto[which.max(hatvalues(lm.fit)),]

# Lets add interactive terms
# It appears that Horsepower and weight is a good interation predictor
# while cylinders and acceleration isn't so great. 
lm.fit <- lm(mpg ~ .-name + horsepower:weight + cylinders:acceleration, data=Auto)
summary(lm.fit)

# Lets experiment with transforming predictors
# sqrt(horsepower) has a low p value when it is the only predictor but
# has a higher p value when adding in other tranformations
lm.fit1 <- lm(mpg ~ I(sqrt(horsepower)), data=Auto)
summary(lm.fit)
lm.fit2 <- lm(mpg ~ I(sqrt(horsepower))+I(log(horsepower))+I(horsepower^2), data=Auto)
summary(lm.fit)

anova(lm.fit1, lm.fit2)

# 10
# The price has a very small p value so it has statistical significance. 
# For every increase in dollar sales go down, on average, by 0.05. 
# A store being urban appears to have no significance.
# A store being in the US tends to add 1.2 to sales. 
lm.fit <- lm(Sales ~ Price + Urban + US, data=Carseats)
summary(lm.fit)


# We can reject the urban predictor because of it's high p value. 
# Our new model has almost identical RSE and R^2. 
lm.fit <- lm(Sales ~ Price + US, data=Carseats)
summary(lm.fit)

confint(lm.fit)

# Let's look at the diagnostic plots
# There appear to be a lot of outliers
plot(predict(lm.fit) ~ studres(lm.fit))

# There are also a couple of leverage points. 
plot(hatvalues(lm.fit))

