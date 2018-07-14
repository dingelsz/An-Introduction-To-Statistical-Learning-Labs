library(ISLR)

# It's a good idea to set the seed for resampling so the randomized aspects
# can later be reproduced
set.seed(1)

train <- sample(392, 196)

# We can fit models using half of our training data and use the other half
# to compute our test error
lm.fit <- lm(mpg ~ horsepower, data=Auto, subset = train)
summary(lm.fit)
mean((Auto$mpg - predict(lm.fit, Auto))[-train]^2)

# We can do the same for higher degree models
lm.quad <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
lm.cube <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)

mean((Auto$mpg - predict(lm.quad, Auto))[-train]^2)
mean((Auto$mpg - predict(lm.cube, Auto))[-train]^2)

