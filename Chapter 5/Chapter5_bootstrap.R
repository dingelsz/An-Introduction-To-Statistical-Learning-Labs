library(ISLR)
library(boot)

set.seed(1)

# Our model
alpha.fn <- function(data, index) {
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y)-cov(X,Y)) / (var(X)+var(Y) - 2*cov(X,Y)))
}

alpha.fn(Portfolio)

# Run a single resample
alpha.fn(Portfolio, sample(100, 100, replace=T))

# Run a bootstrap
boot(Portfolio, alpha.fn, R=1000)

# Let's run a bootstrap for the Auto data set
boot.fn <- function(data, index) {
  return(coef(lm(mpg ~ horsepower, data=Auto, subset = index)))
}
boot.fn(1:392)

set.seed(1)
boot.fn(Auto, sample(392, 392, replace=T))

boot(Auto, boot.fn, R=1000)

# Let's compare the bootstrap error estimations to the linear regression:
summary(lm(mpg ~ horsepower, data=Auto))
# These two differ because the linear model estimations for the std. error
# is based on the assumption that our model is correct. It also assumes that 
# the observations are fixed. Since the actual data has a quadratic relationship
# lets look at the quadratic model
boot.fn <- function(data, index) {
  return(coef(lm(mpg ~ horsepower+I(horsepower^2), data=Auto, subset = index)))
}
set.seed(1)
boot(Auto, boot.fn, R=1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef
