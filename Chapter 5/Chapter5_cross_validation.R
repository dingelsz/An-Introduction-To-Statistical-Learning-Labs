library(ISLR)
library(boot)

# Below we will do a leave one out cross validation (LOOCV)
glm.fit <- glm(mpg ~ horsepower, data=Auto)
cv.err <- cv.glm(Auto, glm.fit)
summary(cv.err)
# The delta gives our test error using cross validation. The second 
# number in delta is the bias corrected delta. 
cv.err$delta

# Let's run cross validation for different regression models:
cv.err <- rep(0, 5)
for (i in 1:5) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data=Auto)
  cv.err[i] <- cv.glm(Auto, glm.fit)$delta[1]
}

# We can also set the number of folds in the cv:
set.seed(17)
cv.err.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data=Auto)
  cv.err.10[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}