library(glmnet)
library(ISLR)

Hitters <- na.omit(Hitters)

x <- model.matrix(Salary ~ ., Hitters)[,-1]
y <- Hitters$Salary

# Grid will be our tunning params
grid <- 10^seq(10, -2, length = 100)
# glmnet automatically standardizes variables
ridge.mod <- glmnet(x, y, alpha=0, lambda=grid)

# Lets use validation to see which model works best
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

ridge.mod <- glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s=4, newx = x[test,])
mean((ridge.pred - y.test)^2)

ridge.pred <- predict(ridge.mod, s=10e10, newx = x[test,])
mean((ridge.pred - y.test)^2)

# We can test if our ridge regression is better than a linear
# least squares model by setting lambda to 0:
ridge.lls.pred <- predict(ridge.mod, s=0, newx = x[test,])
mean((ridge.lls.pred - y.test)^2)
# But in general we should use lm() because it provides
# additional information like r^2 and p values

# Let's do some cross validation
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

ridge.best.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.best.pred - y.test)^2)

# let's rebuild our model using the full data
out <- glmnet(x, y, alpha=0)
predict(out, type="coefficients", s = bestlam)
# Notice that none of the coefficients are 0, that is the ridge
# regression did not select variables. Let's run a lasso regression

lasso.mod <- glmnet(x, y, alpha=1, lambda = grid)
plot(lasso.mod)

# Lets run cross validation to select our model
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred - y.test)^2)

# We can see that the lasso regression selected some of our variables
out <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef <- predict(out, type="coefficients", s=bestlam)
lasso.coef
