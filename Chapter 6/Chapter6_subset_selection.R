library(ISLR)

names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters <- na.omit(Hitters)
dim(Hitters)

library(leaps)
regfit.full <- regsubsets(Salary ~ ., Hitters)
reg.summary <- summary(regfit.full)
# An asterik means that the predictor was included in the model
reg.summary$adjr2
names(reg.summary)

# We can add all of the variables into the model with the nvmax param:
regfit.full <- regsubsets(Salary ~ ., Hitters, nvmax = 19)
reg.summary <- summary(regfit.full)

# We can plot the statistics to see which model is the best
par(mfrow = c(2,2))
par(mar = c(5,5,5,5))

# RSS
plot(reg.summary$rss, xlab = "Number of variables", ylab = "Residual Sum Squared",type = "l")

# Adjusted R^2
plot(reg.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type="l")
maxInd <- which.max(reg.summary$adjr2)
points(maxInd, reg.summary$adjr2[maxInd], col='red', pch = 20, cex=2)

# Cp
plot(reg.summary$cp, xlab = "Number of variables", ylab = "Cp", type="l")
minInd <- which.min(reg.summary$cp)
points(minInd, reg.summary$cp[minInd], pch = 20, cex = 2, col = 'red')

# BIC
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type="l")
minInd <- which.min(reg.summary$bic)
points(minInd, reg.summary$bic[minInd], pch = 20, cex = 2, col='red')

# We can also use the built in plot function to evaluate the 
# models. The black tiles represent variables that are included
# in the model.
par(mfrow=c(1,1))
plot(regfit.full, scale = "Cp")

# Once we find a model we like we can look at it with coef
coef(regfit.full, 10)

# The regsubsets function can also be used for forward and
# backwards selection with the "method" parameter. 
# it takes "forward" and "backward"
?regsubsets
regfit.fwd <- regsubsets(Salary ~ ., Hitters, method="forward")
summary(regfit.fwd)
plot(regfit.fwd, scale="bic")

regfit.bkwd <- regsubsets(Salary ~ ., Hitters, method="backward")
summary(regfit.bkwd)
plot(regfit.bkwd, scale="r2")

# We will now add validation to our approach
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)
test <- !train

regfit.best <- regsubsets(Salary ~ ., Hitters[train,], nvmax = 19)

test.mat <- model.matrix(Salary ~ ., data=Hitters[test, ])

val.err <- rep(NA, 19)
for (i in 1:19) {
  coefi <- coef(regfit.best, id=i)
  pred <- test.mat[,names(coefi)] %*% coefi
  val.err[i] <- mean((Hitters$Salary[test]-pred)^2)
}
val.err
which.min(val.err)
coef(regfit.best, 10)

# Let's put the prediction code in a function so we 
# can resuse it.
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[,xvars] %*% coefi
}

# To finish our model we will go back and create a model using all
# of the data available
regfit.best <- regsubsets(Salary ~ ., Hitters, nvmax = 19)
coef(regfit.best, 10)

# We can go even further and use cross validation
k = 10 # Number of folds
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace=TRUE)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for (j in 1:k) {
  best.fit <- regsubsets(Salary ~ ., data=Hitters[folds != j,], nvmax = 19)
  for (i in 1:19) {
    pred <- predict(best.fit, Hitters[folds == j, ], id=i)
    cv.errors[j,i] = mean( (Hitters$Salary[folds==j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
which.min(mean.cv.errors)
# We have found that the model with 11 predictors performs best
# under cross validation so lets build a model with 11
# predictors using all of our data
regfit.best <- regsubsets(Salary ~ ., data=Hitters, nvmax = 19)
coef(regfit.best, 11)
