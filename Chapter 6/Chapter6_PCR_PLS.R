library(pls)
library(ISLR)

Hitters <- na.omit(Hitters)

set.seed(2)
# We set scale to TRUE so our variables are standardized
# and we can also use the built in cross validation
pcr.fit <- pcr(Salary ~ ., data=Hitters, scale=TRUE, validation="CV")
summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- Hitters$Salary[test]

pcr.fit <- pcr(Salary ~ ., data=Hitters, subset = train, scale=TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

# It looks like 7 componenents gives us the lowest MSE so lets 
# compute a test MSE
pcr.pred <- predict(pcr.fit, x[test, ], ncomp = 7)
mean((pcr.pred - y.test)^2)

# Finally lets make a model with 7 componenets using all of our data
pcr.fit <- pcr(Salary ~ ., scale=TRUE, ncomp=7, data=Hitters)
summary(pcr.fit)

# Lets run partial least squares
set.seed(1)
pls.fit <- plsr(Salary ~ ., data=Hitters, scale=TRUE, subset=train, validation="CV")
summary(pls.fit)
validationplot(pls.fit)
# It looks like we can use fewer components
pls.pred <- predict(pls.fit, x[test,], ncomp=2)
mean((pls.pred - y.test)^2)
# We get a higher test MSE but it's comparable.
# Lets build a full model
pls.fit <- plsr(Salary ~ ., data=Hitters, scale=TRUE, ncomp=2)
summary(pls.fit)
# PLS has fewer components because it is supervised. 