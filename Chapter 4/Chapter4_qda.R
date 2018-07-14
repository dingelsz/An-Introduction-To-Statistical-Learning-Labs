library(MASS)
library(ISLR)

train <- (Smarket$Year < 2005)
Smarket.2005 = Smarket[!train,]
Direction.2005 <- Smarket$Direction[!train]

qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit

qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005) / length(Direction.2005)
# 60% is not bad
mean(qda.class == Direction.2005)
