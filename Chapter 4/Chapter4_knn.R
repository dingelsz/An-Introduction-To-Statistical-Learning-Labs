library(class)
library(ISLR)

train <- (Smarket$Year < 2005)
Direction.2005 <- Smarket$Direction[!train]

train.X <- cbind(Smarket$Lag1, Smarket$Lag2)[train,]
test.X <- cbind(Smarket$Lag1, Smarket$Lag2)[!train,]
train.Direction <- Smarket$Direction[train]

set.seed(1)

# Let's try using knn with the closest neighbor
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005) / length(Direction.2005)
mean(knn.pred == Direction.2005)
# Lets increase the number of neighbors
knn.pred <- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005) / length(Direction.2005)
mean(knn.pred == Direction.2005)
