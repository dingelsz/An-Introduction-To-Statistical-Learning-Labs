library(ISLR)
library(class)

summary(Caravan)
summary(Caravan$Purchase)

# Use the scale function to standardize the data. This way when we 
# Compute the distance for knn we wont weight larger values more than 
# smaller ones (age in years vs age in seconds)

standardize.X <- scale(Caravan[,-86])
test <- 1:1000

train.X <- standardize.X[-test,]
test.X <- standardize.X[test,]
train.Y <- Caravan$Purchase[-test]
test.Y <- Caravan$Purchase[test]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)
mean(test.Y != "No")
