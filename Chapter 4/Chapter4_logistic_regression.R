library(ISLR)

head(Smarket) # S%P500 percentage returns. Lag1 means yesterday... 
summary(Smarket)
cor(Smarket[,-9])

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume ,data = Smarket, family = binomial)

summary(glm.fit)
contrasts(Direction)

# Let's make predictions based on our model
glm.probs <- predict(glm.fit, type="response")
glm.probs[1:10]
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > 0.5] <- "Up"

# We can create a confusion table to observe false positives and true negatives
table(glm.pred, Smarket$Direction) / 1250

# It appears that our model predicts the market 52% of the time but this is only
# For training data. We can split up our data to get a better sense of it's 
# accuracy
train <- (Smarket$Year < 2005)
Smarket.2005 = Smarket[!train,]

dim(Smarket.2005)
Direction.2005 <- Smarket$Direction[!train]

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume ,data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fit, Smarket.2005, type="response")

glm.pred <- rep("Down", 252)
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction.2005) / 252
