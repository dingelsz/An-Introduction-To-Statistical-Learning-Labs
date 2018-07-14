library(MASS)
library(ISLR)

train <- (Smarket$Year < 2005)
Smarket.2005 = Smarket[!train,]
Direction.2005 <- Smarket$Direction[!train]

# Let's create a LDA Model
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)

lda.fit
plot(lda.fit)

lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class <- lda.pred$class
table(lda.class, Direction.2005) / length(Smarket.2005[,1])

max(lda.pred$posterior[,1])
