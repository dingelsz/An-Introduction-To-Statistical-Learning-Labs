library(ISLR)

names(Carseats)

# R will create dummy variables for qualitative predictors
lm.fit <- lm(Sales ~ .+ Income:Advertising + Price:Age, data=Carseats)
summary(lm.fit)

# Contrasts shows how the dummy variables are made
contrasts(Carseats$ShelveLoc)
