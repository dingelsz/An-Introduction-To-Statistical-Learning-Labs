library(MASS)

dim(Boston)
names(Boston)
View(Boston)
?Boston

pairs(Boston)

par(mfrow=c(1,1))

# One observation is that the closer a town is to employment centers 
# The more crime there is
plot(Boston$crim ~ Boston$dis)

# Another observation is that the value of a home seems to have a linear
# Relationship with the number of rooms
plot(Boston$medv ~ Boston$rm)

# An interseting relationship is between nox levels and distance to 
# Employment centers. The closer a town is to an employment center the higher 
# the nox levels. 
plot(Boston$nox ~ Boston$dis)

# Crime 
# It looks like distance to employment centers and the age of a town
# Contribute to its crime rate. 
plot(Boston$crim ~ Boston$dis)
boxplot(Boston$crim)

# It looks like the higher crime rate towns are all old towns whoses property
# on average isn't very high.
range(Boston$crim)
View(Boston[Boston$crim > 40,])


# Towns with high tax have lower crime rates. 
range(Boston$tax)
mean(Boston$tax)
View(Boston[Boston$tax > 700,])
mean(Boston$crim)

# Towns with high pupil to teacher ratios are far from employment centers.
# They tend to have lower taxs.
range(Boston$ptratio)
mean(Boston$ptratio)
sort(unique(Boston$ptratio))
View(Boston[Boston$ptratio == 22,])
mean(Boston$dis)
mean(Boston$tax)

# 506 Towns border the Charles River
length(Boston$chas == 1)

# Analysis of the lowest valued town. The first thing that pops out is the
# high crime rate. The town is also one of the oldest. The town also has a 
# high percent of lower status population. 
View(Boston[order(Boston$medv),])
mean(Boston$crim)
mean(Boston$lstat)

# There are 51 towns with an average of 7-8 bedrooms
dim(Boston[Boston$rm > 7 & Boston$rm < 8,])
# There are 13 towns with an average of 8 or more bedrooms. Crime rates are 
# low in these towns, nox levels tend to be lower, the average value
# of homes is high and the student to teacher ratio is lower. 
dim(Boston[Boston$rm > 8,])
View(Boston[Boston$rm > 8,])
