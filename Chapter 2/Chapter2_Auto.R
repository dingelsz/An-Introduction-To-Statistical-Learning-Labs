auto <- read.csv("../Resources/Auto.csv")

names(auto)
View(auto)

# Looks like there are missing values in the horsepower column so lets
# remove those

auto <- auto[auto$horsepower != "?", ]

# What is the range of each quantitative predictor?
range(auto[,1]) 
range(auto[,3])
range(auto[,5])
range(auto[,6])

mean(auto[,1]) 
mean(auto[,3])
mean(auto[,5])
mean(auto[,6])

sd(auto[,1]) 
sd(auto[,3])
sd(auto[,5])
sd(auto[,6])

# What are the means and standard deviation of the data without the 10-85th 
# observations?

range <- c(1:9, 86:length(auto$mpg))
auto_subset <- auto[range,]

# MPG
range(auto_subset[,1]) 
mean(auto_subset[,1]) 
sd(auto_subset[,1]) 
# Displacement
range(auto_subset[,3]) 
mean(auto_subset[,3]) 
sd(auto_subset[,3]) 
# Weight
range(auto_subset[,5]) 
mean(auto_subset[,5]) 
sd(auto_subset[,5]) 
# Acceleration
range(auto_subset[,6]) 
mean(auto_subset[,6]) 
sd(auto_subset[,6]) 

# Lets look at relationships between the predictors
pairs(auto)
# There are a lot of relationships. The first one that sticks out to me
# Is the mpg ~ year relationship. Cars are becoming more fuel effecient. 
# As expected, car name looks random compared to each predictor. Horsepower ~
# Acceleration is another interesting graph, there are two different clusters. 
# Another observation is that on average, 8 cylinder cars have the worst 
# acceleration while 6 cylinder and 4 are comparable. 