college <- read.csv("../Resources/College.csv")

View(college)

# Set the name of each row to the name of the college and remove the
# column holding university names
rownames(college) <- college[,1]
college <- college[,-1]

# Review the data
summary(college)
pairs(college[,1:5])

# Compare the cost of out of state college tution distribution
# for private and public schools
plot(college[,9] ~ college[,1], ylab = "Tuition", xlab = "Private School?")

# Create a new variable "Elite", universities with over 50% of students coming
# from top universities. 
Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)

# Review the data
View(college)
summary(college$Elite)

# Exploring the data with histograms
par(mar=c(1,1,1,1))
par(mfrow=c(2,2))

hist(college$Accept)
hist(college$Enroll)
hist(college$Room.Board)
hist(college$Books)

# Summary of the data:
# I'm interested in the difference between schools with many top 10%
# highschool admits vs school with few top 25% high school admits

Non_Elite <- rep("No", nrow(college))
Non_Elite[college$Top25perc < 30] <- "Yes"
Non_Elite <- factor(Non_Elite)
college <- data.frame(college, Non_Elite)

# Let's compare room and board distributions for both groups:
par(mar=c(2,2,2,2))
par(mfrow=c(1,2))

boxplot(college$Room.Board[college$Elite == "Yes"])
boxplot(college$Room.Board[college$Non_Elite == "Yes"])

mean(college$Room.Board[college$Elite == "Yes"]) - mean(college$Room.Board[college$Non_Elite == "Yes"])
# It looks like on average the cost of attending a school made up of
# students from top highschools costs $1311.795 more. 