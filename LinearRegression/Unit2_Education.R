pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

#Using tapply() on pisaTrain, what is the average reading test score of males?
tapply(pisaTrain$readingScore, pisaTrain$male, mean, na.rm=TRUE)

 #Type the following commands into your R console 
 #to remove observations with any missing value from pisaTrain and pisaTest:
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)


#Because the race variable takes on text values, it was loaded as a factor 
#variable when we read in the dataset with read.csv() -- you can see this when 
#you run str(pisaTrain) or str(pisaTest). However, by default R selects the 
#first level alphabetically ("American Indian/Alaska Native") as the reference 
#level of our factor instead of the most common level ("White"). Set the 
#reference level of the factor by typing the following two lines in your R 
#console
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

# build a linear regression model (call it lmScore) using the training set to predict 
# readingScore using all the remaining variables
lmScore = lm(readingScore ~. , data = pisaTrain)

# What is the training-set root-mean squared error (RMSE) of lmScore?
SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE/nrow(pisaTrain))


#Predict 

predTest = predict(lmScore, newdata = pisaTest)

# What is the test-set root-mean squared error (RMSE)
SSE = sum((predTest - pisaTest$readingScore)^2)

RMSE = sqrt(SSE/nrow(pisaTest))

SST = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)

R2 = 1 - SSE/SST


# What is the predicted test score used in the baseline model? 
# Remember to compute this value using the training set and not the test set.

baseline = mean(pisaTrain$readingScore)
	

