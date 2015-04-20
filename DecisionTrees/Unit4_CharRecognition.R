
#Read in data
letters = read.csv("letters_ABPR.csv")

#create a new variable isB in the dataframe, which takes the value "TRUE" 
#if the observation corresponds to the letter B,

letters$isB = as.factor(letters$letter == "B")

#Predicting B or Not B
library(caTools)
set.seed(1000)
split = sample.split(letters$isB, SplitRatio = 0.5)

# Split up the data using subset
train = subset(letters, split==TRUE)
test = subset(letters, split==FALSE)

# to get baseline method
table(letters$isB)
2350/ (2350 + 766)
# 0.754

#build a classification tree to predict whether a letter is a B or not
#Remember to remove the variable "letter" out of the model, as this is 
#related to what we are trying to predict

CARTb = rpart(isB ~ . - letter, data=train, method="class")

# Make predictions
Predictb = predict(CARTb, newdata = test, type = "class")
table(test$isB, Predictb)

#of course the Accuracy
(1118 + 340)/(1118 + 340 + 57 + 43)
# 0.935 


#build a random forest model
#no need to add method = "class" explicitly but independent var should be a factor 
#for it to work as classification rather than regression

library(randomForest)
CARTbForest = randomForest(isB ~ . -letter , data = train )

# Make predictions
PredictbForest = predict(CARTbForest, newdata = test)
table(test$isB, PredictbForest)

#Accuracy
(1164 + 373) / (1164 + 373 + 11 + 10)
# 0.986

###########################################################################
#Predicting the letters A, B, P, R using CART
###########################################################################

#predict whether or not a letter is one of the four letters A, B, P or R

letters$letter = as.factor( letters$letter )

set.seed(2000)
split = sample.split(letters$letter, SplitRatio = 0.5)

# Split up the data using subset
train.abpr = subset(letters, split==TRUE)
test.abpr = subset(letters, split==FALSE)

# to get baseline method
table(letters$letter)

#In a multiclass classification problem, a simple baseline model is to predict
#the most frequent class of all of the options

#Baseline model Accuracy thus is
803/ (789 + 766 + 803 + 758)

#Now build a classification tree to predict "letter" ABPR

CART.abpr = rpart(letter ~ . - isB, data=train.abpr, method="class")

# Make predictions
Predict.abpr = predict(CART.abpr, newdata = test.abpr, type = "class")
table(test.abpr$letter, Predict.abpr)

#Accuracy using the confusion matrix
(348 + 318 + 363 + 340) / nrow(test.abpr)
# 0.8786

#############################################################################
#             RANDOM FOREST MODEL to predict A B P R
#############################################################################

#Now build a random forest model on the training data, using the same 
#independent variables as in the previous problem

library(randomForest)
set.seed(1000)
Forest.abpr = randomForest(letter ~ . -isB , data = train.abpr )

# Make predictions
Predict.Forest.abpr = predict(Forest.abpr, newdata = test.abpr)
table(test.abpr$letter, Predict.Forest.abpr)

#Accuracy using the confusion matrix
(390 + 380 + 393 + 364) / nrow(test.abpr)
# 0.980









