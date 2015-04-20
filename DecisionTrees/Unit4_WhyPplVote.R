#load csv file
gerber = read.csv("gerber.csv")

#proportion of people that voted
summary(gerber)
table(gerber$voting)

#largest percentage of ppl that voted among treatment groups
table(gerber$hawthorne, gerber$voting)
table(gerber$civicduty, gerber$voting)
table(gerber$neighbors, gerber$voting)
table(gerber$self, gerber$voting)

#you can use this method too
tapply( gerber$voting, gerber$hawthorne,mean)

#Build a logistic regression model for voting
gerberLog = glm(voting ~ hawthorne + civicduty + neighbors + self , data = gerber, family = binomial)
summary (gerberLog)

#What is the accuracy of the model with threshold 0.3

# Predictions on the test set
predictTest = predict(gerberLog, type="response")

# Confusion matrix with threshold of 0.3
table(gerber$voting, predictTest > 0.3)

# Accuracy of the model
(134513 + 51966) / ( 134513 + 51966 + 100875 + 56730)

# Confusion matrix with threshold of 0.5
table(gerber$voting, predictTest > 0.5)

library(ROCR)
ROCRpred = prediction(predictTest, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)

#We will now try out trees
#Build a regression tree
#We are interested in building a tree to explore the fraction of people 
#who vote, or the probability of voting. We’d like CART to split our groups
#if they have different probabilities of voting. If we used method=‘class’, 
#CART would only split if one of the groups had a probability of voting above 
#50% and the other had a probability of voting less than 50% (since the predicted 
#outcomes would be different). However, with regression trees, CART will split
#even if both groups have probability less than 50%. 

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

#Build entire tree by assigning cp to 0
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)

prp(CARTmodel4, digits=6)
abs(0.296638 - 0.34)

# create logistic regression model
fitLog <- glm(voting ~ control + sex, data=gerber, family='binomial')
summary(fitLog)


LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")


