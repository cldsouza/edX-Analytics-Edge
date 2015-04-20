#Read data
census = read.csv("census.csv")

###############################################################################
#                 BUILD A LOGISTIC MODEL
###############################################################################

set.seed(2000)
library(caTools)
split = sample.split(census$over50k, SplitRatio = 0.6)

# Split up the data using subset
train = subset(census, split==TRUE)
test = subset(census, split==FALSE)

#Build a logistic regression model for predicting Earnings
censusglm = glm(over50k ~ . , data = train, family = binomial)
summary (censusglm)

#What is the accuracy of the model with threshold 0.5

# Predictions on the test set
predictTest = predict(censusglm, type="response", newdata=test)

# Confusion matrix with threshold of 0.5
table(test$over50k, predictTest > 0.5)

# Accuracy of the model
(1888 + 9051) / (1888 + 9051 + 662 + 1190 )
# 0.855

#Baseline Acc
table(census$over50k)
24283/ (24283 + 7695)

#Area under curve auc
library(ROCR)

#Prediction function
ROCRpred = prediction(predictTest, test$over50k)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

###############################################################################
#                 BUILD A CLASSIFICATION TREE 
###############################################################################

library(rpart)
library(rpart.plot)

CARTover50k = rpart(over50k ~ . , data=train, method="class")

# Make predictions
Predict.over50k = predict(CARTover50k, newdata = test, type = "class")

#Plot tree
prp(CARTover50k)

#Predict Accuracy
table(test$over50k, Predict.over50k)

(9243 + 1596) / (9243 + 1596 + 470 + 1482)
# 0.847

#This highlights a very regular phenomenon when comparing CART and logistic 
#regression. CART often performs a little worse than logistic regression in 
#out-of-sample accuracy. However, as is the case here, the CART model is often
#much simpler to describe and understand. 


# Performance of the model
PredictROC = predict(CARTover50k, newdata = test)
pred = prediction(PredictROC[,2], test$over50k)
perf = performance(pred, "tpr", "fpr")  #tpr - true positive rate. 

# plotting the ROC curve 
plot(perf,col="black",lty=3, lwd=3) # CART model ROC curve 

# AUC
auc = as.numeric(performance(pred, "auc")@y.values)
auc


###############################################################################
#                 BUILD A RANDOM FOREST 
###############################################################################

# downsize the training set as this model might run out of memory

set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

library(randomForest)
set.seed(1)
Forest = randomForest(over50k ~ . , data = trainSmall )

# Make predictions using threshold 0.5. you dont need a type arg when 
# threshold is 0.5
PredictForest = predict(Forest, newdata = test) 
table(test$over50k, PredictForest)

#Accuracy
(9586 + 1093) / (9586 + 1093 + 127 + 1985)
# 0.834

# compute metrics that give us insight into which variables are important
# This code produces a chart that for each variable measures the number of 
# times that variable was selected for splitting

vu = varUsed(Forest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(Forest$forest$xlevels[vusorted$ix]))


varImpPlot(Forest)

#############################################################################
#Selecting cp by Cross Validation
#############################################################################


#load cross validation packages
library(caret)
library(e1071)

set.seed(2)
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.002,0.1,0.002)) 

# Perform the cross validation
train(over50k ~ . , data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

#you will get the cp value as 0.002

# Create a new CART model
CART2 = rpart(over50k ~ . , data=train, method="class", cp = 0.002)

# Make predictions
Predict2 = predict(CART2, newdata = test, type = "class")

#Predict Accuracy
table(test$over50k, Predict2)

(9178 + 1838) / (9178 + 1838 + 535 + 1240)
# 0.861

prp(CART2)


# testing Random forest (Here Random forest has less accuracy than CART ???)
Forest = randomForest(over50k ~ . , data = train )
PredictForest = predict(Forest, newdata = test) 
table(test$over50k, PredictForest)

(9689 + 880)/(9689 + 880 + 24 + 2198)
# 0.826

