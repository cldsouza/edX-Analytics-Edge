### Predicting Stock Returns with Cluster-Then-Predict

#   ReturnJan = the return for the company's stock during January (in the year of the observation). 
#   ReturnFeb = the return for the company's stock during February (in the year of the observation). 
#   ReturnMar = the return for the company's stock during March (in the year of the observation). 
#   ReturnApr = the return for the company's stock during April (in the year of the observation). 
#   ReturnMay = the return for the company's stock during May (in the year of the observation). 
#   ReturnJune = the return for the company's stock during June (in the year of the observation). 
#   ReturnJuly = the return for the company's stock during July (in the year of the observation). 
#   ReturnAug = the return for the company's stock during August (in the year of the observation). 
#   ReturnSep = the return for the company's stock during September (in the year of the observation). 
#   ReturnOct = the return for the company's stock during October (in the year of the observation). 
#   ReturnNov = the return for the company's stock during November (in the year of the observation). 
#   PositiveDec = whether or not the company's stock had a positive return in December (in the year of the observation). This variable takes value 1 if the return was positive, and value 0 if the return was not positive.


#Load StocksCluster.csv into a data frame called "stocks"
stocks = read.csv("StocksCluster.csv")

# What is the maximum correlation between any two return variables in the dataset? 
cor(stocks)


max(summary(stocks$mean))


#split the data into a training set and testing set, putting 70% of the data
#in the training set and 30% of the data in the testing set:

library(caTools)

set.seed(144)

spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

###################################################################################
# Logistic regression model
###################################################################################

stocksLog = glm(PositiveDec ~ ., data = stocksTrain, family=binomial)
summary(stocksLog)


# Predictions on the training set
predictTrain = predict(stocksLog, type="response")

# Confusion matrix with threshold of 0.5
table(stocksTrain$PositiveDec, predictTrain > 0.5)

#Acc
(990 + 3640)/ (990 + 3640 + 2689 + 787)
# 0.56


# Predictions on the test set
predictTest = predict(stocksLog, type="response", newdata = stocksTest)

# Confusion matrix with threshold of 0.5
table(stocksTest$PositiveDec, predictTest > 0.5)

#Acc
(417 + 1553) /(457 + 1553 + 1160 + 344)
#0.56

#Acc of baseline
table(stocksTest$PositiveDec)
1897 / (1577 + 1897)
#0.54

###################################################################################
# K-Means Clustering
###################################################################################

#The first step in this process is to remove the dependent variable using 
#the following commands:

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL

limitedTest = stocksTest
limitedTest$PositiveDec = NULL

#In cluster-then-predict, our final goal is to predict the dependent variable, 
#which is unknown to us at the time of prediction. Therefore, if we need to know 
#the outcome value to perform the clustering, the methodology is no longer useful 
#for prediction of an unknown outcome value.

#This is an important point that is sometimes mistakenly overlooked. 
#If you use the outcome value to cluster, you might conclude your method strongly
#outperforms a non-clustering alternative. However, this is because it is using 
#the outcome to determine the clusters, which is not valid. 

# NORMALIZE
#In cases where we have a training and testing set, we'll want to normalize by 
#the mean and standard deviation of the variables in the training set. We can do this by passing just the training set to the preProcess function:

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

## K-MEANS Clustering


#run the k-means clustering algorithm on the normalized data, creating 
#3 clusters

# Specify number of clusters
k = 3

set.seed(144)
km = kmeans(normTrain, centers = k , iter.max = 1000)
str(km)

#check size of each cluster
table(km$cluster)

#Recall from the recitation that we can use the flexclust package to obtain 
#training set and testing set cluster assignments for our observations

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

#How many test-set observations were assigned to Cluster 2?
table(clusterTest)

##################################################################################
#Cluster Specific Predictions
##################################################################################

#Using the subset function, build data frames stocksTrain1, stocksTrain2, and 
#stocksTrain3, containing the elements in the stocksTrain data frame assigned 
#to clusters 1, 2, and 3, respectively (be careful to take subsets of stocksTrain,
#not of normTrain). Similarly build stocksTest1, stocksTest2, and stocksTest3 from
#the stocksTest data frame.

stocksTrain1 = subset(stocksTrain, clusterTrain==1)
stocksTrain2 = subset(stocksTrain, clusterTrain==2)
stocksTrain3 = subset(stocksTrain, clusterTrain==3)


stocksTest1 = subset(stocksTest, clusterTest==1)
stocksTest2 = subset(stocksTest, clusterTest==2)
stocksTest3 = subset(stocksTest, clusterTest==3)


###################################################################################
#Build Logistic Regression Models


StocksModel1 =  glm(PositiveDec ~ ., data = stocksTrain1, family=binomial)
StocksModel2 =  glm(PositiveDec ~ ., data = stocksTrain2, family=binomial)
StocksModel3 =  glm(PositiveDec ~ ., data = stocksTrain3, family=binomial)

# Predictions on the test set
predictTest1 = predict(StocksModel1, type="response", newdata = stocksTest1)
predictTest2 = predict(StocksModel2, type="response", newdata = stocksTest2)
predictTest3 = predict(StocksModel3, type="response", newdata = stocksTest3)

# Confusion matrix with threshold of 0.5
table(stocksTest1$PositiveDec, predictTest1 > 0.5)
(30 + 774) / (30 + 471 + 23 + 774)
#0.6194145

table(stocksTest2$PositiveDec, predictTest2 > 0.5)
(388 + 757) / (388 + 626 + 309 + 757)
#0.5504808

table(stocksTest3$PositiveDec, predictTest3 > 0.5)
(49 + 13) / (49 + 13 + 21 + 13)
#0.6458333

#To compute the overall test-set accuracy of the cluster-then-predict approach, 
#we can combine all the test-set predictions into a single vector and all the 
#true outcomes into a single vector:

AllPredictions = c(predictTest1, predictTest2, predictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes, AllPredictions > 0.5) 
(467 + 1544) / (467 + 1110 + 353 + 1544)



