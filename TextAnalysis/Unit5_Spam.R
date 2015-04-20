#Load the dateset
trials = read.csv("emails.csv", stringsAsFactors=FALSE)
str(trials)

#How many characters are in the longest email in the dataset 
#where longest is measured in terms of the maximum number of characters)?

max(nchar(trials$text))

which.min(nchar(trials$text))


library(tm)
library(SnowballC)

# Create corpus for Email Text
corpus = Corpus(VectorSource(trials$text))
corpus[[1]]

# Convert to lower-case
corpus = tm_map(corpus, tolower)

#Convert to plain text doc
corpus = tm_map(corpus, PlainTextDocument)

# Remove punctuation
corpus = tm_map(corpus, removePunctuation)

# Remove stopwords 
corpus = tm_map(corpus, removeWords, stopwords("english"))

# Stem document 
corpus = tm_map(corpus, stemDocument)

# Create matrix
dtm = DocumentTermMatrix(corpus)

# Remove sparse terms
spdtm = removeSparseTerms(dtm, 0.95)

# Convert to a data frame

emailsSparse = as.data.frame(as.matrix(spdtm))

colnames(emailsSparse) = make.names(colnames(emailsSparse), unique = TRUE)

#What is the word stem that shows up most frequently across all the emails 
#in the dataset?

which.max(colSums(emailsSparse))
sort(colSums(emailsSparse))

#Copy dependant var
emailsSparse$spam = trials$spam

#How many word stems appear at least 5000 times in the ham emails in the dataset? 
sort(colSums(subset(emailsSparse, spam == 0)))

#Convert dependent variable to a factor
emailsSparse$spam = as.factor(emailsSparse$spam)

# Split the data

library(caTools)

set.seed(123)

split = sample.split(emailsSparse$spam, SplitRatio = 0.7)

train = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse, split==FALSE)

##############################################################################
# Logistic Regression Model
##############################################################################

emailsLog = glm(spam ~ ., data = train, family=binomial)
summary(emailsLog)

# Predictions on the training set
predictTrainLog = predict(emailsLog, type="response", newdata=train)

# Confusion matrix with threshold of 0.5
table(train$spam, predictTrainLog > 0.5)

#Training set accuracy
(3052 + 954) / (3052 + 4 + 954 + 0)
#0.9990025


# Training set AUC 
library(ROCR)
ROCRpred = prediction(predictTrainLog, train$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)
#0.9999959


# Compute TRAINING set accuracy

#How many of the training set predicted probabilities 
#from spamLog are less than 0.00001? / greater than 0.99999
table(train$spam, predictTrainLog < 0.00001)
table(train$spam, predictTrainLog >= 0.99999)


#Predictions on Test Set
predictTestLog = predict(emailsLog, type="response", newdata=test)

# Confusion matrix with threshold of 0.5
table(test$spam, predictTestLog > 0.5)

#Accuracy of Test Set
(1257 + 376)/ (1257 + 376 + 51 + 34)
# 0.9505239


# Testing set AUC 
library(ROCR)
ROCRpred.test = prediction(predictTestLog, test$spam)
as.numeric(performance(ROCRpred.test, "auc")@y.values)
#0.9627517







##############################################################################
# CART model
##############################################################################

library(rpart)
library(rpart.plot)

spamCART = rpart(spam ~ ., data=train, method="class")
predTrainCART = predict(spamCART)[,2]
prp(spamCART )

table(train$spam, predTrainCART > 0.5)
(2885 + 894) /(2885 + 894 + 167 + 64)
#0.942394

# Performance of the model
PredictCART = predict(spamCART, newdata = train)
pred = prediction(PredictCART[,2], train$spam)
perf = performance(pred, "tpr", "fpr")  #tpr - true positive rate. 

# AUC
auc = as.numeric(performance(pred, "auc")@y.values)
auc
#0.9696044

##############################################################################
# Testing Set predictions

PredictCARTtest = predict(spamCART, newdata = test, type = "class")

#Accuracy
table(test$spam, PredictCARTtest)
(1228 + 386) / (1228 + 386 + 80 + 24)

##############################################################################

## OR

PredictCARTtest1 = predict(spamCART, newdata = test) ### removed type = "class" 

#Accuracy
table(test$spam, PredictCARTtest1[,2] > 0.5)
(1228 + 386) / (1228 + 386 + 80 + 24)

       

pred.CART.test = prediction(PredictCARTtest1[,2], test$spam)

# AUC
aucTest = as.numeric(performance(pred.CART.test, "auc")@y.values)
aucTest

##############################################################################
# Random Forests
##############################################################################

library(randomForest)
set.seed(123)

spamRF = randomForest(spam~., data=train) 

#(Remember to pass the argument type="prob" to the predict function to get
# predicted probabilities for a random forest model.
# The probabilities will be the second column of the output.)

predTrainRF = predict(spamRF, type="prob")[,2] 
table(train$spam, predTrainRF > 0.5)

#Accuracy 
(3013 + 914) /(3013 + 914 + 39 + 44)
#0.9793017

# Performance of the model
pred = prediction(predTrainRF, train$spam)
perf = performance(pred, "tpr", "fpr")  #tpr - true positive rate. 

# AUC
auc = as.numeric(performance(pred, "auc")@y.values)
auc
#0.9979116

##############################################################################
# Testing set accuracy

# Make predictions using threshold 0.5. you dont need a type arg when 
# threshold is 0.5
PredictTestRF = predict(spamRF, newdata = test) 
table(test$spam, PredictTestRF)

#Accuracy
(1290 + 386 )/ (1290 + 18 + 24 + 386)


#(Remember to pass the argument type="prob" to the predict function to get
# predicted probabilities for a random forest model.
# The probabilities will be the second column of the output.)

PredictTestRF1 = predict(spamRF, newdata = test, type="prob")[,2] 
table(test$spam, PredictTestRF1 > 0.5)

pred.test.RF = prediction(PredictTestRF1, test$spam) # with Probabilities
# AUC
auc = as.numeric(performance(pred.test.RF, "auc")@y.values)
auc




#Obtain the word counts for each email with the command:
#wordCount = rowSums(as.matrix(dtm))

#or the following lines if there is a memory issue

library(slam)
wordCount = rollup(dtm, 2, FUN=sum)$v 

hist(wordCount)
hist(log(wordCount))

#Create a variable called logWordCount in emailsSparse 
#that is equal to log(wordCount)

emailsSparse$logWordCount = log(wordCount)

tapply( emailsSparse$logWordCount, emailsSparse$spam, mean)

boxplot(emailsSparse$logWordCount~emailsSparse$spam)


train2 = subset(emailsSparse, split==TRUE)
test2 = subset(emailsSparse, split==FALSE)

spam2CART = rpart(spam ~ ., data=train2, method="class")
prp(spam2CART )

set.seed(123)
spam2RF = randomForest(spam~., data=train2) 

# Predict accuracy

Predict.spam2CART.test = predict(spam2CART, newdata = test2) ### removed type = "class" 

#Accuracy
table(test2$spam, Predict.spam2CART.test[,2] > 0.5)

(1214 + 384) / (1214 + 384 + 94 + 26)
#0.9301513


# AUC
pred.test = prediction(Predict.spam2CART.test[,2], test$spam)
aucTest = as.numeric(performance(pred.test, "auc")@y.values)
aucTest
#0.9582438


#(Remember to pass the argument type="prob" to the predict function to get
# predicted probabilities for a random forest model.
# The probabilities will be the second column of the output.)

PredictTestRF2 = predict(spam2RF, newdata = test2, type="prob")[,2] 
table(test2$spam, PredictTestRF2 > 0.5)
(1298 + 382) / (1298 + 382 + 10 + 28)
#0.9778813

pred.test2.RF = prediction(PredictTestRF2, test2$spam) # with Probabilities
# AUC
auc2 = as.numeric(performance(pred.test2.RF, "auc")@y.values)
auc2


#Using n-grams
#Another source of information that might be extracted from text is the 
#frequency of various n-grams. An n-gram is a sequence of n consecutive words 
#in the document. For instance, for the document "Text analytics rocks!", 
#which we would preprocess to "text analyt rock", the 1-grams are "text",
#"analyt", and "rock", the 2-grams are "text analyt" and "analyt rock",
#and the only 3-gram is "text analyt rock". n-grams are order-specific,
#meaning the 2-grams "text analyt" and "analyt text" are considered two 
#separate n-grams. We can see that so far our analysis has been extracting
#only 1-grams.

#We do not have exercises in this class covering n-grams, but if you are
#interested in learning more, the "RTextTools", "tau", "RWeka", and "textcat"
#packages in R are all good resources.












