
#Load the dateset
trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)

summary(trials)
str(trials)

# How many characters are there in the longest abstract?
max(nchar(trials$abstract))

# How many search results provided no abstract? (HINT: A search result provided 
# no abstract if the number of characters in the abstract field is zero.)

table(nchar(trials$abstract) == 0)

#Find the observation with the minimum number of characters in the title 
#(the variable "title") out of all of the observations in this dataset. 

which.min(nchar(trials$title))
trials$title[1258]

#or

trials$title[which.min(nchar(trials$title))]

library(tm)
library(SnowballC)

# Create corpus for Title
corpusTitle = Corpus(VectorSource(trials$title))
corpusTitle[[1]]

# Create corpus for Abstract
corpusAbstract = Corpus(VectorSource(trials$abstract))
corpusAbstract[[1]]

# Convert to lower-case
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

# Remove punctuation

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

# Remove stopwords 
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

# Stem document 

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)


# Create matrix

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

# Remove sparse terms

sparse.dtmTitle = removeSparseTerms(dtmTitle, 0.95)
sparse.dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

# Convert to a data frame

dtmTitle = as.data.frame(as.matrix(sparse.dtmTitle))
dtmAbstract = as.data.frame(as.matrix(sparse.dtmAbstract))


##############################################################################
# Building a model
##############################################################################

#We want to combine dtmTitle and dtmAbstract into a single data frame to make
#predictions. However, some of the variables in these data frames have the 
#same names. To fix this issue, run the following commands:

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))


#Using cbind(), combine dtmTitle and dtmAbstract into a single data frame 
#called dtm:

dtm = cbind(dtmTitle, dtmAbstract)

#Copy dependant var "trial" to dtm, from original data frame called "trials"

dtm$trial = trials$trial

# Split the data

library(caTools)

set.seed(144)

split = sample.split(dtm$trial, SplitRatio = 0.7)

train = subset(dtm, split==TRUE)
test = subset(dtm, split==FALSE)

#Accuracy of baseline model on training set

730/ (572 + 730)
#0.5606759


##############################################################################
# Build a CART model
##############################################################################

library(rpart)
library(rpart.plot)

trialCART = rpart(trial ~ ., data=train, method="class")
prp(trialCART)


#############################################################################
#Adding probabilities 
#############################################################################

# Make predictions on the "TRAINING" set

pred = predict(trialCART, newdata=train)

pred[1:10,]
pred.prob = pred[,2]

summary(pred)

# Compute TRAINING set accuracy
table(train$trial, pred.prob >= 0.5)

#Training set Acc
(631 + 441) / (631 + 99 + 131 + 441)

#Sensitivity
441 / (441 + 131)

#Specificity 
631 / (631 + 99)

#OR

pred1 = predict(trialCART, newdata=train, type="class")
table(train$trial, pred1)

#Training set Acc
(631 + 441) / (631 + 99 + 131 + 441)

##############################################################################

# Make predictions on the "Testing" set

predTest = predict(trialCART, newdata=test, type="class")
table(test$trial, predTest)

# Accuracy
(261 + 162) / (261 + 52 + 83 + 162)


# ROC curve

library(ROCR)

predTest1 = predict(trialCART, newdata=test)
predTest1.prob = predTest1[,2]


predROCR = prediction(predTest1.prob, test$trial)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)










 






