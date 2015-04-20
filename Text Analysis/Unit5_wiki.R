# Load the dataset
wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)

# Create dependent variable
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

library(tm)
library(SnowballC)


# Create corpus
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded[[1]]

corpusAdded = tm_map(corpusAdded, PlainTextDocument)

# Remove stopwords 
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))

corpusAdded[[1]]

# Stem document 

corpusAdded = tm_map(corpusAdded, stemDocument)

corpusAdded[[1]]

# Create matrix

dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

# Check for sparsity

findFreqTerms(dtmAdded, lowfreq=20)

# Remove sparse terms

sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

# Convert to a data frame

wordsAdded = as.data.frame(as.matrix(sparseAdded))

# prepend all the words with the letter A, by using the command

colnames(wordsAdded) = paste("A", colnames(wordsAdded))


#############################################################################
#Repeat steps for removed words to create Removed bag of words data frame
#prepended by R instead of A
#############################################################################


# Create corpus
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved[[1]]

corpusRemoved = tm_map(corpusRemoved, PlainTextDocument)

# Remove stopwords 
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))

corpusRemoved[[1]]

# Stem document 

corpusRemoved = tm_map(corpusRemoved, stemDocument)

corpusRemoved[[1]]

# Create matrix

dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved

# Check for sparsity

findFreqTerms(dtmRemoved, lowfreq=20)

# Remove sparse terms

sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

# Convert to a data frame

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))

# prepend all the words with the letter R, by using the command

colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))


#Combine the two data frames into a data frame called wikiWords with the
#following line of code

wikiWords = cbind(wordsAdded, wordsRemoved) 

# Add dependent variable Vandal from wiki frame to wikiWords

wikiWords$Vandal = wiki$Vandal

# Split the data

library(caTools)

set.seed(123)

split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)

trainWikiWords = subset(wikiWords, split==TRUE)
testWikiWords = subset(wikiWords, split==FALSE)


# Build a CART model

library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal ~ ., data=trainWikiWords, method="class")

prp(wikiCART)

# Evaluate the performance of the model
predict.wikiCART = predict(wikiCART, newdata=testWikiWords, type="class")

table(testWikiWords$Vandal, predict.wikiCART)
# This model gives a very low accuracy rate

#############################################################################

#copy the data frame
wikiWords2 = wikiWords

#Typically if the revision comments include http(self promoting website) 
#it could be a Vandal 
#Make a new column in wikiWords2 that is 1 if "http" was in Added 

wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

table(wikiWords2$HTTP)

# Creating a new CART model with HTTP as an independent variable

wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)

wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")

prp(wikiCART2)

# Evaluate the performance of the model
predict.wikiCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")

table(wikiTest2$Vandal, predict.wikiCART2)

#Accuracy
(609 + 57)/ (609 + 9 + 488 + 57)

#The number of words added or removed could be predictive more so than the
#actual words themselves

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))


#Create a new CART tree

wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)

wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")

prp(wikiCART2)

# Evaluate the performance of the model
predict.wikiCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")

table(wikiTest2$Vandal, predict.wikiCART2)
(514 + 248) / ( 514 + 248 + 297 + 104)


#############################################################################

wikiWords3 = wikiWords2

# add the two original variables Minor and Loggedin to this new data frame:

wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

#Create a new CART tree

wikiTrain3 = subset(wikiWords3, split==TRUE)
wikiTest3 = subset(wikiWords3, split==FALSE)

wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")

prp(wikiCART3)

# Evaluate the performance of the model
predict.wikiCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")

table(wikiTest3$Vandal, predict.wikiCART3)

(595 + 241) / (595 + 241 + 23 + 304)









