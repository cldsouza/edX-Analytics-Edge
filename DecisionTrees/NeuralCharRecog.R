#Read in data
letters = read.csv("letters_ABPR.csv")

#create a new variable isB in the dataframe, which takes the value "TRUE" 
#if the observation corresponds to the letter B,

#letters$isB = as.factor(letters$letter == "B")
letters$isB = (letters$letter == "B")



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

##############################################################################
                  Neural Networks
##############################################################################

library("neuralnet")


net.charb <- neuralnet(isB ~ xbox + ybox + width + height + onpix + xbar + ybar + x2bar + y2bar + xybar + x2ybar + xy2bar + xedge + xedgeycor + yedge + yedgexcor, train, hidden=2, threshold=0.1)

print(net.charb)

#Plot the neural network
plot(net.charb)

test1 = test
test1$letter = NULL
test1$isB = NULL


net.results <- compute(net.charb, test1) #Run them through the neural network
 
#Lets see what properties net.results has
ls(net.results)

#Lets see the results
print(net.results$net.result)

table(test$isB, net.results$net.result > 0.5)

#Accuracy
(1135 + 348)/ (1135 + 348 + 40 + 35)
#0.95


##############################################################################

net.charb <- neuralnet(isB ~ xbox + ybox + width + height + onpix + xbar + ybar + x2bar + y2bar + xybar + x2ybar + xy2bar + xedge + xedgeycor + yedge + yedgexcor, train, hidden=1, threshold=0.05)

print(net.charb)

#Plot the neural network
plot(net.charb)

test1 = test
test1$letter = NULL
test1$isB = NULL


net.results <- compute(net.charb, test1) #Run them through the neural network
 
#Lets see what properties net.results has
ls(net.results)

#Lets see the results
print(net.results$net.result)

table(test$isB, net.results$net.result > 0.5)

#Accuracy
(1135 + 348)/ (1135 + 348 + 40 + 35)
#0.95




