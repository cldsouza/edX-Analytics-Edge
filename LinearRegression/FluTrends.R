FluTrain = read.csv("FluTrain.csv")
# which week corresponds to the highest percentage 
# of ILI-related physician visits? 
which.max(FluTrain$ILI)

FluTrain$Week[303]

which.max(FluTrain$Queries)

# Creating a histogram
  hist(FluTrain$ILI, xlab = "ILI)", main = "Histogram of ILI")


lm(formula = log(ILI) ~ Queries, data = FluTrain)

PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

# Next, we need to determine which element in the test set is for March 11, 2012.
# We can determine this with:

which(FluTest$Week == "2012-03-11 - 2012-03-17")

#Now we know we are looking for prediction number 11. This can be accessed with

PredTest1[11] 


SSE = sum((PredTest1 - FluTest$ILI)^2)

RMSE = sqrt(SSE/nrow(FluTest))

install.packages("zoo")
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)?
FluTrain$ILILag2 = coredata(ILILag2)

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)


FluTest$ILILag2[1] = FluTrain$ILI[416]


PredTest2 = exp(predict(FluTrend2, newdata=FluTest))

# Get RMSE
SSE = sum((PredTest2 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))




