loans = read.csv("loans.csv")

# Which of the following is the best reason to fill in the missing values for 
# these variables instead of removing observations with missing data? 
# (Hint: you can use the subset() function to build a data frame with the
# observations missing at least one value. To test if a variable, for example 
# pub.rec, is missing a value, use is.na(pub.rec).)
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))


library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
#We predicted missing variable values using the
#available independent variables for each observation. 

#Split into Training and Test sets
set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

# Logistic Regression Model
loansLog = glm(not.fully.paid ~ ., data = train, family=binomial)
summary(loansLog)

# Consider two loan applications, which are identical other than the fact 
# that the borrower in Application A has FICO credit 
# score 700 while the borrower in Application B has FICO credit score 710.
oddsA <- as.numeric(exp(coefficients(loansLog)[c("(Intercept)")] + coefficients(loansLog)[c("fico")] * 700) )

#predict
predictTest = predict(loansLog, type ="response", newdata = test)

# Add the predicted.risk variable to the test set
test$predicted.risk <- predictTest

# Create the confusion matrix of the test set use 0.5
table(test$not.fully.paid , predictTest > 0.5)

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predictTest, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)
auc

bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial") 

#predict
predictBivariateTest = predict(bivariate, type ="response", newdata = test)

table(test$not.fully.paid , predictBivariateTest > 0.5)

prediction.bivariate <- prediction(predictBivariateTest, test$not.fully.paid)
as.numeric(performance(prediction.bivariate, "auc")@y.values)

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

# Maximum profit
 10*(exp(0.2121 * 3) - 1)

#use the subset() function to build a data frame called highInterest 
#consisting of the test set loans with an interest rate of at least 15%.

highInterest = subset ( test, test$int.rate >= 0.15 ) 
summary(highInterest$profit)

# Next, we will determine the 100th smallest predicted probability 
# of not paying in full by sorting the predicted risks in increasing order 
# and selecting the 100th element of this sorted list. Find the highest
# predicted risk that we will include by typing the following command into 
# your R console

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]

# Build a data frame called selectedLoans consisting of the high-interest 
#loans with predicted risk not exceeding the cutoff 

selectedLoans <- subset(highInterest, highInterest$predicted.risk <= cutoff)

# Number of loans that were not fully paid in selected 100 loans

table(selectedLoans$not.fully.paid) 

summary(selectedLoans$profit)

#Practice with data set
tapply(test$predicted.risk, test$not.fully.paid, mean) 
tapply(selectedLoans$predicted.risk, selectedLoans$not.fully.paid, mean)
cutoff1 = sort(highInterest$predicted.risk, decreasing=TRUE)[100]
#notice difference with cutoff and cutoff1

 
