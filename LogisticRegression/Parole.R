parole = read.csv("parole.csv")
#Number of parolees who violated the terms of parole
table(parole$violator)

# To convert variables to factors, the following commands should be run
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# Logistic Regression Model
paroleLog = glm(violator ~ ., data = train, family=binomial)
summary(paroleLog)

#Consider a parolee who is male, of white race, aged 50 years at prison release, from the state of Maryland, served 3 months, had a maximum sentence of 12 months, did not commit multiple offenses, and committed a larceny. Answer the following questions based on the model's predictions for this individual. (HINT: You should use the coefficients of your model, the Logistic Response Function, and the Odds equation to solve this problem.)
odds <- as.numeric(exp(coefficients(paroleLog)[c("(Intercept)")] + coefficients(paroleLog)[c("male")] * 1 + coefficients(paroleLog)[c("race")] * 1 + coefficients(paroleLog)[c("time.served")] * 3 + coefficients(paroleLog)[c("max.sentence")] * 12 + coefficients(paroleLog)[c("multiple.offenses")] *  0 + coefficients(paroleLog)[c("crime2")]))

#predict
predictTest = predict(paroleLog, type ="response", newdata = test)


# Confusion matrix with threshold of 0.5
#remember to check against TEST set and not complete Data - I always seem to make this MISTAKE
table(test$violator, predictTest > 0.5)
