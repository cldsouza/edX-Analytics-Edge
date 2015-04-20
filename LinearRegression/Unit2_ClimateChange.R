climate = read.csv("climate_change.csv")
climate_test = subset(climate, Year > 2006)
climate_train = subset(climate, Year < 2007)

ClimateReg = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=climate_train)

ClimateReg2 = lm(Temp ~ MEI + N2O + TSI + Aerosols, data=climate_train)

step_climate = step (ClimateReg)

summary(step_climate)

#Make predictions on test set

predictTest = predict(step_climate, newdata=climate_test)

# Compute out-of-sample R^2
SSE = sum((predictTest - climate_test$Temp)^2)
SST = sum((mean(climate_train$Temp) - climate_test$Temp)^2)
R2 = 1 - SSE/SST
R2

RMSE = sqrt(SSE/nrow(climate_test))