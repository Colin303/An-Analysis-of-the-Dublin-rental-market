#Linear Regression
bike <- read.csv("BikeDayCleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)
str(bike)

library(dplyr)
library(caTools)

#sample of data - set to 731 to match the given testing set
BikeHour <- read.csv("BikeHourCleaned.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", check.names = FALSE)

set.seed (7)
test <- BikeHour[sample(nrow(BikeHour), 731), ]

#remove date, casual, registered - not needed
test <- test[, c(-1, -14, -15)]
str(test)

#Change the follwoing to factors
test$workingday = as.factor(test$workingday)
test$holiday = as.factor(test$holiday)
test$year = as.factor(test$year)

#pairs(test)

#remove non-numeric variables
num = test[, c(-1, -2, -3, -4, -5, -6, -7, -8)]

#check for correlations between numerical values
cor(num)

#correlation between temp and atemp
#low correlations with the rest of the variables

#Creating linear model with all variables, including temp & atemp
model = lm(count~., data = test)
summary(model)
#multiple R-squared value of 0.37, so 37% accurate. Not a bad result.

#plot model residuals, qq graphs
par(mfrow = c(1,1))
plot(model)

res <- signif(residuals(model), 5)
pre <- predict(model)
plot(model)

#remove atemp and temp seperately to compare accuracy result
model = lm(count~. -atemp, data = test)
summary(model)

model = lm(count~. -temp, data = test)
summary(model)
#same value for R-squared 0.37 (expected) - removing atemp/temp had no effect on the accuracy.

#Model built with only numeric values  
model = lm(count~., data = num)
summary(model)

#Accuracy is lower. R-Squared value is 0.24, 24%. This is expected as majority of
#predictors were removed.

#Build a model with hour being a numeric value
test2 <- test

#change hour to numeric
test2$hour = as.character(test2$hour)
test2$hour = as.numeric(test2$hour)

str(test2)

model = lm(count~., data = test2)
summary(model)

#R-Sqaured value is 0.37, 37% accuracy.
#previous models were better.

#multiple linear regression
#lm(what we want to predict ~ first independent variable + second independent variable, data = data name)
model <- lm(count ~ hour + temp, data = test2)
model
summary(model)
#poor model 27% accuracy

model <- lm(count ~ hour + weekday, data = test2)
model
summary(model)
#worse - 14% accuracy 

model <- lm(count ~ workingday + temp, data = test2)
model
summary(model)

#same as previous model 15% accuracy

model <- lm(count ~ temp + month, data = test2)
model
summary(model)
#15% accuracy 

#lets try more than two variables 
model2 <- lm(count ~ temp + hour + weather + season + workingday, data = test2)
model2
summary(model2)
#28%, better model with more variables 

model1 <- lm(count ~ temp + hour + weather + season + workingday + month + humidity + weekday, data = test2)
model1
summary(model1)

#34%, improving but not great.
#this doesnt seem like its the correct solution for the problem on hand. 
#but lets compare the two highest accuracy models using anova
anova(model1, model2)


