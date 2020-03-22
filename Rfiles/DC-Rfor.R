#datacamp Random forest tutorial
#https://datascienceplus.com/random-forests-in-r?utm_campaign=News&utm_medium=Community&utm_source=DataCamp.com

require(randomForest)
require(MASS) #package that contains bostan dataset
attach(Boston)
set.seed(101)

dim(Boston)

#split into training and test datasets

#training Sample with 300 observations
train=sample(1:nrow(Boston),300)
?Boston  #to search on the dataset

#We are going to use variable ′medv′ as the Response variable, which is the Median Housing Value.
#We will fit 500 Trees

Boston.rf=randomForest(medv ~ . , data = Boston , subset = train)
Boston.rf

plot(Boston.rf)

#The above Random Forest model chose Randomly 4 variables to be considered at each split. 
#We could now try all possible 13 predictors which can be found at each split.

oob.err=double(13)
test.err=double(13)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:13)
{
  rf=randomForest(medv ~ . , data = Boston , subset = train,mtry=mtry,ntree=400) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,Boston[-train,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(Boston[-train,], mean( (medv - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}

test.err

oob.err

#What happens is that we are growing 400 trees for 13 times i.e for all 13 predictors.
matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))

