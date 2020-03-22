#algortihms
df <- daft_city

str(df)
#remove unused columns (addr, long, lat)
df <- df[, -c(2, 3, 4)]

str(df)
dfnum <- df[, -c(4, 6)]
str(dfnum)

cor(dfnum)

model = lm(price~., data = df)
summary(model)
plot(model)

df2 <- df[, -6]

str(df2)

model2 = lm(price~., data = df2)
summary(model2)

df3 <- df[, -c(2,3,4,6)]
str(df3)

model3 = lm(price~., data = df3)
summary(model3)
plot(model3)

boxplot(daft_city$price)

install.packages("outliers")
library(outliers)

daft_city <- daft
daft_city$postcode <- daft_city$addr
daft_city$postcode<- gsub('.*\\,', "", daft_city$postcode)
str(daft_city)
daft_city$postcode <- as.factor(daft_city$postcode)
table(levels(daft_city$postcode))
table(daft_city$postcode)
daft_city$postcode <- as.character(daft_city$postcode)

daft_city[daft_city$postcode %in% c("Smithfield Market"),]$postcode <- "Dublin 7"
daft_city[daft_city$postcode %in% c(11),]$postcode <- "Dublin 11"
daft_city[daft_city$postcode %in% c(31),]$postcode <- "Co. Dublin"

daft_city$postcode <- as.factor(daft_city$postcode)

df <- daft_city
str(df)

dfnew <- df[!rowSums(df[1] >6000),]
hist(dfnew$price)
boxplot(dfnew$price)

str(dfnew)
dfnew<- dfnew[, -c(2, 3, 4, 10, 11, 12)]

model4 = lm(price~., data = dfnew)
summary(model4)
set.seed(1)
library(dplyr)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(dfnew))

## set the seed to make your partition reproducible
train_ind <- sample(seq_len(nrow(dfnew)), size = smp_size)

train <- dfnew[train_ind, ]
test <- dfnew[-train_ind, ]

model5 = lm(price~., data = test)
summary(model5)
library(tidyr)

install.packages("caret")
library(caret)
lake <- train(price~., dfnew, method = "lm", trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
print(lake)


rf <- train(price~., dfnew, method = "rf", trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
print(rf)

