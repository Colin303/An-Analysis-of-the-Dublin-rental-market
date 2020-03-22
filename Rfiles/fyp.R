#final year project
#install.packages("pxR")
#install.packages("dplyr")
#install.packages("tree")
library(tree)
library(pxR)
library(dplyr)
library(ggplot2)
library(plotly)
?read.px
df <- read.px("ria02.px")
df <- as.data.frame(df)
?select
dublin <- df %>% select(Number.of.Bedrooms, Property.Type, Location, Year, value) %>% filter(grepl('Dublin', Location))
str(dublin)
levels(clean$Property.Type)
sapply(dublin, FUN=function(x) sum(is.na(x)))

dublin2017 <- dublin %>% select(Number.of.Bedrooms, Property.Type, Location, Year, value) %>% filter(grepl('2017', Year))

sapply(dublin2017, FUN=function(x) sum(is.na(x)))

dublin2017 <- droplevels.data.frame(dublin2017)

clean <- na.omit(dublin2017)
str(clean)
clean$Location <- as.character(clean$Location)

names <- c("bedrooms", "prop", "location", "year", "value")
colnames(clean) <- names

str(clean)
tree = tree(value~., clean)
summary(tree)
plot(tree)
text(tree, pretty=0)
install.packages("rattle")
library(rattle)
asRules(fit)
library(rpart)
fit <- rpart(value~., data = clean)

cv.tree = cv.tree(tree)
plot(cv.tree$size, cv.tree$dev, type = 'b')

prune.tree = prune.tree(tree, best=3)
plot(prune.tree)
text(prune.tree, pretty=1)
summary(prune.tree)

?aggregate
library(dplyr)
remove(col)
agg <- clean %>% group_by(Property.Type, Location) %>% summarise(round(mean(value), 2))

#------------------------------------------------------------------------------------
#daft city cleaning
#daft city is a new dataset from daft with all dublin areas, incl new columns
daft_city <- read.csv(file = "daft_city.csv", sep = ",", stringsAsFactors = FALSE)
?read.csv
daft_city <- daft_city[, c(-1, -2)]

#check for NAs
sapply(daft_city, FUN=function(x) sum(is.na(x)))

#overviews have 69 Nas (looks exlusively studio apartment to rent are missing this data)

#now to deal with price
daft_city$price<- gsub("€", "", daft_city$price)
daft_city$price<- gsub(",", "", daft_city$price)
class(daft_city$price)
daft_city$price <- as.numeric(gsub('[Per weekPer month]','',daft_city$price)) * c(1,4)[grepl('Per week',daft_city$price) + 1]
#changed weekly to monthly and changed to numeric
class(daft_city$price)

#looking at graphs, data looks non-normal
hist(daft_city$price)
boxplot(daft_city$price) 
qqnorm(daft_city$price)

#shapiro test confirms this
shapiro.test(daft_city$price)

#removing row 39 cause its messed up
daft_city<- daft_city[-c(39),]

#now looking at another column
head(daft_city$overviews)
library(tidyr)

furnished <- "furnished"
beds <- "beds"
daft_city$beds <- beds
daft_city$furnished <- furnished

#splitting the overviews column into its separate parts
daft_city<- separate(daft_city, overviews, furnished, sep = ",", remove = F, convert = T)

#now I have a furnished column f
#remove it from the original column and split again
daft_city$overviews<- gsub("Furnished,", "", daft_city$overviews)
daft_city$overviews<- gsub("Unfurnished,", "", daft_city$overviews)
daft_city$overviews<- gsub("Furnished or unfurnished,", "", daft_city$overviews)

daft_city<- separate(daft_city, overviews, beds, sep = ",", remove = F, convert = T)

#removes everything before the comma, leaving only number of bathrooms
daft_city$overviews<- gsub('.*\\,', "", daft_city$overviews)

#looking at beds column, i want to remove the (double / single) values
head(daft_city$beds)

daft_city$beds<- gsub('\\(.*', "", daft_city$beds)

#making a backup from this point.. 
backup <- daft_city

#remove non-numbers
daft_city$beds<- gsub(' ', "", daft_city$beds)
daft_city$beds<- gsub("[^0-9.-]", "", daft_city$beds)
class(daft_city$beds)

#now changing beds to factors
str(daft_city$beds)
daft_city$beds <- as.factor(daft_city$beds)
str(daft_city$beds)
levels(daft_city$beds)
head(daft_city$beds)

#now to do the same for bathrooms
#change colname overviews to bathrooms
names(daft_city)[names(daft_city) == 'overviews'] <- 'bathrooms'

#removing non numeric
head(daft_city$bathrooms)
daft_city$bathrooms<- gsub(' ', "", daft_city$bathrooms)
daft_city$bathrooms<- gsub("[^0-9.-]", "", daft_city$bathrooms)
head(daft_city$bathrooms)

#now changing bathroom to factors
str(daft_city$bathrooms)
daft_city$bathrooms <- as.factor(daft_city$bathrooms)
str(daft_city$bathrooms)
levels(daft_city$bathrooms)
head(daft_city$bathrooms)

#now to make furnished a factor
daft_city$furnished<- gsub('\\(', "", daft_city$furnished)
daft_city$furnished<- gsub('4 Bedrooms 1 single', "", daft_city$furnished)
daft_city$furnished <- as.factor(daft_city$furnished)
levels(daft_city$furnished)

#same for dwelling_type
#rename to dwelling
names(daft_city)[names(daft_city) == 'dwelling_type'] <- 'dwelling'
daft_city$dwelling <- as.factor(daft_city$dwelling)
levels(daft_city$dwelling)

#rename city_center_distance to distance, change to numeric
names(daft_city)[names(daft_city) == 'city_center_distance'] <- 'distance'
class(daft_city$distance)
daft_city$distance <- as.numeric(daft_city$distance)

hist(daft_city$distance)
boxplot(daft_city$distance)

#removing row 139 because long + lat is completely wrong and massive outlier for city center distance
daft_city[138,] <- 0
daft_city <- daft_city[-138,]
#row 955 has Na for distance, looking at its co-ordinates, looks like it should be about a value of 2.
daft_city$distance[is.na(daft_city$distance)]<-2

#looking good, last to deal with address
str(daft_city)
backup <- daft_city

str(daft_city)

#graphs ---------------------------------------------------------
hist(daft_city$price, col = "blue", main = "Histogram of price")
hist(daft_city$distance, col = "red")

daft_city$bathrooms <- as.numeric(daft_city$bathrooms)
hist(daft_city$bathrooms, col = "green")

daft_city$beds <- as.numeric(daft_city$beds)
hist(daft_city$bathrooms, col = "yellow")
?boxplot
boxplot(daft_city$price, main = "Boxplot of Price")
boxplot(daft_city$distance, main = "Boxplot of distance")
boxplot(daft_city$beds, main = "Boxplot of beds")
boxplot(daft_city$bathrooms, main = "Boxplot of bathrooms")

daft_city$beds <- as.factor(daft_city$beds)

counts <- table(daft_city$beds)
barplot(counts, col = "red", main = "Barchart of Beds")

counts <- table(daft_city$bathrooms)
barplot(counts, col = "blue", main = "Barchart of Bathrooms")

#correlations

library(ggplot2)
ggplot(data = daft_city, aes(x = distance, y = price)) +
  geom_point()

corDF = data.frame(daft_city$price, daft_city$distance, daft_city$beds, daft_city$bathrooms)
cor(corDF)

library("corrplot")
corVis <- cor(corDF)
corrplot(corVis, method = "number")
corrplot(corVis, method = "color")

# splitting daft_city into different areas, dublin 2, dublin 4, dublin 24 etc

#splitting daft into zipcodes..

daft <- daft_city

daftdublin3 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling) %>% filter(grepl('Dublin 3', addr))
daftdublin4 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 4', addr))
daftdublin5 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 5', addr))
daftdublin6 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 6', addr)) %>% filter(!grepl('Dublin 6W', addr))
daftdublin6w <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 6W', addr))
daftdublin7 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 7', addr))
daftdublin8 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 8', addr))
daftdublin9 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 9', addr))

daftdublin1 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 1', addr)) %>% filter(!grepl('Dublin 18', addr)) %>% filter(!grepl('Dublin 16', addr)) %>% filter(!grepl('Dublin 15', addr)) %>% filter(!grepl('Dublin 14', addr)) %>% filter(!grepl('Dublin 13', addr)) %>% filter(!grepl('Dublin 12', addr))%>% filter(!grepl('Dublin 11', addr))%>% filter(!grepl('Dublin 10', addr)) %>% filter(!grepl('Dublin 17', addr)) %>% filter(!grepl('Dublin 19', addr))

daftdublin10 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 10', addr))
daftdublin11 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 11', addr))
daftdublin12 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 12', addr))
daftdublin13 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 13', addr))
daftdublin14 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling) %>% filter(grepl('Dublin 14', addr))
daftdublin15 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 15', addr))
daftdublin16 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 16', addr))
daftdublin17 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 17', addr))
daftdublin18 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling) %>% filter(grepl('Dublin 18', addr))

daftdublin2 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 2', addr)) %>% filter(!grepl('Dublin 20', addr)) %>% filter(!grepl('Dublin 22', addr)) %>% filter(!grepl('Dublin 24', addr))

daftdublin20 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 20', addr))
daftdublin22 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 22', addr))
daftdublin24 <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling)  %>% filter(grepl('Dublin 24', addr))

#non post code dublin locations, known as "other".
daftdublinco <- daft %>% select(price, addr,longitude, latitude, bathrooms, beds, furnished, distance, dwelling) %>% filter(grepl('Co. Dublin', addr))

counts <- table(daft_city$dwelling)
barplot(counts, main="Types of Dwellings")
counts$
library(plotly)

#Dwellings all data ------------------------------------
d <- summary(daft_city$dwelling)
p <- plot_ly(daft_city, x = names(d), y = d, type = 'bar',
  marker = list(color = c('rgba(222,45,38,0.8)', 'rgba(204,204,204,1)',
                          'rgba(204,204,204,1)', 'rgba(204,204,204,1)')))%>%
layout(title = "Dwellings All Data",
       xaxis = list(title = "Types"),
       yaxis = list(title = "Amount"))
p

#dwellings dublin other ------------------------------------
d2 <- summary(daftdublinco$dwelling)
p2 <- plot_ly(daftdublinco, x = names(d2), y = d2, type = 'bar',
             marker = list(color = c('rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                                     'rgba(222,45,38,0.8)', 'rgba(204,204,204,1)')))%>%
  layout(title = "Dwellings Dublin Other",
         xaxis = list(title = "Types"),
         yaxis = list(title = "Amount"))
p2

#dwellings dublin 2 ------------------------------------

d3 <- summary(daftdublin2$dwelling)
p3 <- plot_ly(daftdublin2, x = names(d3), y = d3, type = 'bar',
              marker = list(color = c('rgba(222,45,38,0.8)', 'rgba(204,204,204,1)',
                                      'rgba(204,204,204,1)', 'rgba(204,204,204,1)')))%>%
  layout(title = "Dwellings Dublin 2",
         xaxis = list(title = "Types"),
         yaxis = list(title = "Amount"))
p3

#dwelling comparison Dublin 2 / Dublin other

data <- data.frame(names(d3), d3, d2)

p4 <- plot_ly(data, x = ~names.d3., y = d3, type = 'bar', name = 'Dublin 2', marker = list(color = 'rgb(49,130,189)')) %>%
  add_trace(y = ~d2, name = 'Dublin Other', marker = list(color = 'rgb(204,204,204)')) %>%
  layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')
p4

#------------ comparison of North side
s1 <- summary(daftdublin1$beds)
s3 <- summary(daftdublin3$beds)
s5 <- summary(daftdublin5$beds)
s7 <- summary(daftdublin7$beds)
s9 <- summary(daftdublin9$beds)
s11 <- summary(daftdublin11$beds)
s13 <- summary(daftdublin13$beds)
s15 <- summary(daftdublin15$beds)
s17 <- summary(daftdublin17$beds)


north <- data.frame(names(s1), s3, s5, s7, s9, s11, s13, s15, s17, d2)

p5 <- plot_ly(north, x = ~names.s1., y = s1, type = 'bar', name = 'Dublin 1', marker = list(color = 'rgb(49,130,189)')) %>%
  add_trace(y = ~s3, name = 'Dublin 3', marker = list(color = "green")) %>%
  add_trace(y = ~s5, name = 'Dublin 5', marker = list(color = "purple")) %>%
  add_trace(y = ~s7, name = 'Dublin 7', marker = list(color = "orange")) %>%
  add_trace(y = ~s9, name = 'Dublin 9', marker = list(color = "yellow")) %>%
  add_trace(y = ~s11, name = 'Dublin 11', marker = list(color = "pink")) %>%
  add_trace(y = ~s13, name = 'Dublin 13', marker = list(color = "red")) %>%
  add_trace(y = ~s15, name = 'Dublin 15', marker = list(color = "black")) %>%
  add_trace(y = ~s17, name = 'Dublin 17', marker = list(color = "gray")) %>%
  
  layout(title = "North side beds",
    xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')
p5

#Comparison of south side -----------------------------------------------------

s2 <- summary(daftdublin2$beds)
s4 <- summary(daftdublin4$beds)
s6 <- summary(daftdublin6$beds)
s6w <- summary(daftdublin6w$beds)
s8 <- summary(daftdublin8$beds)
s10 <- summary(daftdublin10$beds)
s12 <- summary(daftdublin12$beds)
s14 <- summary(daftdublin14$beds)
s16 <- summary(daftdublin16$beds)
s18<- summary(daftdublin18$beds)
s20<- summary(daftdublin20$beds)
s22<- summary(daftdublin22$beds)
s24<- summary(daftdublin24$beds)


south <- data.frame(names(s2), s2, s4, s6, s6w, s8, s10, s12, s14, s16, s18, s20, s22, s24)

p6 <- plot_ly(south, x = ~names.s2., y = s2, type = 'bar', name = 'Dublin 2', marker = list(color = 'rgb(49,130,189)')) %>%
  add_trace(y = ~s4, name = 'Dublin 4', marker = list(color = "green")) %>%
  add_trace(y = ~s6, name = 'Dublin 6', marker = list(color = "purple")) %>%
  add_trace(y = ~s6w, name = 'Dublin 6w', marker = list(color = "orange")) %>%
  add_trace(y = ~s8, name = 'Dublin 8', marker = list(color = "yellow")) %>%
  add_trace(y = ~s10, name = 'Dublin 10', marker = list(color = "pink")) %>%
  add_trace(y = ~s12, name = 'Dublin 12', marker = list(color = "red")) %>%
  add_trace(y = ~s14, name = 'Dublin 14', marker = list(color = "black")) %>%
  add_trace(y = ~s16, name = 'Dublin 16', marker = list(color = "brown")) %>%
  add_trace(y = ~s18, name = 'Dublin 18', marker = list(color = "olive")) %>%
  add_trace(y = ~s20, name = 'Dublin 20', marker = list(color = "cream")) %>%
  add_trace(y = ~s24, name = 'Dublin 24', marker = list(color = "ginger")) %>%
  layout(title = "South side beds",
    xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')
p6

#comparison of sides: red = south, blue = north --------------------------
#side by side
both <- north
both <- cbind(north, south)

p7 <- plot_ly(both, x = ~names.s1., y = s2, type = 'bar', name = 'Dublin 2', marker = list(color = "red")) %>%
  add_trace(y = ~s4, name = 'Dublin 4', marker = list(color = "red")) %>%
  add_trace(y = ~s6, name = 'Dublin 6', marker = list(color = "red")) %>%
  add_trace(y = ~s6w, name = 'Dublin 6w', marker = list(color = "red")) %>%
  add_trace(y = ~s8, name = 'Dublin 8', marker = list(color = "red")) %>%
  add_trace(y = ~s10, name = 'Dublin 10', marker = list(color = "red")) %>%
  add_trace(y = ~s12, name = 'Dublin 12', marker = list(color = "red")) %>%
  add_trace(y = ~s14, name = 'Dublin 14', marker = list(color = "red")) %>%
  add_trace(y = ~s16, name = 'Dublin 16', marker = list(color = "red")) %>%
  add_trace(y = ~s18, name = 'Dublin 18', marker = list(color = "red")) %>%
  add_trace(y = ~s20, name = 'Dublin 20', marker = list(color = "red")) %>%
  add_trace(y = ~s24, name = 'Dublin 24', marker = list(color = "red")) %>%

  add_trace(y = ~s1, name = 'Dublin 1', marker = list(color = "blue")) %>%
  add_trace(y = ~s3, name = 'Dublin 3', marker = list(color = "blue")) %>%
  add_trace(y = ~s5, name = 'Dublin 5', marker = list(color = "blue")) %>%
  add_trace(y = ~s7, name = 'Dublin 7', marker = list(color = "blue")) %>%
  add_trace(y = ~s9, name = 'Dublin 9', marker = list(color = "blue")) %>%
  add_trace(y = ~s11, name = 'Dublin 11', marker = list(color = "blue")) %>%
  add_trace(y = ~s13, name = 'Dublin 13', marker = list(color = "blue")) %>%
  add_trace(y = ~s15, name = 'Dublin 15', marker = list(color = "blue")) %>%
  add_trace(y = ~s17, name = 'Dublin 17', marker = list(color = "blue")) %>%
  layout(title = "Comparison",
         xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')
p7

#comparison combined w/ Dublin other ---------------------------------------------------------------------

p8 <- plot_ly(both, x = ~names.s1., y = s2, type = 'bar', name = 'Dublin 2', marker = list(color = "red")) %>%
  
  
  add_trace(y = ~s1, name = 'Dublin 1', marker = list(color = "blue")) %>%
  add_trace(y = ~s4, name = 'Dublin 4', marker = list(color = "red")) %>%
  add_trace(y = ~s3, name = 'Dublin 3', marker = list(color = "blue")) %>%
  add_trace(y = ~s5, name = 'Dublin 5', marker = list(color = "blue")) %>%
  add_trace(y = ~s6, name = 'Dublin 6', marker = list(color = "red")) %>%
  add_trace(y = ~s6w, name = 'Dublin 6w', marker = list(color = "red")) %>%
  add_trace(y = ~s7, name = 'Dublin 7', marker = list(color = "blue")) %>%
  add_trace(y = ~s8, name = 'Dublin 8', marker = list(color = "red")) %>%
  add_trace(y = ~s9, name = 'Dublin 9', marker = list(color = "blue")) %>%
  add_trace(y = ~s10, name = 'Dublin 10', marker = list(color = "red")) %>%
  add_trace(y = ~s11, name = 'Dublin 11', marker = list(color = "blue")) %>%
  add_trace(y = ~s12, name = 'Dublin 12', marker = list(color = "red")) %>%
  add_trace(y = ~s13, name = 'Dublin 13', marker = list(color = "blue")) %>%
  add_trace(y = ~s14, name = 'Dublin 14', marker = list(color = "red")) %>%
  add_trace(y = ~s15, name = 'Dublin 15', marker = list(color = "blue")) %>%
  add_trace(y = ~s16, name = 'Dublin 16', marker = list(color = "red")) %>%
  add_trace(y = ~s17, name = 'Dublin 17', marker = list(color = "blue")) %>%
  add_trace(y = ~s18, name = 'Dublin 18', marker = list(color = "red")) %>%
  add_trace(y = ~s20, name = 'Dublin 20', marker = list(color = "red")) %>%
  add_trace(y = ~s24, name = 'Dublin 24', marker = list(color = "red")) %>%
  add_trace(y = ~d2, name = 'Dublin Other', marker = list(color = "green")) %>%
  
  layout(title = "Comparison",
         xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')
p8

#---------------------------------------------------------------------------------
chart(dublin2017)



