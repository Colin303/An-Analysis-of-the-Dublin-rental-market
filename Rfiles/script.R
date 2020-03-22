setwd("/Users/colin/documents/workspace") #change this to where you downloaded the .csv

daft <- read.csv(file="daft.csv", head=TRUE, sep=",")
airbnb <- read.csv(file="airbnb.csv", head=TRUE, sep=",")#will autoencode the text attributes to factors
vacant <- read.csv(file="vacant.csv", head = TRUE, sep =",")

str(daft)

#check datasets for na
sapply(daft, FUN=function(x) sum(is.na(x)))
sapply(airdata, FUN=function(x) sum(is.na(x)))
sapply(vacant, FUN=function(x) sum(is.na(x)))

#dealing with Daft data
str(daft)
daft$price <- as.character(daft$price)
daft$addr <- as.character(daft$addr)
daft$daftURL <- as.character(daft$daftURL)

# Per week | Per Month cleansing
#change to numbers, if per month * 1, if per week * 4
daft$price
class(daft$price)
daft$price<- gsub("€", "", daft$price)
daft$price<- gsub(",", "", daft$price)
class(daft$price)
daft$price <- as.numeric(gsub('[Per weekPer month]','',daft$price)) * c(1,4)[grepl('Per week',daft$price) + 1]

#subset the datasets to what I need, especially airbnb & vacant
str(airbnb)
#subset of full airbnb dataset is air
air <- subset(airbnb, select=c(id, listing_url, street, neighbourhood, neighbourhood_cleansed, 
                               city, state, zipcode, smart_location, latitude, longitude, property_type,
                               room_type, bedrooms, beds, bed_type, price, weekly_price, monthly_price,
                               security_deposit, cleaning_fee, minimum_nights, maximum_nights, review_scores_value))

#subset has all the columbs we want, except some are factor and should be char
str(air)

#chaning those to char
air$street <- as.character(air$street)
air$neighbourhood <- as.character(air$neighbourhood)
air$neighbourhood_cleansed <- as.character(air$neighbourhood_cleansed)
air$city <- as.character(air$city)
air$state <- as.character(air$state)
air$zipcode <- as.character(air$zipcode)
air$smart_location <- as.character(air$smart_location)

#changing to char but will become numeric 
air$price <- as.character(air$price)
air$weekly_price <- as.character(air$weekly_price)
air$monthly_price <- as.character(air$monthly_price)

#changing price columns to numeric
#x[is.na(x)] <- 0

air$price
air$weekly_price
air$monthly_price

air$price<- gsub("\\.00", "", air$price)
air$price<- gsub("\\$", "", air$price)

air$weekly_price<- gsub("\\.00", "", air$weekly_price)
air$weekly_price<- gsub("\\$", "", air$weekly_price)
air$weekly_price<- gsub(",", "",  air$weekly_price)
#x[is.na(x)] <- 0 | change NA to 0 if needed

air$monthly_price<- gsub("\\.00", "", air$monthly_price)
air$monthly_price<- gsub("\\$", "", air$monthly_price)
air$weekly_price<- gsub(",", "",  air$monthly_price)
#x[is.na(x)] <- 0 | change NA to 0 if needed

air$price <- as.numeric(air$price)
air$weekly_price <- as.numeric(air$weekly_price)
air$monthly_price <- as.numeric(air$monthly_price)

#splitting the air data into just homes/apartments, not shared rooms
airdata <- subset(air,room_type %in% c('Entire home/apt'))

#splitting airdata dataset into different zipcodes

airdublin3 <-subset(airdata, zipcode %in% c('Dublin 3', '3', 'D03'))
airdublin4 <-subset(airdata, zipcode %in% c('Dublin 4', '4', 'D04'))
airdublin5 <-subset(airdata, zipcode %in% c('Dublin 5', '5', 'D05'))
airdublin6 <-subset(airdata, zipcode %in% c('Dublin 6', '6', 'D06'))
airdublin6w <-subset(airdata, zipcode %in%c('Dublin 6W', '6W', 'D6W'))
airdublin7 <-subset(airdata, zipcode %in% c('Dublin 7', '7', 'D07'))
airdublin8 <-subset(airdata, zipcode %in% c('Dublin 8', '9', 'D08'))
airdublin9 <-subset(airdata, zipcode %in% c('Dublin 9', '9', 'D09'))

airdublin1 <- airdata %>% select(id, listing_url, street, neighbourhood, neighbourhood_cleansed, 
                             city, state, zipcode, smart_location, latitude, longitude, property_type,
                             room_type, bedrooms, beds, bed_type, price, weekly_price, monthly_price,
                             security_deposit, cleaning_fee, minimum_nights, maximum_nights, review_scores_value) %>% filter(grepl('1', zipcode)) %>% filter(!grepl('18', zipcode)) %>% filter(!grepl('16', zipcode)) %>% filter(!grepl('15', zipcode)) %>% filter(!grepl('14', zipcode)) %>% filter(!grepl('13', zipcode)) %>% filter(!grepl('12', zipcode))%>% filter(!grepl('11', zipcode))%>% filter(!grepl('10', zipcode)) %>% filter(!grepl('17', zipcode))%>% filter(!grepl('19', zipcode))%>% filter(!grepl('D01', zipcode))%>% filter(!grepl('D02', zipcode))%>% filter(!grepl('D03', zipcode))%>% filter(!grepl('D04', zipcode))%>% filter(!grepl('D05', zipcode))%>% filter(!grepl('D06', zipcode))%>% filter(!grepl('D07', zipcode))%>% filter(!grepl('D08', zipcode))%>% filter(!grepl('D09', zipcode))%>% filter(!grepl('D20', zipcode))%>% filter(!grepl('D22', zipcode))%>% filter(!grepl('D24', zipcode))%>% filter(!grepl('D6W', zipcode))%>% filter(!grepl('K32', zipcode))%>% filter(!grepl('K67', zipcode))%>% filter(!grepl('K78', zipcode))          

airdublin10 <- airdata %>% select(street, neighbourhood, neighbourhood_cleansed, 
                                  city, state, zipcode, latitude, longitude, property_type,
                                  price, weekly_price, monthly_price) %>% filter(grepl('D10', zipcode))
airdublin11 <-subset(airdata, zipcode %in% c('Dublin 11', '11', 'D11'))
airdublin12 <-subset(airdata, zipcode %in% c('Dublin 12', '12', 'D12'))
airdublin13 <-subset(airdata, zipcode %in% c('Dublin 13', '13', 'D13'))
airdublin14 <-subset(airdata, zipcode %in% c('Dublin 14', '14', 'D14'))
airdublin15 <-subset(airdata, zipcode %in% c('Dublin 15', '15', 'D15'))
airdublin16 <-subset(airdata, zipcode %in% c('Dublin 16', '16', 'D16'))
airdublin17 <- airdata %>% select(street, neighbourhood, neighbourhood_cleansed, 
                                  city, state, zipcode, latitude, longitude, property_type,
                                  price, weekly_price, monthly_price) %>% filter(grepl('D17', zipcode))
airdublin18 <-subset(airdata, zipcode %in% c('Dublin 18', '18', 'D18'))
#airdublin19 = 0
airdublin2 <- airdata %>% select(street, neighbourhood, neighbourhood_cleansed, 
                             city, state, zipcode, latitude, longitude, property_type,
                             price, weekly_price, monthly_price) %>% filter(grepl('2', zipcode)) %>% filter(!grepl('20', zipcode)) %>% filter(!grepl('22', zipcode)) %>% filter(!grepl('24', zipcode)) %>% filter(!grepl('D20', zipcode))%>% filter(!grepl('D22', zipcode))%>% filter(!grepl('D24', zipcode))%>% filter(!grepl('D1', zipcode))%>% filter(!grepl('D0', zipcode))

airdublin20 <- airdata %>% select(street, neighbourhood, neighbourhood_cleansed, 
                               city, state, zipcode, latitude, longitude, property_type,
                               price, weekly_price, monthly_price) %>% filter(grepl('20', zipcode)) %>% filter(!grepl('D0', zipcode)) %>% filter(!grepl('D1', zipcode))%>% filter(!grepl('A96', zipcode))
airdublin22 <- airdata %>% select(street, neighbourhood, neighbourhood_cleansed, 
                                  city, state, zipcode, latitude, longitude, property_type,
                                  price, weekly_price, monthly_price) %>% filter(grepl('22', zipcode)) %>% filter(!grepl('D0', zipcode)) %>% filter(!grepl('D1', zipcode))%>% filter(!grepl('A96', zipcode))%>% filter(!grepl('K34', zipcode))%>% filter(!grepl('A94', zipcode))
airdublin24 <- airdata %>% select(street, neighbourhood, neighbourhood_cleansed, 
                                  city, state, zipcode, latitude, longitude, property_type,
                                  price, weekly_price, monthly_price) %>% filter(grepl('24', zipcode)) %>% filter(!grepl('D0', zipcode)) %>% filter(!grepl('D1', zipcode))%>% filter(!grepl('A96', zipcode))%>% filter(!grepl('K34', zipcode))%>% filter(!grepl('A94', zipcode))

#splitting daft into zipcodes..

daftdublin3 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 3', addr))
daftdublin4 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 4', addr))
daftdublin5 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 5', addr))
daftdublin6 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 6', addr)) %>% filter(!grepl('Dublin 6W', addr))
daftdublin6w <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 6W', addr))
daftdublin7 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 7', addr))
daftdublin8 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 8', addr))
daftdublin9 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 9', addr))

daftdublin1 <- daft %>% select(id, addr, price, daftURL) %>% filter(grepl('Dublin 1', addr)) %>% filter(!grepl('Dublin 18', addr)) %>% filter(!grepl('Dublin 16', addr)) %>% filter(!grepl('Dublin 15', addr)) %>% filter(!grepl('Dublin 14', addr)) %>% filter(!grepl('Dublin 13', addr)) %>% filter(!grepl('Dublin 12', addr))%>% filter(!grepl('Dublin 11', addr))%>% filter(!grepl('Dublin 10', addr)) %>% filter(!grepl('Dublin 17', addr)) %>% filter(!grepl('Dublin 19', addr))

daftdublin10 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 10', addr))
daftdublin11 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 11', addr))
daftdublin12 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 12', addr))
daftdublin13 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 13', addr))
daftdublin14 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 14', addr))
daftdublin15 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 15', addr))
daftdublin16 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 16', addr))
daftdublin17 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 17', addr))
daftdublin18 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 18', addr))

daftdublin2 <- daft %>% select(id, addr, price, daftURL) %>% filter(grepl('Dublin 2', addr)) %>% filter(!grepl('Dublin 20', addr)) %>% filter(!grepl('Dublin 22', addr)) %>% filter(!grepl('Dublin 24', addr))

daftdublin20 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 20', addr))
daftdublin22 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 22', addr))
daftdublin24 <- daft %>% select(id, addr, price, daftURL, longitude, latitude) %>% filter(grepl('Dublin 24', addr))

#okay so now I have split the data into each zipcode, I can now find average price for each zipcode for daft listings

daftavgd1 <- mean(daftdublin1$price)
daftavgd2 <- mean(daftdublin2$price)
daftavgd3 <- mean(daftdublin3$price)
daftavgd4 <- mean(daftdublin4$price)
daftavgd5 <- mean(daftdublin5$price)
daftavgd6 <- mean(daftdublin6$price)
daftavgd6w <- mean(daftdublin6w$price)
daftavgd7 <- mean(daftdublin7$price)
daftavgd8 <- mean(daftdublin8$price)
daftavgd9 <- mean(daftdublin9$price)
daftavgd10 <- mean(daftdublin10$price)
daftavgd11 <- mean(daftdublin11$price)
daftavgd12 <- mean(daftdublin12$price)
daftavgd13 <- mean(daftdublin13$price)
daftavgd14 <- mean(daftdublin14$price)
daftavgd15 <- mean(daftdublin15$price)
daftavgd16 <- mean(daftdublin16$price)
daftavgd17 <- mean(daftdublin17$price)
daftavgd18 <- mean(daftdublin18$price)
daftavgd20 <- mean(daftdublin20$price)
daftavgd22 <- mean(daftdublin22$price)
daftavgd24 <- mean(daftdublin24$price)

#same for airbnb listings
airavgd1 <- mean(airdublin1$price)
airavgd2 <- mean(airdublin2$price)
airavgd3 <- mean(airdublin3$price)
airavgd4 <- mean(airdublin4$price)
airavgd5 <- mean(airdublin5$price)
airavgd6 <- mean(airdublin6$price)
airavgd6w <- mean(airdublin6w$price)
airavgd7 <- mean(airdublin7$price)
airavgd8 <- mean(airdublin8$price)
airavgd9 <- mean(airdublin9$price)
airavgd10 <- mean(airdublin10$price)
airavgd11 <- mean(airdublin11$price)
airavgd12 <- mean(airdublin12$price)
airavgd13 <- mean(airdublin13$price)
airavgd14 <- mean(airdublin14$price)
airavgd15 <- mean(airdublin15$price)
airavgd16 <- mean(airdublin16$price)
airavgd17 <- mean(airdublin17$price)
airavgd18 <- mean(airdublin18$price)
airavgd20 <- mean(airdublin20$price)
airavgd22 <- mean(airdublin22$price, na.rm = TRUE)#found 1 NA so just removed it, didnt change the average 
airavgd24 <- mean(airdublin24$price)


zipcode <- c('D1','D2','D3','D4','D5','D6','D6w','D7','D8','D9','D10','D11','D12','D13','D14','D15','D16','D17','D18','D20','D22','D24')
airbnbaverage <- c(airavgd1,airavgd2,airavgd3,airavgd4,airavgd5,airavgd6,airavgd6w,airavgd7,airavgd8,airavgd9,airavgd10,airavgd11,airavgd12,airavgd13,airavgd14,airavgd15,airavgd16,airavgd17,airavgd18,airavgd20,airavgd22,airavgd24)
daftaverage <- c(daftavgd1,daftavgd2,daftavgd3,daftavgd4,daftavgd5,daftavgd6,daftavgd6w,daftavgd7,daftavgd8,daftavgd9,daftavgd10,daftavgd11,daftavgd12,daftavgd13,daftavgd14,daftavgd15,daftavgd16,daftavgd17,daftavgd18,daftavgd20,daftavgd22,daftavgd24)

averages.data <- data.frame(zipcode, airbnbaverage, daftaverage)

#barchart for daft average rent by zipcode
str(averages.data)
averages.data$zipcode <- as.character(averages.data$zipcode)

averages.data$zipcode <- factor(averages.data$zipcode, levels = averages.data$zipcode[order(-averages.data$daftaverage)])

ggplot(data=averages.data, aes(x=zipcode, y=daftaverage)) +
  geom_bar(stat="identity", fill = "tomato")

#barchart for airbnb average price by zipcode
str(averages.data)
averages.data$zipcode <- as.character(averages.data$zipcode)

averages.data$zipcode <- factor(averages.data$zipcode, levels = averages.data$zipcode[order(-averages.data$airbnbaverage)])

ggplot(data=averages.data, aes(x=zipcode, y=airbnbaverage)) +
  geom_bar(stat="identity", fill = "blue")


# Simple Scatterplot
str(averages.data)
averages.data$zipcode <- as.character(averages.data$zipcode)

plot(averages.data$airbnbaverage,averages.data$daftaverage , main="Scatterplot by AVG", 
     xlab="Airbnb daily price", ylab="Rent")
abline(lm(daftaverage~airbnbaverage), col="red") # regression line (y~x) 






