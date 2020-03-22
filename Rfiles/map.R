
# load packages
library(ggplot2)
library(ggmap)
library(ggalt)
x <- data.frame(daft$longitude, daft$latitude)
df.places <- c(daft$longitude, daft$latitude)

y <-data.frame(air$longitude, air$latitude)

#--------avg price / postcode------------------

#entered in order to match map, not in numerical order
avg <- c(mean(daftdublin13$price), mean(daftdublinco$price), mean(daftdublin2$price), mean(daftdublin10$price), mean(daftdublin1$price),
         mean(daftdublin15$price), mean(daftdublin9$price), mean(daftdublin17$price), mean(daftdublin5$price),
         mean(daftdublin3$price), mean(daftdublin7$price), mean(daftdublin8$price), mean(daftdublin12$price),
         mean(daftdublin20$price), mean(daftdublin22$price), mean(daftdublin24$price), mean(daftdublin6w$price),
         mean(daftdublin6$price), mean(daftdublin4$price), mean(daftdublin14$price), mean(daftdublin16$price), 
         mean(daftdublin18$price), mean(daftdublin11$price))
avg

code <- c(13, "DLR", 2, 10, 1, 15, 9, 17, 5, 3, 7, 8, 12, 20, 22, 24, "6w", 6, 4, 14, 16, 18, 11)

code <- as.character(code)

postcodes<- as.data.frame(code)
postcodes <- cbind(code, round(avg, 2))
postcodes <- as.data.frame(postcodes)
postcodes$V2 <- as.numeric(as.character(postcodes$V2))

#SPDF MAP ------------------------------------------------------------------------

install.packages("geojsonio")
install.packages("units")
library(geojsonio)
library(leaflet)
spdf <- geojson_read("dublin.geojson",  what = "sp")

m <- leaflet(spdf) %>%
  setView(-6.247101, 53.33461, 10.4) %>%
  addTiles()

bins <- c(1600, 1800, 2000, 2200, 2400, 2600, 2800, 3000, 3200, 3400, Inf)
pal <- colorBin("YlOrRd", domain = postcodes$V2, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g Avg rent cost",
  postcodes$code, postcodes$V2
) %>% lapply(htmltools::HTML)

m <- m %>% addPolygons(
  fillColor = ~pal(postcodes$V2),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"))

m <- m %>% addLegend(pal = pal, values = ~postcodes$V2, opacity = 0.7, title = NULL,
                position = "bottomright")

#----- now for median ----------
med <- c(median(daftdublin13$price), median(daftdublinco$price), median(daftdublin2$price), median(daftdublin10$price), mean(daftdublin1$price),
         median(daftdublin15$price), median(daftdublin9$price), median(daftdublin17$price), median(daftdublin5$price),
         median(daftdublin3$price), median(daftdublin7$price), median(daftdublin8$price), median(daftdublin12$price),
         median(daftdublin20$price), median(daftdublin22$price), median(daftdublin24$price), median(daftdublin6w$price),
         median(daftdublin6$price), median(daftdublin4$price), median(daftdublin14$price), median(daftdublin16$price), 
         median(daftdublin18$price), median(daftdublin11$price))
med
str(med)

code <- c(13, "DLR", 2, 10, 1, 15, 9, 17, 5, 3, 7, 8, 12, 20, 22, 24, "6w", 6, 4, 14, 16, 18, 11)

code <- as.character(code)

post<- as.data.frame(code)
post <- cbind(code, round(med, 2))
post <- as.data.frame(postcodes)
post$V2 <- as.numeric(as.character(post$V2))
str(post)

m2 <- leaflet(spdf) %>%
  setView(-6.247101, 53.33461, 10.4) %>%
  addTiles()

bins2 <- c(1600, 1800, 2000, 2200, 2400, 2600, 2800, 3000, 3200, 3400, Inf)
pal2 <- colorBin("YlGnBu", domain = postcodes$V2, bins = bins2)

labels2 <- sprintf(
  "<strong>%s</strong><br/>%g Median rent cost",
  postcodes$code, postcodes$V2
) %>% lapply(htmltools::HTML)

m2 <- m2 %>% addPolygons(
  fillColor = ~pal2(postcodes$V2),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels2,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"))

m2 <- m2 %>% addLegend(pal = pal2, values = ~post$V2, opacity = 0.7, title = NULL,
                position = "bottomright")
m2

#----- now for mode ----------
install.packages("modeest")
library(modeest)
mfv1(daftdublin16$price)

mde <- c(mfv1(daftdublin13$price), mfv1(daftdublinco$price), mfv1(daftdublin2$price), mfv1(daftdublin10$price), mfv(daftdublin1$price),
         mfv1(daftdublin15$price), mfv1(daftdublin9$price), mfv1(daftdublin17$price), mfv1(daftdublin5$price),
         mfv1(daftdublin3$price), mfv1(daftdublin7$price), mfv1(daftdublin8$price), mfv1(daftdublin12$price),
         mfv1(daftdublin20$price), mfv1(daftdublin22$price), mfv1(daftdublin24$price), mfv1(daftdublin6w$price),
         mfv1(daftdublin6$price), mfv1(daftdublin4$price), mfv1(daftdublin14$price), mfv1(daftdublin16$price), 
         mfv1(daftdublin18$price), mfv1(daftdublin11$price))
mde
str(mde)
code <- c(13, "DLR", 2, 10, 1, 15, 9, 17, 5, 3, 7, 8, 12, 20, 22, 24, "6w", 6, 4, 14, 16, 18, 11)

code <- as.character(code)

post3<- as.data.frame(code)
post3 <- cbind(code, mde)
post3 <- as.data.frame(post3)
post3$mde <- as.numeric(as.character(post3$mde))
str(post3)

m3 <- leaflet(spdf) %>%
  setView(-6.247101, 53.33461, 10.4) %>%
  addTiles()

bins3 <- c(1000, 1300, 1600, 1900, 2100, 2400, 2700, 3000, 3300, 3600, Inf)
pal3 <- colorBin("YlGn", domain = post3$mde, bins = bins3)

labels3 <- sprintf(
  "<strong>%s</strong><br/>%g Mode rent cost",
  post3$code, post3$mde
) %>% lapply(htmltools::HTML)

m3 <- m3 %>% addPolygons(
  fillColor = ~pal3(post3$mde),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels3,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"))

m3 <- m3 %>% addLegend(pal = pal3, values = ~post3$mde, opacity = 0.7, title = NULL,
                       position = "bottomright")
m3

m
m2
m3

#---------------------------



register_google(key = "AIzaSyAbwQLm6q3JtuqRm8V9FQCldP_PeatOsF8")  
#google api key

# Get city Coordinates --------------------------------
dublin <-  geocode("dublin")  # get longitude and latitude

# Get the Map ----------------------------------------------
  
# Google Road Map
dublin_map <- qmap("dublin", zoom=10, source = "google", maptype="roadmap")

# Plot Daft $ Airbnb Map -------------------------------------
dublin_map + geom_point(aes(x=daft.longitude, y=daft.latitude),
                                  data = x,
                                  alpha = 4/10, 
                                  size = 1, 
                                  color = "red")+ 
  geom_point(aes(x=air.longitude, y=air.latitude),
             data = y,
             alpha = 1/10, 
             size = 1, 
             color = "blue")

# Plot Daft solo Map -------------------------------------

dub <- dublin_map + geom_point(aes(x=daft$longitude, y=daft$latitude),
                               data = daft,
                               alpha = 0.5, 
                               size = 1.5,
                               color = daft$range)

#legend("bottomright", legend = levels(daft$range))

dub


b <- c(-Inf, 1000, 2000, Inf)
names <- c("blue", "orange", "red")
daft$range <- cut(daft$price, breaks = b, labels = names)

f <- c(-Inf, 1000, 2000, Inf)
fnames <- c(2, 4, 6)
daft$size <- cut(daft$price, breaks = b, labels = names)
daft$size <- as.numeric(daft$size)

#dwelling type map

dwel <- dublin_map + geom_point(aes(x=daft$longitude, y=daft$latitude),
                               data = daft,
                               alpha = 0.5, 
                               size = 2.5,
                               color = daft$dwel,
                               legend = TRUE)

?legend

dwel

levels(daft$dwelling)

daft$dwel <- "orange"

daft[daft$dwelling %in% c('House to Rent'),]$dwel <- "purple"
daft[daft$dwelling %in% c('Flat to Rent'),]$dwel <- "blue"
daft[daft$dwelling %in% c('Studio apartment to Rent'),]$dwel <- "red"


b <- c(-Inf, 1000, 2000, Inf)
names <- c("blue", "orange", "red")
daft$range <- cut(daft$price, breaks = b, labels = names)

f <- c(-Inf, 1000, 2000, Inf)
fnames <- c(2, 4, 6)
daft$size <- cut(daft$price, breaks = b, labels = names)
daft$size <- as.numeric(daft$size)






# Plot Airbnb solo Map -------------------------------------
dublin_map + geom_point(aes(x=air.longitude, y=air.latitude),
                      data = y,
                      alpha = 4/10, 
                      size = 1, 
                      color = "blue")



#mapbox and plotly maps.. not working

library(plotly)

dat <- map_data("world", "ireland") %>% group_by(group)
p <- plot_mapbox(dat, x = ~long, y = ~lat) %>%
  add_paths(size = I(2)) %>%
  layout(mapbox = list(zoom = 8.5,
                       center = list(lat = 53.31746,
                                     lon =-6.257807)
  ))
p


#-6.257807
#53.31746

dat <- map_data("world", "ireland") %>% group_by(group)
p <- daft %>% plot_mapbox(x = daft$latitude, y = daft$longitude,
                          split = ~price,
                          mode = 'scattermapbox', hoverinfo='name') %>%
  add_paths(size = I(2)) %>%
  layout(font = list(color='white'),
         plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
          mapbox = list(style = "dark", 
                        zoom = 8.5,
                        center = list(lat = 53.31746,
                                     lon =-6.257807)))
p

Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1Ijoibmlsb2siLCJhIjoiY2p1enJxYTBuMDd3YzN6cWxzeGpzdHVqMiJ9.pW9yCN4OK49bwUZXK9Ku5Q')

library(plotly)

df = read.csv('https://raw.githubusercontent.com/bcdunbar/datasets/master/meteorites_subset.csv')

p <- df %>%
  plot_mapbox(
              split = ~class, size=2,
              mode = 'scattermapbox', hoverinfo='name') %>%
  layout(title = 'Meteorites by Class',
         font = list(color='white'),
         plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
         mapbox = list(style = 'dark'),
         legend = list(orientation = 'h',
                       font = list(size = 8)),
         margin = list(l = 25, r = 25,
                       b = 25, t = 25,
                       pad = 2))
p
















