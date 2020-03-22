#Rshiny tut
#https://medium.com/@joyplumeri/how-to-make-interactive-maps-in-r-shiny-brief-tutorial-c2e1ef0447da

#load libraries
install.packages("shiny")
install.packages("leaflet")
install.packages("leaflet.extras")
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
#import data
data <- daft_city

#ui code -------------------------
ui <- fluidPage(
  mainPanel( 
    #this will create a space for us to display our map
    leafletOutput(outputId = "mymap"), 
    #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
    absolutePanel(top = 60, left = 20, 
                  checkboxInput("markers", "Depth", FALSE),
                  checkboxInput("heat", "Heatmap", FALSE)
    )
  ))

#server code ----------------------
server <- function(input, output, session) {
  #define the color pallate for the magnitidue of the earthquake
  pal <- colorNumeric(
    palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
    domain = data$price)
  
  #define the color of for the depth of the earquakes
  pal2 <- colorFactor(
    palette = c('blue', 'yellow', 'red', 'green'),
    domain = data$dwelling
  )
  
  #create the map
  output$mymap <- renderLeaflet({
    leaflet(data) %>% 
      setView(lng = -6.094057, lat = 53.37188, zoom = 10)  %>% #setting the view over ~ center of North America
      addTiles() %>% 
      addCircles(data = data, lat = ~ latitude, lng = ~ longitude, weight = 1,
                 radius = 5, popup = ~as.character(price), 
                 label = ~as.character(paste0("Price: ", sep = " ", price)),
                 color = ~pal(price), fillOpacity = 0.5)
  })
  
  #next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.
  observe({
    proxy <- leafletProxy("mymap", data = data)
    proxy %>% clearMarkers()
    if (input$markers) {
      proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(dwelling), fillOpacity = 0.2, label = ~as.character(paste0("Magnitude: ", sep = " ", price))) %>%
        addLegend("bottomright", pal = pal2, values = data$dwelling,
                  title = "Depth Type",
                  opacity = 1)}
    else {
      proxy %>% clearMarkers() %>% clearControls()
    }
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = data)
    proxy %>% clearMarkers()
    if (input$heat) {
      proxy %>%  addHeatmap(lng=~longitude, lat=~latitude, intensity = ~price, blur =  1, max = 0.05, radius = 1) 
    }
    else{
      proxy %>% clearHeatmap()
    }
  })
  
}


shinyApp(ui, server)

