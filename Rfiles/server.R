library(shiny)
library(leaflet)
library(tidyverse)
library(plotly)


server <- function(input, output, session) {

  n <- eventReactive(input$recalc, {
    input$slider2
  })
  
  points <- eventReactive(input$recalc, {
    
    df <- data.frame(lat = runif(input$slider2, min = 55.95, max = 55.96), lng = runif(input$slider2, min = -3.19, max = -3.15))
    
  }, ignoreNULL = FALSE)
  
  
  price <- eventReactive(input$recalc, {
        
        (round(runif(input$slider2, min = input$slider1[1], max = input$slider1[2] ), digits = 2))
  }, ignoreNULL = FALSE)
  
  #here we receive the map style selections
  map_style <- reactive({input$radio})


  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(map_style(),options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = daft, clusterOptions = markerClusterOptions(), 
                 popup = paste0( as.character(daft$addr), "<br>",as.character(daft_city$postcode),"<br>",as.character(daft$dwelling), "<br>","€", as.character(daft$price), "<br>"))
    
  })
  
  
}
#run
shinyApp(ui, server)
