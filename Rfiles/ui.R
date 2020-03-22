library(shiny)
library(leaflet)
library(tidyverse)
library(plotly)


ui <- fluidPage(
  
  titlePanel("Interactive property map"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons("radio", label = h3("Choose Map Style"),
                   choices = list("Stamen.TonerLite", "OpenStreetMap.DE", "OpenStreetMap.HOT", "OpenStreetMap.Mapnik"), 
                   selected = "Stamen.TonerLite"),
      conditionalPanel(
        condition = "input.toggle % 2 == 1",
        p("Chose one of three different styles of maps"),
        p("Your selection will be applied instantly to the map on the main panel, but might take a couple of seconds to fully load"),
        p("If you can only see the markers and not the map, please zoom out to allow for some time to load the map")
      )
    ),
    mainPanel(
      leafletOutput("mymap")
    )
  
)
)
