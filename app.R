library(shiny)
library(leaflet)
library(leaflet.extras)

# Define UI for the map
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%")
)

# Set up map, layers, source data, and styles
server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$Stamen.Toner) %>%
      setView(-86.82, 33.09, zoom = 7) %>% #33.0954169,-86.8259659
      
      addLayersControl(
        overlayGroups = c("AM", "FM"),
        position = c("topleft"),
        options = layersControlOptions(collapsed = TRUE))
  })
  
  stations<-readRDS("data/radio-aff.RDS")
  am<-stations[grep("AM", stations$Frequency), ]
  fm<-stations[grep("FM", stations$Frequency), ]
  
  au.icon<-makeIcon(
    iconUrl = "data:image/png;base64,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")
  
  observe({
      leafletProxy("mymap") %>%
        addMarkers(as.numeric(fm$Lon),
                         as.numeric(fm$Lat),
                         icon = au.icon,
                         #radius = 53327,
                         group = "FM",
                         popup = paste("<b>City:</b>", fm$City, "<br/>", "<br/>",
                                       "<b>Call Letters</b>: ", fm$`Call Letters`, "<br/>", "<br/>",
                                       "<b>Frequency</b>: ", fm$Frequency, "<br/>", "<br/>",
                                       "<b>Auburn Sports Today</b>: ", fm$`Auburn Sports Today`,"<br/>", "<br/>",
                                       "<b>Tiger Talk</b>: ", fm$`Tiger Talk`,"<br/>", "<br/>",
                                       "<b>Football</b>: ", fm$Football,"<br/>", "<br/>",
                                       "<b>Mens Basketball</b>: ", fm$`Mens Basketball`,"<br/>", "<br/>",
                                       "<b>Baseball</b>: ", fm$Baseball,"<br/>", "<br/>")) %>%
        addMarkers(as.numeric(am$Lon), 
                         as.numeric(am$Lat), 
                         icon = au.icon,
                         #radius = 128748,
                         group = "AM",
                         popup = paste("<b>City:</b>", am$City, "<br/>", "<br/>",
                                       "<b>Call Letters</b>: ", am$`Call Letters`, "<br/>", "<br/>",
                                       "<b>Frequency</b>: ", am$Frequency, "<br/>", "<br/>",
                                       "<b>Auburn Sports Today</b>: ", am$`Auburn Sports Today`,"<br/>", "<br/>",
                                       "<b>Tiger Talk</b>: ", am$`Tiger Talk`,"<br/>", "<br/>",
                                       "<b>Football</b>: ", am$Football,"<br/>", "<br/>",
                                       "<b>Mens Basketball</b>: ", am$`Mens Basketball`,"<br/>", "<br/>",
                                       "<b>Baseball</b>: ", am$Baseball,"<br/>", "<br/>"))

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

