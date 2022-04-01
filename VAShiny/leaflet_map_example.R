#leaflet map example
#need to run dependencies_leaflet.R first
#Names1 ~ Density of Studies

input <- list()
input$Indicator <- "Names1"

map <- leaflet(poly) %>% addTiles()

palette <- colorNumeric(
  palette = pal[[input$Indicator]],
  domain = poly[[input$Indicator]]
)


map %>%
  addPolygons(stroke=F, smoothFactor = 0.2, 
              color = ~palette(poly[[input$Indicator]]),
              weight = 1, popup=poly@data,
              fillOpacity = 0.7)  %>%
  
  addScaleBar(position = "bottomleft", options = scaleBarOptions()) %>%
  addLegend("topright", title=choiceNames[grep(paste0(input$Indicator,"$"), choiceValues)], pal=palette, values=poly[[input$Indicator]]) 

