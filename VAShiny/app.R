
# file.edit("VAShiny/dependenciesfinal.R")
# source("VAShiny/dependencies.R")

#Space - Choose where to zoom - country selection; regional selection, global view (no selection)
#Indicator
#TempRes
#Textinput - function TBA


library(shiny)
# library(leaflet.minicharts)
# library(leaflet)
# library(ggplot2)
# library(ggiraph)



ui <- fluidPage(
    
    
    # App title ----
    titlePanel("Valuation Atlas"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(width = 3,
            # Upper panel for choices ----      
            fluidRow(
                selectInput(inputId = "Space", label="Choose where you want to go", choices = c("Country", "Region", "Global"), selected = "Global"), # Make zoom
                radioButtons(inputId = "Indicator", label="Choose the dataset", choices = c("Density of Studies", "Density of Organizations", "Ratio of Studies and Organizations"), selected = "Density of Studies"),
                radioButtons(inputId = "TempRes", label="Choose temporal resolution", choices = c("Total", "Before 2010", "After 2010"), selected = "Total"), #
                textInput(inputId = "Search", label="")
                
                
                
            ),
            
            # Lower panel for information ----

            fluidRow(
                br(),br(), # add two line breaks
                em("The designations employed and the presentation of material on the maps shown here do not imply the expression of any opinion whatsoever on the part of the IPBES concerning the legal status of any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers or boundaries.")),
            # renderImage("IPBES_LOGO.png") # add ipbes logo here 
                     
                     
                        
        ), # sidebar panel end
        
        # Main panel for map ----
        mainPanel(width=9,
                  # "This is where the map will be", 
                 
                  # print(X)
                  leafletOutput("map", height="90vh") #vh means viewing height
                  
                  # ,
                  # 
                  # leafletOutput("mapfill", height="90vh") #vh means viewing height
                  # 
                  
                  # leafletOutput(c("map", "mapfill"), height="90vh") #vh means viewing height
                  
                  )#main panel end
    )
)

server <- function(input, output, session) {

    output$map = renderTmap({
        # tmap_mode("plot")
        
        #original: problem - when changing indicator
        # map <- tm_shape(poly) + tm_polygons(
        #                                     col=input$Indicator,
        #                                     palette=pal[[input$Indicator]],
        #                                     id="Country Name", border.alpha = 1,
        #                                     popup.vars=grep("log|geometry|Country Name", names(poly), invert=T, value=T)) # the second one needs to be separated somehow or else a new selection reloads the whole map
        #     
        
        #possible solution 1: 
        # tm_shape(poly) + tm_borders()
    
        # tm_shape(poly) + tm_polygons( # set first choice here
        #                                     col="Density of Studies",
        #                                     palette=pal[["Density of Studies"]],
        #                                     id="Country Name", border.alpha = 1, zindex=401,
        #                                     popup.vars=grep("log|geometry|Country Name", names(poly), invert=T, value=T)) # the second one needs to be separated somehow or else a new selection reloads the whole map
        # tm_basemap("Stamen.Watercolor")
        
        tm_shape(poly) + tm_view(set.zoom.limits=c(2,7)) + tm_borders()# this one needs to be separated somehow or else a new selection reloads the whole map
        
        
        
                                # +tm_polygons(...) # separate tm shape and tm polygons
        # tmap_leaflet(map)
        
        
        
        
        # tmap_leaflet(tm_shape(poly) + tm_fill())
        
        
    })
    
    observe({
        # pop <- input$Indicator
        tmapProxy("map", session, {
            tm_remove_layer(401)
            # tm_shape(poly) + tm_polygons(col=input$Indicator,
            #                              palette=pal[[input$Indicator]], n=10,
            #                              id="Country Name", border.alpha = 1, zindex=401,
            #                              popup.vars=grep("log|geometry|Country Name", names(poly), invert=T, value=T))
            
            # tm_borders(poly)+
                # tm_view(set.zoom.limits=c(2,7)) + 
              tm_shape(poly)+      tm_fill(col=input$Indicator,
                                                                             palette=pal[[input$Indicator]], n=10,
                                                                             id="Country Name", border.alpha = 1, zindex=401,
                                                                             popup.vars=grep("log|geometry|Country Name", names(poly), invert=T, value=T))
        })
    })
    
    #possible solution 2: seperate objects
    # output$mapfill = tm_polygons(
    #                                     col=input$Indicator,
    #                                     palette=pal[[input$Indicator]],
    #                                     id="Country Name", border.alpha = 1,
    #                                     popup.vars=grep("log|geometry|Country Name", names(poly), invert=T, value=T)) # the second one needs to be separated somehow or else a new selection reloads the whole map


    
    
}

shinyApp(ui = ui, server = server)


#helpful links (possibly)
# https://stackoverflow.com/questions/41940403/popup-on-a-shape-using-tmap
# https://community.rstudio.com/t/integrating-tmap-into-shiny/6036
# https://community.rstudio.com/t/how-to-plot-leaflet-map-in-shiny-mainpanel/107079
# https://geocompr.robinlovelace.net/adv-map.html#interactive-maps


# https://community.rstudio.com/t/select-polygon-by-clicking-on-map-changing-item-selected-via-dropdown/89211/2

# https://medium.com/ibm-data-ai/asynchronous-loading-of-leaflet-layer-groups-afc073999e77


# https://stackoverflow.com/questions/30091093/is-it-possible-to-update-polygon-fill-in-leaflet-for-shiny-without-recreating-th
    

# https://github.com/cenuno/shiny/blob/master/Interactive_UI/Dynamic_Legend/server.R#L25


# https://r-tmap.github.io/tmap-book/layers.html

