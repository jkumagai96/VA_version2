
# file.edit("VAShiny/dependenciesfinal.R")
# source("VAShiny/dependencies.R")

#Space - Choose where to zoom - country selection; regional selection, global view (no selection)
#Indicator
#TempRes
#Textinput - function TBA


library(shiny)
# library(leaflet.minicharts)
library(leaflet)
library(leafpop)
# library(ggplot2)
# library(ggiraph)



ui <- fluidPage(
    
    
    # App title ----
    titlePanel("Valuation Atlas"),
    
    # Sidebar layout with input and output definitions ----
    # sidebarLayout(
        # Sidebar panel for inputs ----
        
    column(width = 2,
           
           # Upper panel for choices ----    
           
    # Map
    conditionalPanel(
        condition = "input.inTabset == 'Map'",
        {fluidRow(style="background-color:#e8e8e8; padding:10px; border-radius:10px",
                        tags$b("View Map"), br(), br(), 
                        
                        # Add Reset view button
                        # Add options to limit map in box
                        
                        # Choose space
                        selectInput(inputId = "Space", label="Choose where you want to go", choices = c("Country", "Region", "Global"), selected = "Global"), # Make zoom
        
                        #Make second choices pop up based on first choice in Space
                        #Country selection
                        conditionalPanel(condition='input.Space=="Country"',
                                                                selectInput(inputId="Country",
                                                                            label="Select country",
                                                                            choices=c("",poly$NAME_0)) 
                                                            ),
                        #Region selection                                    
                        conditionalPanel(condition='input.Space=="Region"',
                                                                             selectInput(inputId="Region",
                                                                                         label="Select region",
                                                                                         choices=c("", "Region1", "Region2"))
                                                            ),
                        
                        
                        
                        # radioButtons(inputId = "Indicator", label="Choose the dataset", choices = c("Density of Studies", "Density of Organizations", "Ratio of Studies and Organizations"), selected = "Density of Studies"),
                        radioButtons(inputId = "Indicator", label="Choose the dataset", choiceNames = choiceNames, choiceValues = choiceValues),
                        
                        radioButtons(inputId = "TempRes", label="Choose temporal resolution", choices = c("Total", "Before 2010", "After 2010"), selected = "Total"), #
                        textInput(inputId = "Search", label="")
                        
                        
                        
                        )}   
    ),
    
    #Table
    conditionalPanel(
        condition = "input.inTabset == 'Table'",
        {fluidRow(style="background-color:#e8e8e8; padding:10px; border-radius:10px",
                  tags$b("View data"), br(), br(),
                  
                  #Country selection
                  
                    # tags$b("Select a country to view data"), br(), br(),
                                   selectInput(inputId="Country",
                                               label="Select country",
                                               choices=c("",poly$NAME_0))
         )}   
        
    ),
    
    
    #Compare
    conditionalPanel(
        condition = "input.inTabset == 'Compare'",
        {fluidRow(style="background-color:#e8e8e8; padding:10px; border-radius:10px",
                  tags$b("View comparison"), br(), br(),
                  
                  #Country selection
                  
                  selectInput(inputId="Country1",
                              label="1st country",
                              choices=c("",poly$NAME_0)),
                  
                  selectInput(inputId="Country1",
                              label="2nd country",
                              choices=c("",poly$NAME_0))
                  
                  
                  
                  
        )}   
        
    ),
    
       
    
    tags$footer(style = "font-size:8px;text-align:justify;position:fixed;bottom:0%;width:14%;",
        
                HTML('
                        <i>    
                            The designations employed and the presentation of material 
                            on the maps shown here do not imply the expression of any 
                            opinion whatsoever on the part of the IPBES concerning the 
                            legal status of any country, territory, city or area or of 
                            its authorities, or concerning the delimitation of its frontiers or boundaries. 
                        </i>
                        <br><br>
                             <center><img src="IPBES_LOGO.jpg" width="40%"></center>
                        <br><br>
                     '),
            
                                               
                ),    
           
            # Lower panel for information ----
            # Disclaimer
            # fluidRow(
            #     br(),br(), # add two line breaks
            #     em("The designations employed and the presentation of material on the maps shown here do not imply the expression of any opinion whatsoever on the part of the IPBES concerning the legal status of any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers or boundaries."),
            #     br(),br(),br(),br(),
            # 
            # # IPBES logo
            #     # tags$img(src="IPBES_LOGO.jpg", align="center", height="60%", width="60%") # center doesnt work
            #     HTML('<center><img src="IPBES_LOGO.jpg" width="60%"></center>')
            #     
            #     
            #     )      
                     
                        
        ), # sidebar panel end
        
        # fluidRow(renderImage("Data/IPBES_LOGO.jpg") # add ipbes logo here 
        # 
        # ),
        
        # Main panel for map ----
        column(width = 10,
               
               tabsetPanel(id="inTabset",
                  tabPanel("Map", leafletOutput("map", height="80vh") ),
                  tabPanel("Table", tableOutput("table")),
                  tabPanel("Compare")
        )#end tabset panel
                  )
    # )
)

server <- function(input, output, session) {
    
    
    # fpdata <- reactive({ #filtered data for country selection and popup table
    #     pdata[poly$NAME_0 == input$Country,]
    # })
    
    fpdata1 <- reactive({ #filtered data for country selection and popup table
        if(exists("input$Country")==F){return(pdata)}else{return(pdata[poly$NAME_0 == input$Country,])}
    })
    
    
    # Map   
    {
    output$map <- renderLeaflet({my_map})
    
    # This reactive expression for color palette
    # palette <- reactive({
    #     colorNumeric(
    #         palette = pal[[input$Indicator]],
    #         domain = poly[[input$Indicator]]
    #     )
    # })
    # 
    

    
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
        poly[input$Indicator]
    })
    
    # This reactive expression for color palette
    # colorpal <- reactive({
    #     pal[[input$Indicator]]
    # })
    
    
    observe({
        palette <- colorBin(bins=10, pretty=T,
                            palette = pal[[input$Indicator]],
                            domain = poly[[input$Indicator]]
                            )
        
        leafletProxy("map", data = filteredData()) %>%
            clearShapes() %>% clearControls() %>%
            addPolygons(stroke=F, smoothFactor = 0.2, 
                        color = ~palette(poly[[input$Indicator]]), label=poly$NAME_0, # data shown on hover
                        weight = 1, popup=popupTable(pdata, row.numbers = F, feature.id = F), #map(1:length(names(poly)), ~poly[[.x]])
                        fillOpacity = 0.7)  %>%
            addScaleBar(position = "bottomleft", options = scaleBarOptions()) %>%
            addLegend("topright", title=choiceNames[grep(paste0(input$Indicator,"$"), choiceValues)], pal=palette, values=poly[[input$Indicator]], na.label = "Missing", opacity=1)
            labelFormat(suffix=" ")
        
    })
    
    
    # Country selection zoom
    fpts <- reactive({ #filtered points
        coordpts[poly$NAME_0 == input$Country,]
    })

    fpdata <- reactive({ #filtered data for popup table
        pdata[poly$NAME_0 == input$Country,]
    })


    observe({
        req(input$Country)
        ctryzoom <- fpts()
        leafletProxy("map") %>%
            clearMarkers() %>%  # addMarkers(lng = ctryzoom$Lon, lat = ctryzoom$Lat, popup=popupTable(pdata, row.numbers = F, feature.id = F)) %>% 
            setView(lng = ctryzoom$Lon, lat = ctryzoom$Lat, zoom = 6)%>%
            addPopups(lng = ctryzoom$Lon, lat = ctryzoom$Lat, popup=popupTable(fpdata(), row.numbers = F, feature.id = F))
             
    })


    
    #Add polygons
    # observeEvent(input$Indicator, {
    #     leafletProxy("map", filteredData()) %>%
    #         addPolygons(fillColor = colorpal)
    # })
    
    # Add polygons
    # observe({
    #     # pal <- colorpal()
    #     
    #     leafletProxy("map", data = filteredData()) %>%
    #         clearShapes() %>%
    #         addPolygons(fillColor = colorpal)
    # })
    }
    
    
    # input$tableId_rows_selected <- reactive(input$Country)
    # https://stackoverflow.com/questions/21515800/subset-a-data-frame-based-on-user-input-shiny
    
    # Table
    output$table <- renderTable(fpdata1())
    
    
    # if(exists("input$Country")){return(pdata)}else{pdata[poly$NAME_0 == input$Country,]}
    
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

# view selectinput values
# https://stackoverflow.com/questions/22423363/r-shiny-access-input-fields-in-ui


# https://rstudio.github.io/leaflet/shiny.html


#https://community.rstudio.com/t/make-shiny-leaflet-map-less-cumbersome-faster/210/8

# https://datascience.blog.wzb.eu/2021/04/16/interactive-visualization-of-geospatial-data-with-r-shiny/

# dateline issue
# https://github.com/rstudio/leaflet/issues/729
