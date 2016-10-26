################################################ SHINY WEB APP ###########################################################
library(shiny)
library(rsconnect)
library(leaflet)
library(plyr)
library(dplyr)
library(htmltools)
library(raster)
library(rgdal)
library(magrittr)
library(sp)
library(ggplot2)
library(rgeos)
library(maptools)
library(dplyr)
library(broom)
library(stringr)

################################################## USER INTERFACE (UI) ###################################################
ui <- navbarPage(title = "Deforestation",theme="http://bootswatch.com/spacelab/bootstrap.css", inverse=TRUE,
  tabPanel("Analisis",
    tags$style(type="text/css", "html, body {width:100%;height:100%}"),
    div(class="outer",
        tags$head(includeCSS("www/style.css")),
        leafletOutput("mymap", width="100%", height="100%"),
        absolutePanel(top=20, left=60, height=20, width=600, h4("Paper parks? Deforestation in Colombia")),
        absolutePanel(top=10, right=10,
                      selectInput(inputId = "park", label = "", choices = c(as.character(natural_parks[[1]]$NAME)))
                  )
     )
  ),
  tabPanel("About",
           p("This proyect is an extension of the working paper Bonilla & Higuera (2016) which assesses 
         the effects of protected areas in Colombia using high-resolution forest loss imagery for the period 2000-2012.
         This demo will only use natural protected areas"),
                 br(),
                 br(),
                 br(),
                 br(),
                 "This proyect is under development. The authors are grateful of Colombia's Central Bank (Banco de la República)
                 support.",
                 br(),
                 br(),
                 HTML('<center><img src="banrep_logo.png" height="72" width="72"/></center>')
                # img(src = "banrep_logo.png", height = 72, width = 72)
           )
)

  
  
#   fluidPage(
#   titlePanel("Deforestation in Colombia: evidence from high-resolution satellite imagery"),
#   sidebarLayout(
#     sidebarPanel(
#       h2("Intro"),
#       p("This proyect is an extension of the working paper Bonilla & Higuera (2016) which assesses 
#         the effects of protected areas in Colombia using high-resolution forest loss imagery for the period 2000-2012.
#         This demo will only use natural protected areas"),
#       br(),
#       br(),
#       br(),
#       br(),
#       "This proyect is under development. The authors are grateful of Colombia's Central Bank (Banco de la República)
#       support.",
#       br(),
#       br(),
#       HTML('<center><img src="banrep_logo.png" height="72" width="72"/></center>')
#        # img(src = "banrep_logo.png", height = 72, width = 72)
#     ),
#     mainPanel(
#       h3("Measuring the impact of protected areas: an RD approach"),
#       br(),
#       p("This Web aplication allows to assess the effect of legal protection on deforestation for individual parks in Colombia.
#         Please select the National or Regional Protected area."),
#       
#       selectInput(inputId = "park", label = "", choices = c(as.character(natural_parks[[1]]$NAME))),
#       h2("Natural protected areas in Colombia"),
#       leafletOutput("mymap")
#     )
#   )
# )
  


##################################################### SERVER (SERVER) ####################################################
#Load data for Leaflet maps
natural_parks <- readRDS("data/natural_parks.rds")
natural_parks[[1]]$TYPE <- ifelse(natural_parks[[1]]$DESIG %in% c("Distritos De Conservacion De Suelos",
              "Distritos Regionales De Manejo Integrado",
              "Parque Natural Regional",
              "Reservas Forestales Protectoras Regionales",
              " A\u0081reas De Recreacion",
              "Reserva Forestal Protectora Nacional"), "Regional", "National")


# Create a pop-up:
popup <- paste0("<b>Nombre: </b>", natural_parks[[1]]@data$NAME, 
                "<b><br />Tipo: </b>", natural_parks[[1]]@data$DESIG, 
                "<b><br />Año </b>: ", natural_parks[[1]]@data$STATUS_YR)

# popup_select <- paste0("<b>Nombre: </b>", natural_parks[[1]]@data$NAME[natural_parks[[1]]@data$NAME == input$park], 
#                        "<b><br />Tipo: </b>", natural_parks[[1]]@data$DESIG[natural_parks[[1]]@data$DESIG == input$park], 
#                        "<b><br />Año </b>: ", natural_parks[[1]]@data$STATUS_YR[natural_parks[[1]]@data$STATUS_YR == input$park])
# 


pal <- colorFactor(
  palette = topo.colors(2),
  domain = natural_parks[[1]]$TYPE
)


server <- function(input, output, session){
  data <- reactive({
    natural_parks[[1]][natural_parks[[1]]$NAME == input$park, ]
  })
  
  ##################################################################################################################
  ################################################### MAP IN LEAFLET ###############################################
  ##################################################################################################################

  output$mymap <- renderLeaflet({ 
    leaflet() %>% addTiles() %>% addPolygons(data = natural_parks[[1]], popup = popup,
                                             layerId = as.character(natural_parks[[1]]$NAME),
                                             color = pal(natural_parks[[1]]$TYPE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addLegend(position="bottomleft", pal = pal, values = natural_parks[[1]]$TYPE, labels=c("National", "Regional"))
  })
  

     observeEvent(input$mymap_shape_click, { # update the location selectInput on map clicks
       p <- input$mymap_shape_click
       if(!is.null(p$id)){
         if(is.null(input$park) || input$park != p$id) updateSelectInput(session, "park", selected = p$id)
       }
     })
     
     observeEvent(input$mymap_shape_click, { # update the map view on map clicks
       p <- input$mymap_shape_click
       proxy <- leafletProxy("mymap")
       if(is.null(p$id)){
         proxy
       } else {
         proxy %>% setView(lng=p$lng, lat=p$lat, input$mymap_zoom) %>% removeMarker(layerId="NAME")
       }
     })
     
     observeEvent(input$park, { # update the map view on location selectInput changes
       p <- input$mymap_shape_click
       p2 <- gCentroid(natural_parks[[1]][natural_parks[[1]]$NAME == input$park, ]) %>% coordinates()
       proxy <- leafletProxy("mymap")
       if(length(p$id) && input$park != p$id){
         proxy %>% clearPopups() %>% setView(lng = p2[1], lat = p2[2] , input$mymap_zoom) %>% addMarkers(lng = p2[1], lat = p2[2], layerId = "NAME")
       }
     })
     
     ############################################################################################################### 
     ################################################# GRAPHS AND DATA #############################################
     ###############################################################################################################
     

   

}

shinyApp(ui = ui, server = server)


