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
        absolutePanel(id = "controls", fixed = TRUE,
                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                      width = 330, height = "auto",
                      
                      h2("Paper parks? Deforestation in Colombia"),
                      h4("Select one protected area:"),
                      selectInput(inputId = "park", label = "", choices = c(as.character(natural_parks[[1]]$NAME))),
                      plotOutput("rdplot", width = 300, height = 300),
                      textOutput("message")
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


##################################################### SERVER (SERVER) ####################################################
#Load data for Leaflet maps
defo_dist <- readRDS("data/defo_dist.rds")
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
     
     #Individual graphs for all territories (natural parks + territories)
     #Data subset by input
     data <- reactive({defo_dist %>% subset(defo_dist$buffer_name == input$park) %>%
         mutate(., bin = cut(.$dist_disc, breaks = c(-50:50), include.lowest = T)) %>%
         group_by(bin) %>%
         summarize(meanbin = mean(loss_sum), sdbin = sd(loss_sum), n = length(ID)) %>%
         .[complete.cases(.),] %>%
         as.data.frame() %>%
         mutate(treatment = ifelse(as.numeric(row.names(.)) > 50, 1, 0), bins = row.names(.)) %>%
         mutate(bins = mapvalues(.$bins, from = c(1:100), to = c(-50:49)))
     })
     
     observeEvent(input$park, {
       if(dim(data())[1] == 0){
         renderText({
           "There are no effective controls or treatments "
         })
       } else {
         output$rdplot <-
           renderPlot({
             g <- ggplot(data(), aes(y = (meanbin), x = as.numeric(bins), colour = as.factor(treatment)))
             g <- g + stat_smooth(method = "auto")
             g <- g + geom_point(colour = "black", size = 1)
             g <- g + labs(x = "Distance (km)", y = "Deforestation (Ha x km^2)")
             # g <- g + scale_x_continuous(limits = c(-20, 20))
             # g <- g + scale_y_continuous(limits = c(0, 0.3))
             # # g <- g + ggtitle(str_c("Discontinuidad\n", "para", type, sep = " "))
             g <- g + guides(colour = FALSE)
             # g <- g + theme_bw()
             g
             # # ggsave(str_c("RDggplot_", type, "strategy2",".pdf"), width=30, height=20, units="cm")
             # # }, x = l, type = c("Áreas protegidas nacionales","Áreas protegidas regionales","Resguardos indígenas", "Comunidades negras"))
           })
       }
     })
 }

shinyApp(ui = ui, server = server)


