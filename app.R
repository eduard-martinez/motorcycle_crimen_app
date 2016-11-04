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
library(raster)
################################################## USER INTERFACE (UI) ###################################################
load("app_data.RData")
r <- raster("loss_year_reclassify_1km.tif")

ui <- navbarPage(title = "Deforestation",theme="http://bootswatch.com/spacelab/bootstrap.css", inverse=TRUE,
  tabPanel("Map",
           withMathJax(),
    tags$style(type="text/css", "html, body {width:100%;height:100%}"),
    div(class="outer",
        tags$head(includeCSS("www/style.css"), tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
                                                          tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]}});", type='text/x-mathjax-config')),
        leafletOutput("mymap", width="100%", height="100%"),
        absolutePanel(id = "controls", fixed = TRUE,
                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                      width = 330, height = "auto",
                      h2("Paper parks? Deforestation in Colombia"),
                      h4("Select one protected area:"),
                      selectInput(inputId = "park", label = "", choices = c("", as.character(natural_parks[[1]]$NAME)), selected = ""),
                      plotOutput("rdplot", width = 300, height = 300)
                      ),
        absolutePanel(id = "estimate", fixed = TRUE,
                      draggable = TRUE, top = 100, left = 40, right = "auto", bottom = "auto",
                      width = 280, height = "auto",
                      conditionalPanel(
                        condition = "input.park != '' ",
                        h5("Regression discontinuity estimator", align = "center"),
                        h6("Annual average effects", align = "center"),
                        plotOutput("rdestimate", width = 260, height = 260),
                        htmlOutput("avoided1")
                      )
        )
     )
  ),
  tabPanel("Description",
           # includeHTML("text/Untitled.html")
           p("NULL")
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
# r <- raster("data/loss_year_reclassify_1km.tif")
# defo_dist <- readRDS("data/defo_dist.rds")
# natural_parks <- readRDS("data/natural_parks.rds")
natural_parks[[1]]$TYPE <- ifelse(natural_parks[[1]]$DESIG %in% c("Distritos De Conservacion De Suelos",
              "Distritos Regionales De Manejo Integrado",
              "Parque Natural Regional",
              "Reservas Forestales Protectoras Regionales",
              " A\u0081reas De Recreacion",
              "Reserva Forestal Protectora Nacional"), "Regional", "National")



# Create a pop-up:
popup <- paste0("<center><b>", natural_parks[[1]]@data$NAME, "</b></center>",
                "<center><i>Type: ", natural_parks[[1]]@data$DESIG, "</i></center>",
                "<b><br />Creation year: </b> ", natural_parks[[1]]@data$STATUS_YR,
                "<b><br />Annual deforestation: </b>", round(natural_parks[[1]]@data$defo_total, 3), " ha.",
                "<b><br />Annual deforestation rate: </b>", round(natural_parks[[1]]@data$defo_total_pixel, 3), " ha/km$^2$")


pal_raster <- colorFactor(palette =  c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c"), 
                          domain = c(1:4),
                          na.color = "#00000000"
                          )


pal <- colorFactor(
  palette = c("#31a354", "#e5f5f9"),
  domain = natural_parks[[1]]$TYPE
)


server <- function(input, output, session){
  
  ##################################################################################################################
  ################################################### MAP IN LEAFLET ###############################################
  ##################################################################################################################

  output$mymap <- renderLeaflet({ 
    leaflet() %>% addTiles() %>% addPolygons(data = natural_parks[[1]], popup = popup,
                                             layerId = as.character(natural_parks[[1]]$NAME),
                                             color = pal(natural_parks[[1]]$TYPE), weight = 2,
                                             opacity = 0.8) %>%
      addProviderTiles("CartoDB.Positron") %>% addRasterImage(r, colors = pal_raster, opacity = 0.8, project = FALSE) %>% 
      addLegend(position="bottomleft", pal = pal, values = natural_parks[[1]]$TYPE, labels=c("National", "Regional"), title = "Protected area jusrisdiction") %>%
      addLegend(position="bottomleft", colors =  c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c"), values = c(1, 2, 3, 4), 
                labels = c("20-40", "40-60", "60-80", "80-100"), title = paste0("Loss cover", "(ha/km", tags$sup(2), ")"))
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
       if(input$park != ""){
       p <- input$mymap_shape_click
       p2 <- gCentroid(natural_parks[[1]][natural_parks[[1]]$NAME == input$park, ]) %>% coordinates()
       proxy <- leafletProxy("mymap")
       if(length(p$id) && input$park != p$id){
         proxy %>% clearPopups() %>% setView(lng = p2[1], lat = p2[2] , input$mymap_zoom) %>% addMarkers(lng = p2[1], lat = p2[2], layerId = "NAME")
       }
       } else {
           proxy <- leafletProxy("mymap")
           proxy
         }
     })
     
     ############################################################################################################### 
     ################################################# GRAPHS AND DATA #############################################
     ###############################################################################################################
     
     ############################################################################################################### 
     ####################################################### RD ####################################################
     ###############################################################################################################
     
     #Individual graphs for all territories (natural parks + territories)
     #Data subset by input
     data <- reactive({
       if(input$park == ""){
         return(NULL)
       } else {
       defo_dist %>% subset(defo_dist$buffer_name == input$park) %>%
         mutate(., bin = cut(.$dist_disc, breaks = c(-50:50), include.lowest = T)) %>%
         group_by(bin) %>%
         summarize(meanbin = mean(loss_sum), sdbin = sd(loss_sum), n = length(ID)) %>%
         .[complete.cases(.),] %>%
         as.data.frame() %>%
         mutate(treatment = ifelse(as.numeric(row.names(.)) > 50, 1, 0), bins = row.names(.)) %>%
         mutate(bins = mapvalues(.$bins, from = c(1:100), to = c(-50:49)))
       }
     })
     
         output$rdplot <-
           renderPlot({
             if(input$park == ""){
               return(NULL)
             } else {
             g <- ggplot(data(), aes(y = (meanbin), x = as.numeric(bins), colour = as.factor(treatment)))
             g <- g + stat_smooth(method = "auto")
             g <- g + geom_point(colour = "black", size = 1)
             g <- g + labs(x = "Distance (km)", y = expression(paste("Deforestation", " (ha/",km^{2}, ")")))
             # g <- g + scale_x_continuous(limits = c(-20, 20))
             # g <- g + scale_y_continuous(limits = c(0, 0.3))
             # # g <- g + ggtitle(str_c("Discontinuidad\n", "para", type, sep = " "))
             g <- g + guides(colour = FALSE)
             # g <- g + theme_bw()
             g
             # # ggsave(str_c("RDggplot_", type, "strategy2",".pdf"), width=30, height=20, units="cm")
             # # }, x = l, type = c("Áreas protegidas nacionales","Áreas protegidas regionales","Resguardos indígenas", "Comunidades negras"))
             }
           })
     
     
     ############################################################################################################### 
     ################################################### RD ESTIMATE ###############################################
     ###############################################################################################################
     
     data_2 <- reactive({
       if(input$park == ""){
         return(NULL)
       } else {
         all_rd_df %>% mutate(buffer_name = as.factor(buffer_name)) %>% 
           subset(buffer_name == input$park) %>%
           rbind(., rd_agg[c(2:3), ])
       } 
     })
         
         output$rdestimate <-
           renderPlot({
             if(input$park == ""){
               return(NULL)
             } else{
               g2 <- ggplot(data_2(), aes(x = Type, y = LATE))
               g2 <- g2 + ylab(expression(paste("Local Average Treatment Effect", " (", "ha/",km^{2}, ")")))
               g2 <- g2 + geom_errorbar(width=.1, aes(ymin = ci_l, ymax = ci_u))
               g2 <- g2 + geom_point(shape = 21, size = 3, fill = "white")
               g2 <- g2 + geom_hline(aes(yintercept = 0))
               g2
             }
           })
         
         
         data_3 <- reactive({
           if(input$park == ""){
             return(NULL)
           } else {
             all_rd_df %>% mutate(buffer_name == as.factor(buffer_name)) %>%
               subset(buffer_name == input$park) %>%
               mutate(change = (LATE/defo_mean) * 100) %>%
               mutate(valid = ifelse(p_value > 0.05, 0, 1)) %>%
               mutate(avoided = LATE * N_r)
           }
         }) 
         
         output$avoided1 <-
           renderUI({
             if(input$park == ""){
               return(NULL)
             } else if(data_3()$valid == 1 & data_3()$LATE < 0){
              withMathJax(HTML(paste0("<b>LATE: </b>", data_3()$LATE, " ha/km$^2$",
                     "<b><br /> Avoided deforestation: </b>", round(data_3()$avoided, 3), " ha.",
                     "<b><br /> Percent change: </b>", round(data_3()$change, 2), " %")))
             } else if (data_3()$valid == 1 & data_3()$LATE > 0){
              withMathJax(HTML(paste0("<b>LATE: </b>", data_3()$LATE, " ha/km$^2$",
                     "<b><br /> Excess deforestation: </b>", round(data_3()$avoided, 3), " ha.",
                     "<b><br /> Percent change: </b>", round(data_3()$change, 2), "%")))
             } else if(data_3()$valid == 0){
               withMathJax(HTML(paste0("<center>The selected park does not have a significant effect</center>")))
             }
           })
 }



shinyApp(ui = ui, server = server)


