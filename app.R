
# Load packages
library('shiny')
library('shinythemes')
library('tidyverse')
library('maptools')
library("broom")
library('leaflet')
library('htmltools')
library('rsconnect')
library('rgdal')
library('magrittr')
library('sp')
library('rgeos')
library("ggpubr")

# Load maps and data
maps = readRDS("data/maps.rds")
df_results = readRDS("data/df_results.rds")
 
#-----------------------#
# Define user interface #
#-----------------------# 
                              
ui <- navbarPage(title = "Restricciones a motocicletas y sus efectos sobre el crimen en Colombia", id="nav", theme = "http://bootswatch.com/spacelab/bootstrap.css", inverse=TRUE,
                 
                 # Panel Results
                 tabPanel("Resultados",withMathJax(),tags$style(type="text/css", "html, body {width:100%;height:100%}"),
                          div(class="outer", tags$head(includeCSS("style/style.css"),includeScript("style/gomap.js")),
                          #tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
                          #tags$script("MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]}});", type='text/x-mathjax-config')),
                         
                          
                          # Map
                          leafletOutput("mymap", width="100%", height="100%"),
                              
                          # Coefplot
                          absolutePanel(id = "coefplot", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = 50, right = "auto", bottom = "auto", width = 330, height = "auto", 
                                        h3("Restricciones a motocicletas y sus efectos sobre el crimen en Colombia"),
                                        selectInput(inputId = "zonerestric", label = "Seleccione una restricción:", choices = c("",as.character(maps[[2]]$city_restric)), selected = ""),
                                        plotOutput("DD_plot", width = 280, height = 240),
                                        htmlOutput("avoided_coef")
                          ),

                        
                          # Event Study
                          absolutePanel(id = "event", fixed = TRUE,draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",width = 330, height = "auto",
                                        conditionalPanel(
                                                        condition = "input.zonerestric != '' ",
                                                        h4('Event Study', align = "center"),
                                                        selectInput(inputId = "type_crime", label = "Seleccione un tipo de delito:", 
                                                                    choices = c(unique(df_results[[1]]$type_crime)),
                                                                    selected = "Totales"),
                                                        plotOutput("eventstudy", width = 300, height = 250),
                                                        htmlOutput("avoided_event")
                                                        
                                        )
                          )
                      )
                 ),
                 
                 
                 # Panel Description
                 tabPanel("Estrategia Empírica",
                          # Solucion al problema de insertar html https://stackoverflow.com/questions/25882276/does-shiny-ui-r-support-including-html-pages-on-each-tabpanel-with-in-tabsetpane 
                          includeHTML("text/description.html")
                 ),
                 
                 
                 # Panel About
                 tabPanel("Acerca de este proyecto",
                          # Solucion al problema de insertar html https://stackoverflow.com/questions/25882276/does-shiny-ui-r-support-including-html-pages-on-each-tabpanel-with-in-tabsetpane 
                          includeHTML("text/about.html")
                 )
)





# Create a pop-up:
popup <- paste0("<center><b>", maps[[2]]@data$name_restric, "</b></center>",
                "<b><br />Vigencia: </b> ", maps[[2]]@data$date_restric,
                "<b><br />Densidad del delito: </b>", maps[[2]]@data$area_restric, " % de los delitos totales en la ciudad.",
                "<b><br />Delitos en motocicleta / Delitos totales: </b>", maps[[2]]@data$area_restric, " %.",
                "<b><br />Extención: </b>", maps[[2]]@data$percent_restric, " % del área total de la ciudad.")

pal <- colorFactor( palette = c("#31a354", "#e5f5f9","pink2"), domain = maps[[2]]$name_restric)


#---------------#
# Define server #
#---------------#
server <- function(input, output,session) {

          ### Create the map
          output$mymap <- renderLeaflet({  
          leaflet() %>% addTiles() %>% addPolygons(data = maps[[1]] , color='black' , weight = 2 , fillColor=NA, fillOpacity = 0.01) %>% 
          addPolygons(data = maps[[2]], popup = popup, layerId = as.character(maps[[2]]$city_restric), color = "red", fill = "red", weight = 2, opacity = 0.5) %>% 
          addProviderTiles("CartoDB.Positron") #%>% 
          #addLegend(position="bottomleft", pal = pal, values = maps[[2]]$name_restric, labels=c(unique(maps[[2]]$name_restric)), title = "Zone") 
          })
          observeEvent(input$mymap_shape_click, { # update the location selectInput on map clicks
          p <- input$mymap_shape_click
          if(!is.null(p$id)){if(is.null(input$zonerestric) || input$zonerestric != p$id) updateSelectInput(session, "zonerestric", selected = p$id)
          }
          })
          observeEvent(input$mymap_shape_click, { # update the map view on map clicks
                       p <- input$mymap_shape_click
                       proxy <- leafletProxy("mymap")
                       if(is.null(p$id)){proxy} 
                       else {proxy %>% setView(lng=p$lng, lat=p$lat, input$mymap_zoom) %>% removeMarker(layerId="city_restric")}
          })
          observeEvent(input$zonerestric, { # update the map view on location selectInput changes
                       if(input$zonerestric != ""){
                       p <- input$mymap_shape_click
                       p2 <- gCentroid(maps[[2]][maps[[2]]$city_restric == input$zonerestric, ]) %>% coordinates()
                       proxy <- leafletProxy("mymap")
                       if(length(p$id) && input$zonerestric != p$id){
                          proxy %>% clearPopups() %>% setView(lng = p2[1], lat = p2[2] , input$mymap_zoom) %>% addMarkers(lng = p2[1], lat = p2[2], layerId = "city_restric")
                       }
                       } 
                       else {proxy <- leafletProxy("mymap")
                             proxy
                       }
          })
   
          
          ### Coefplot
          data_1 <- reactive({
                    if(input$zonerestric == ""){ return(NULL)} 
                    else {df_results[[1]] %>% subset(city_restric == input$zonerestric) %>%
                          subset(type_crime == "Total crimes")
                    }
          })
          data_2 <- reactive({
                    if(input$zonerestric == ""){ return(NULL)} 
                    else {df_results[[1]] %>% subset(city_restric == input$zonerestric) %>%
                          subset(type_crime == input$type_crime)
                    }
          })
          output$DD_plot <- renderPlot({
                            if(input$zonerestric == ""){return(NULL)} 
                            else {g2 <- ggplot(data_2(), aes(y = coef, x = zone, color=zone,fill = zone)) + 
                                        scale_color_manual(values=c("darkblue","brown4")) + scale_fill_manual(values=c("darkblue","brown4")) +
                                        geom_errorbar(width=.1, aes(ymin = ci_lower, ymax = ci_upper),show.legend = F) + 
                                        geom_point(shape = 21, size = 3,show.legend = F) + 
                                        geom_hline(aes(yintercept = 0),linetype="solid",colour = "black") +
                                        theme_bw()  +  theme(plot.title = element_text(hjust = 0.5,size = 16)) + xlab("Zona") + ylab("Coeficiente") + 
                                        ggtitle(as.character(input$type_crime)) 
                                  g2
                           }
          })
          
          
          ### Event Study
          data_3 <- reactive({
                    if(input$zonerestric == ""){  return(NULL)} 
                    else { df_results[[2]] %>% subset(city_restric == input$zonerestric) %>%
                           subset(type_crime == input$type_crime)
                    } 
          })
          output$eventstudy <- renderPlot({
                               if(input$zonerestric == ""){ return(NULL) } 
                               else{ g3 <- ggplot(data_3(), aes(x = months, y = coef, color = zone,fill = zone)) + labs(" ") +
                                           scale_color_manual(values=c("darkblue","brown4")) + scale_fill_manual(values=c("darkblue","brown4")) +
                                           geom_errorbar(width=.1, aes(ymin = ci_lower, ymax = ci_upper)) + scale_x_discrete(limits=c(-6:5)) +
                                           geom_point(shape = 21, size = 3) + ylab("Coeficiente") + xlab("Meses al inicio de la restricción") +
                                           geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = -1),linetype="dashed",colour = "black") +
                                           theme_bw() + theme(plot.title = element_text(hjust = 0.5,size = 20),
                                                              legend.title = element_blank(),legend.position="bottom",
                                                              legend.direction = "horizontal",legend.text = element_text(hjust = 0.5,size = 10) )
                                    g3
                               }
           })
        
          
           ### avoided_coef     
           treated <- reactive({if(input$zonerestric == ""){return(NULL)}else {subset(data_2(),zone=="Treated") %>% .[1,2] %>% as.numeric()}})
           spillover <- reactive({if(input$zonerestric == ""){return(NULL)}else {subset(data_2(),zone=="Spillover") %>% .[1,2] %>% as.numeric()}})
           output$avoided_coef <- renderUI({if(input$zonerestric == ""){ return(NULL)} else {
           if(treated() < 0.05 & spillover() < 0.05){
              withMathJax(HTML(paste0("<p>","En este grafico se muestra el efecto promedio de la restricción sobre los delitos (",
                                      tolower(input$type_crime),") en la zona tratada y la zona de spillover.","</p>"), 
                               paste0("<p>","Los resultados sugieren que durante los 6 primeros meses de la restricción, se redujeron (",as.character(round(as.numeric(subset(data_2(),zone=="Treated")$coef),5)),
                                      ") los delitos (",tolower(input$type_crime),") en la zona de tratamiento. Sin embargo, el efecto es compensado por un incremento de igual magnitud (",as.character(round(as.numeric(subset(data_2(),zone=="Spillover")$coef),5)),
                                      ") en la zona de spillover.","</p>"))) 
           }
           else if(treated() < 0.05 & spillover() >= 0.05){
             withMathJax(HTML(paste0("<p>","En este grafico se muestra el efecto promedio de la restricción sobre los delitos (",
                                     tolower(input$type_crime),") en la zona tratada y la zona de spillover.","</p>"), 
                              paste0("<p>","Los resultados sugieren que durante los 6 primeros meses de la restricción, se redujeron (",as.character(round(as.numeric(subset(data_2(),zone=="Treated")$coef),5)),
                                     ") los delitos (",tolower(input$type_crime),") en la zona de tratamiento. Mientras que para la zona de spillover no se observan cambios estadísticamente significativos.","</p>"))) 
           }
           else if(treated() >= 0.05 & spillover() < 0.05){
             withMathJax(HTML(paste0("<p>","En este grafico se muestra el efecto promedio de la restricción sobre los delitos (",
                                     tolower(input$type_crime),") en la zona tratada y la zona de spillover.","</p>"), 
                              paste0("<p>","Los resultados sugieren que durante los 6 primeros meses de la restricción, se incrementaron (",as.character(round(as.numeric(subset(data_2(),zone=="Spillover")$coef),5)),
                                     ") los delitos (",tolower(input$type_crime),") en la zona de spillover. Mientras que para la zona de tratamiento no se observan cambios estadísticamente significativos.","</p>"))) 
           }
           else if (treated() >= 0.05 & spillover() >= 0.05){
             withMathJax(HTML(paste0("<p>","En este grafico se muestra el efecto promedio de la restricción sobre los delitos (",
                                     tolower(input$type_crime),") en la zona tratada y la zona de spillover.","</p>"), 
                              paste0("<p>","Los resultados sugieren que durante los 6 primeros meses de la restricción, la medida no tuvo efectos estadísticamente significativos sobre 
                                     delitos (",tolower(input$type_crime),") ni en la zona de tratamiento ni en la zona de spillover.","</p>"))) 
           }
           }})
          
           
           ### avoided_event
           output$avoided_event <- renderUI({if(input$zonerestric == ""){ return(NULL)} 
                                            else {withMathJax(HTML(paste0("Esta grafica muestra la dinámica temporal del efecto de la restricción sobre los los delitos (",
                                                                          tolower(input$type_crime),") para una ventana de 6 meses alrededor de la fecha implementación.")))
                                            }
           })

}

#---------------------#
# Run the application #
#---------------------#
shinyApp(ui = ui, server = server)
