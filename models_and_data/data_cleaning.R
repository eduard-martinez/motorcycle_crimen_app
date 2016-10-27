setwd("~/Dropbox/BANREP/Deforestacion/Datos/HansenProcessed/")
res <- brick("loss_year_brick_1km.tif")

#Open natural parks shapefile (2 SP object, 1. Projected in meters and 2. Mercator)
setwd("~/Dropbox/BANREP/Deforestacion/Datos/UNEP")
natural_parks <- readOGR(dsn = "WDPA_June2016_COL-shapefile", layer = "WDPA_June2016_COL-shapefile-polygons")
natural_parks_proj <- spTransform(natural_parks, CRS=CRS("+init=epsg:3857")) #Projection in meters

#For tracktability
natural_parks <- list(natural_parks, natural_parks_proj)
natural_parks[[2]]@data$ID <- c(1:dim(natural_parks[[2]]@data)[1])
natural_parks[[1]]@data$ID <- c(1:dim(natural_parks[[1]]@data)[1])

#Remove NP that are out of continental land and parks after 2012
natural_parks <- natural_parks %>%
  lapply(., function(x){
    x[!(x@data$NAME %in% c("Malpelo Fauna and Flora Sanctuary", 
                           "Old Providence Mc Bean Lagoon",
                           "Malpelo",
                           "Jhonny Cay Regional Park",
                           "The Peak Regional Park",
                           "Corales De Profundidad",
                           "Los Corales Del Rosario Y De San Bernardo",
                           "Gorgona",
                           "Acandi Playon Y Playona",
                           "Uramba Bahia Malaga")) & !x@data$STATUS_YR > 2000 & !x@data$GIS_AREA < 1 , ]
    
    
  }) %>%
  #Remove sections of park outside continental Colombia
  mapply(function(x, y){
    raster::intersect(y, x)
  }, x = . , y = colombia_municipios)

setwd("~/GitHub/deforestation_app/data/")
saveRDS(natural_parks, "natural_parks.rds")

# Distances dataframe
reg_dis <- readRDS("dist_web_2001.rds") %>%
  mutate(buffer_id = as.character(buffer_id)) %>%
  merge(., natural_parks[[1]])




