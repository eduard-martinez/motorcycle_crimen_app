
### Setup
cat("\f")
rm(list = ls())
setwd("~/Dropbox/Michael Weintraub/Restriccion Motos/")
listpackages <- c("tidyverse","rgeos","rgdal","maptools","geosphere","haven")
sapply(listpackages,require,character.only=TRUE)

### Load crimen data

data = read_dta(file = "data/processed/crimes/Barranquilla/Geocoded/Barranquilla 2012-2018.dta")






"" "Bogota"       "Cartagena"    "Descriptive" 
[5] "Neiva"        "Santa Marta" 


### Functions
read_shape = function(path,name_layer,ID){
             city = readOGR(dsn = paste0("Data/Processed/Maps/",path), layer = name_layer) %>% 
             spTransform(.,CRSobj = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
             slot(slot(city,"polygons")[[1]], "ID") = ID
             row.names(city@data) <- ID
return(city)
}
atributtes = function(df,city_rest,name_rest,date_rest,area_rest,percent_rest){
             df <- mutate(df,city_restric = city_rest, 
                             name_restric = name_rest, 
                             date_restric = date_rest,
                             rate_crime = area_rest,
                             rate_total = area_rest,
                             percent_restric = percent_rest)  
} 

### Load shape city
barranquilla = read_shape("Barranquilla/Base Maps","barranquilla_habited","1")
soledad = read_shape("Barranquilla/Base Maps","soledad_habited","2")
bogota = read_shape("Bogota/Base Maps","bogota_habited","3")
cartagena = read_shape("Cartagena/Base Maps","cartagena_habited","4")
neiva = read_shape("Neiva/Base Maps","neiva_habited","5")

### Rbind shapes citys
citys <- spRbind(barranquilla,soledad) %>% spRbind(.,bogota) %>% spRbind(.,cartagena) %>% spRbind(.,neiva)

### Load Restrictions
r_baq_male = read_shape("Barranquilla/restriction","restric_man","1") 
r_baq_male@data = atributtes(df = r_baq_male@data,city_rest = "Acompañante hombre - Barranquilla", area_rest = as.character(round(gArea(r_baq_male)/gArea(barranquilla)*100,2)),
                             name_rest = "Restricción de acompañante hombre", date_rest = '1 de febrero de 2017 - actual', 
                             percent_rest = '')
r_bog_male = read_shape("Bogota/restriction","restric","2")
r_bog_male@data = atributtes(df = r_bog_male@data,city_rest = "Acompañante hombre - Bogotá",area_rest = as.character(round(gArea(r_bog_male)/gArea(bogota)*100,2)),
                             name_rest = "Restricción de acompañante hombre", date_rest = '2 de febrero de 2018 - 2 de agosto del año 2018', 
                             percent_rest = '')
r_car_pass = read_shape("Cartagena/restriction","restric","3")
r_car_pass@data = atributtes(df = r_car_pass@data,city_rest = "Acompañantes - Cartagena",area_rest = as.character(round(gArea(r_car_pass)/gArea(cartagena)*100,2)),
                             name_rest = "Restricción de acompañantes", date_rest = '20 de septiembre de 2016 - actual', 
                             percent_rest = '')
r_neiva_pass = read_shape("Neiva/restriction","restric","4")
r_neiva_pass@data = atributtes(df = r_neiva_pass@data,city_rest = "Acompañantes - Neiva" , area_rest = as.character(round(gArea(r_neiva_pass)/gArea(neiva)*100,2)),
                             name_rest = "Restricción de acompañantes", date_rest = '29 de enero de 2016 - actual', 
                             percent_rest = '')
r_sol_moto = read_shape("Barranquilla/Base Maps","soledad_habited","5")
r_sol_moto@data = atributtes(df = r_sol_moto@data,city_rest = "Sin motocicletas - Soledad",area_rest = as.character(round(gArea(r_sol_moto)/gArea(soledad)*100,2)),
                             name_rest = "Zona sin motocicletas", date_rest = '22 de marzo de 2013 - actual', 
                             percent_rest = '')
r_baq_moto = read_shape("Barranquilla/restriction","without_motorcycle_2011",as.character(6:11)) %>%
             gUnaryUnion() %>% as(.,  "SpatialPolygonsDataFrame") 
r_baq_moto@data = atributtes(df = r_baq_moto@data,city_rest = "Sin motocicletas - Barranquilla",area_rest = as.character(round(gArea(r_baq_moto)/gArea(barranquilla)*100,2)),
                             name_rest = "Zona sin motocicletas", date_rest = '11 de septiembre de 2006 - actual', 
                             percent_rest = '')
slot(slot(r_baq_moto,"polygons")[[1]], "ID") = "6"
row.names(r_baq_moto@data) <- "6"

### Rbind shapes restrictions
restric <- spRbind(r_baq_male,r_bog_male) %>% spRbind(.,r_car_pass) %>% 
           spRbind(.,r_neiva_pass) %>% spRbind(.,r_sol_moto) %>% spRbind(.,r_baq_moto)

### Save
maps <- list(citys,restric)
saveRDS(maps,file = 'github-app/data/maps.rds')
