
### Setup
cat("\f")
rm(list = ls())
setwd("~/Dropbox/Michael Weintraub/Restriccion Motos/")
listpackages <- c("tidyverse","rgeos","rgdal","maptools","geosphere","haven")
sapply(listpackages,require,character.only=TRUE)
options("scipen"=100, "digits"=4) # Forzar a R a no usar e+

### Functions
read_event = function(file_dta){
           df <- read_dta(file = paste0("document/results/",file_dta)) %>% 
                 .[.$meses==6,] %>% .[,c("coef","pval","ci_lower","ci_upper","months","zone","city_restric","type_crime")]
           return(df)
}

read_effect = function(file_dta){
            df <- read_dta(file = paste0("document/results/",file_dta)) %>% 
                  .[.$meses==6,] %>% .[,c("coef","pval","ci_lower","ci_upper","zone","city_restric","type_crime")]
            return(df)
}

### Load effect estimates
file_effect = list.files("document/results/") %>% .[grep("app",.)]
effect_df = lapply(file_effect, function(x) read_effect(file_dta = x)) %>% data.table::rbindlist(.,use.names = T,fill = T) %>% data.frame(stringsAsFactors = F)

# Replace effect and 
effect_df = mutate(effect_df , city_restric = ifelse(test = city_restric=="Male passenger - Barranquilla",yes = "Acompañante hombre - Barranquilla",
                                                  no = ifelse(test = city_restric=="Male passenger - Bogotá",yes = "Acompañante hombre - Bogotá", 
                                                              no = ifelse(test = city_restric=="No motorcycles - Barranquilla",yes = "Sin motocicletas - Barranquilla",
                                                                          no = ifelse(test = city_restric=="No motorcycles - Soledad",yes = "Sin motocicletas - Soledad", 
                                                                                      no = ifelse(test = city_restric=="No passengers - Cartagena",yes = "Acompañantes - Cartagena",no = "Acompañantes - Neiva"))))))
effect_df = mutate(effect_df , type_crime = ifelse(test = type_crime=="Aggressor on a motorcycle",yes = "Desde una motocicleta",
                                                  no = ifelse(test = type_crime=="Homicide and personal injury",yes = "Homicidos y lesiones personales", 
                                                              no = ifelse(test = type_crime=="Property crimes",yes = "Contra la propiedad",
                                                                          no = ifelse(test = type_crime=="Thefts",yes = "Hurto a personas", 
                                                                                      no = ifelse(test = type_crime=="Total crimes",yes = "Totales",no = "Agresor a pie"))))))

### Load event study
file_event = list.files("document/results/") %>% .[grep("event study",.)]
event_df = lapply(file_event, function(x) read_event(file_dta = x)) %>% data.table::rbindlist(.,use.names = T,fill = T) %>% data.frame(stringsAsFactors = F)
event_df = mutate(event_df , city_restric = ifelse(test = city_restric=="Male passenger - Barranquilla",yes = "Acompañante hombre - Barranquilla",
                                                      no = ifelse(test = city_restric=="Male passenger - Bogotá",yes = "Acompañante hombre - Bogotá", 
                                                                  no = ifelse(test = city_restric=="No motorcycles - Barranquilla",yes = "Sin motocicletas - Barranquilla",
                                                                              no = ifelse(test = city_restric=="No motorcycles - Soledad",yes = "Sin motocicletas - Soledad", 
                                                                                          no = ifelse(test = city_restric=="No passengers - Cartagena",yes = "Acompañantes - Cartagena",no = "Acompañantes - Neiva"))))))
event_df = mutate(event_df , type_crime = ifelse(test = type_crime=="Aggressor on a motorcycle",yes = "Desde una motocicleta",
                                                   no = ifelse(test = type_crime=="Homicide and personal injury",yes = "Homicidos y lesiones personales", 
                                                               no = ifelse(test = type_crime=="Property crimes",yes = "Contra la propiedad",
                                                                           no = ifelse(test = type_crime=="Thefts",yes = "Hurto a personas", 
                                                                                       no = ifelse(test = type_crime=="Total crimes",yes = "Totales",no = "Agresor a pie"))))))

### Save 
#df_results <- list(effect_df,event_df)
#saveRDS(df_results,file = 'github-app/data/df_results.rds')

