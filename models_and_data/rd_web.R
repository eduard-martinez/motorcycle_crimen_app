# Run RD regressions (For parks and territories)
rm(list=ls())
library(plyr)
library(dplyr)
library(data.table)
library(rdrobust)
library(rdd)
library(stringr)
library(stargazer)
library(foreign)
library(rddtools)
library(ggplot2)
library(magrittr)
library(foreign)
library(stringr)
library(pbapply)



##################################################################################################################
########################################## STRATEGY 2: EFFECTIVE BORDERS #########################################
##################################################################################################################

##################################################################################################################
################################################## 1. PREPARE DATA ###############################################
##################################################################################################################

#Import datasets (covariates)
setwd("~/Dropbox/BANREP/Deforestacion/Datos/Dataframes")
defo <- read.csv("dataframe_deforestacion.csv") %>% dplyr::select(-X)
cov <- read.csv("geographic_covariates.csv")
treecover <- read.csv("treecover_2000.csv") %>% dplyr::select(ID, treecover_agg)
setwd("~/Documents/web_data/")
dist <- read.csv("distancia_dataframe_clean.csv") %>% dplyr::rename(dist = layer)

#Conflict covariates (municipal level)
# muni <- read.csv("colombia_municipios_code_r.csv") %>% dplyr::select(ID, layer)
# setwd("~/Dropbox/BANREP/Deforestacion/Datos/Conflicto")
# conflict <- read.dta("conflicto_pre2000.dta")
# conflict_muni <- merge(muni, conflict, by.x = "layer", by.y = "codmun", all = T)

#Aggregate deforestation (2001 - 2012)
defo$loss_sum <- rowSums(defo[, c(4:length(names(defo)) - 1 )])
loss_sum <- dplyr::select(defo, c(ID, loss_sum)) %>% mutate(loss_sum = loss_sum / 12)

#Prepare data
data_dist <- function(x){
  merge(loss_sum, x, by.x = "ID", by.y = "ID") %>%
    mutate(., loss_sum = loss_sum * 100) %>%
    mutate(., dist_disc = ifelse(treatment == 1, 1, -1) * dist) %>%
    mutate(., dist_disc = dist_disc / 1000)  %>%
    merge(., cov, by = "ID", all.x = T) %>%
    merge(., treecover, by = "ID") %>%
    mutate(., buffer_name = as.character(buffer_name))
}

defo_dist_controls <- data_dist(dist)

defo_dist_controls_l <- data_dist(dist) %>%
  split(defo_dist_controls$buffer_name)

##################################################################################################################
################################################# 1. ESTIMATE RD'S ###############################################
##################################################################################################################

#All parks without controls
rd_robust_list_nc <- pblapply(defo_dist_controls_l, failwith(NULL, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    # x = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec,
    #           park$sq_1km.1, park$treecover_agg, park$clumps_1),
    vce = "nn", 
    nnmatch = 3,
    all = T
  )
})) 

#All parks with controls 

rd_robust_list <- pblapply(defo_dist_controls_l, failwith(NULL, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    # covs = eval(parse( text = list_covs)),
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec,
            park$sq_1km.1, park$treecover_agg, park$clumps_1),
    vce = "nn", 
    nnmatch = 3,
    all = T
  )
})) 


#All parks with controls (with a tryCatch()/plyr::failwith() clause to drop the colinear covariate)
list_covs <- str_c("cbind(", "x$altura_tile_30arc, ", "x$slope, ", "x$roughness, ", "x$prec, ",
      "x$sq_1km.1,", "x$treecover_agg, ", "x$clumps_1)")

stepwise_rd <- function(list_rd, list_df, list_covs){
  count <- 2
  new_rd <- list()
  null_names <- names(list_rd)[sapply(list_rd, function(x) is.null(x) == TRUE)]
  df <- list_df[null_names]
  while(length(null_names) > 4){
    new_rd[[length(new_rd) + 1]] <- pblapply(df, failwith(NULL, function(x){
      # covs <- eval(parse(text = list_covs))[, c(1:ncol(eval(parse(text = list_covs))) - 1)]
      rdrobust(
        y = x$loss_sum,
        x = x$dist_disc,
        covs = eval(parse(text = list_covs))[, -c(ncol(covs):ncol(covs) - count)],
        vce = "nn",
        all = T)
    }))
    null_names <- names(new_rd[[length(new_rd)]])[sapply(new_rd[[length(new_rd)]], function(x) is.null(x) == TRUE)]
    df <- df[null_names]
    count <- count + 1
    # print(length(new_rd[[count]]))
    # print(count)
  }
  return(new_rd)
}

#Run stepwise rd and keep in the list only the valid results
l <- stepwise_rd(list_rd = rd_robust_list, list_df = defo_dist_controls_l, list_covs = list_covs) %>%
  lapply(., function(x){
    valid <- names(x)[sapply(x, function(y) is.null(y) == FALSE)]
    x[valid]
  })


##################################################################################################################
########################################### 3. TRANSFORM DATA TO DF ##############################################
##################################################################################################################

#Append all valid elements in the list 
l_all <- do.call("c", l)
keys <- unique(c(names(l), names(rd_robust_list)))
# keys <- unique(unlist(lapply(all_rd, names)))
# setNames(do.call(mapply, c(FUN = c, lapply(l, "[", keys))), keys)
all_rd <- setNames(mapply(c, rd_robust_list[keys], l_all[keys]), keys)

#Filter the data.frames distance list (get valid data.frames)
defo_dist_valid <- defo_dist_controls_l[names(all_rd)[sapply(all_rd, function(x) is.null(x) == FALSE)]]

##################################################################################################################
############################################## RD TO DF FUNCTION #################################################
##################################################################################################################
rd_to_df <- function(list, dataframe){
  rd <- lapply(list, "[", "tabl3.str") %>%
    lapply(as.data.frame) %>%
    lapply( "[", 3 , ) %>%
    ldply() %>% mutate(N_l = unlist(lapply(list, "[", "N_h_l"))) %>%
    mutate(N_r = unlist(lapply(list, "[", "N_h_r"))) %>%
    mutate(N = N_l + N_r) %>%
    mutate(bws = unlist(lapply(list, function(x) x$bws[1, 1])))
  
  # -- Comment from here to the end to get the reduced form of the funcion -- #

  defo_mean <- mapply(function(x, y){
    y %>%
      filter(abs(dist_disc) <= x$bws[1, 1] & treatment == 0) %>%
      summarize(mean = mean(loss_sum))
  }, x = list , y = dataframe, SIMPLIFY = F) %>% unlist()

  df <- rd %>% cbind(., defo_mean) %>%
    mutate_all(funs(as.character)) %>% mutate_all(funs(as.numeric)) %>%
    rename(buffer_name = .id, LATE = tabl3.str.Coef, se = tabl3.str.Std..Err., z = tabl3.str.z,
           p_value = tabl3.str.P..z., ci_l = tabl3.str.CI.Lower, ci_u = tabl3.str.CI.Upper) %>%
    mutate(buffer_name = names(dataframe)[sapply(dataframe, function(y) is.null(y) == FALSE)]) %>%
    mutate(Type = "Selected") %>%
    mutate(ci_l_alt = LATE - 1.96 * se) %>%
    mutate(c_u_alt = LATE - 1.96 * se)
}


#Get a data.frame for the valid results (remove from all_rd the NULL ones)
nulls <- names(all_rd)[sapply(all_rd, function(x) is.null(x) == TRUE)]
all_rd_df <- rd_to_df(list = all_rd[sapply(all_rd, function(x) is.null(x) == FALSE)],
                      dataframe = defo_dist_valid)

#Compare with aggregated results (export from models in deforestation)
setwd("~/Dropbox/BANREP/Backup Data")
rd_agg <- rd_to_df(readRDS("rd_robust_parks_2_ctrl_fx.rds")) %>% #reduced form of the function rd_to_df
  rename(LATE = tabl3.str.Coef, se = tabl3.str.Std..Err., z = tabl3.str.z,
                p_value = tabl3.str.P..z., ci_l = tabl3.str.CI.Lower, ci_u = tabl3.str.CI.Upper) %>%
  mutate_all(funs(as.character)) %>% mutate_all(funs(as.numeric)) %>%
  mutate(buffer_name = c("All", "National", "Regional")) %>%
  mutate(Type = c("Protected areas", "National", "Regional")) %>%
  mutate(defo_mean = c(NA, 0.219, 0.197))  %>%
  mutate(ci_l_alt = LATE - 1.96 * se) %>%
  mutate(c_u_alt = LATE - 1.96 * se)

rd_agg <- rd_agg[ ,c(11, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 12, 14, 15)]

#Merge individual results with aggregated results
all_rd_df <- rbind(all_rd_df, rd_agg)

##################################################################################################################
############################################ 4. PREPARE DATA FOR LEAFLET #########################################
##################################################################################################################

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


#Subset natural parks list for only valid results
natural_parks <- natural_parks %>%
  lapply(., function(x){
    x$NAME <- as.character(x$NAME)
    x[x$NAME %in% all_rd_df$buffer_name, ]
  })


##################################################################################################################
####################################### 5. CREATE NEW VARIABLES WITH DEFO DATA  ##################################
##################################################################################################################

#Deforestation by park
# Table deforestation by park type
setwd("~/Dropbox/BANREP/Deforestacion/Datos/HansenProcessed/")
res <- brick("loss_year_brick_1km.tif")

#Get defo by polygon
defo_type <- raster::extract(res, natural_parks[[1]], fun = sum, na.rm = T, df = T)

#Get area by pixel
cells_natural <- cellFromPolygon(res, natural_parks[[1]]) %>%
  sapply(length)

defo_type_tot <- defo_type %>%
  transmute(loss_sum = rowSums(defo_type[, c(4:length(names(defo_type)) - 1 )])) %>% 
  mutate(defo_total = (loss_sum * 100) / 12) %>% 
  mutate(area = gArea(natural_parks[[2]], byid = T) / 1e6) %>% 
  mutate(area_pixel = cells_natural) %>% 
  mutate(defo_total_area = defo_total / area) %>% 
  mutate(defo_total_pixel = defo_total / area_pixel) %>%
  mutate(id = row.names(.))

natural_parks <- lapply(natural_parks, function(x){ 
  mutate(x@data, id = c(1:length(x)))
  x@data <- cbind(x@data, defo_type_tot)
  return(x)
})

#Get a binary raster for deforestation for all the period
setwd("~/Dropbox/BANREP/Deforestacion/Datos/HansenProcessed/")
loss_agg <- stackApply(res[[c(2:13)]], 1, fun = sum,  filename = "loss_year_aggregated_1km.tif",
                       format = "GTiff",
                       options = "INTERLEAVE=BAND", 
                       progress = "text", overwrite = T)

rcl <- matrix(c(-Inf, 0.2, NA,
                0.2, 0.4, 1,
                0.4, 0.6, 2,
                0.6, 0.8, 3,
                0.8, 1, 4), ncol = 3, byrow = TRUE)

loss_agg_rec <- reclassify(loss_agg, rcl, filename = "data/loss_year_reclassify_1km.tif",
                           format = "GTiff",
                           options = "INTERLEAVE=BAND", 
                           progress = "text", overwrite = T) 

loss_agg_proj <- loss_agg %>% 
  projectRaster(., crs = CRS("+init=epsg:3857"),  filename = "loss_year_aggregated_1km.tif",
                format = "GTiff",
                options = "INTERLEAVE=BAND", 
                progress = "text", overwrite = T )

##################################################################################################################
############################################ 6. PREPARE DATA FOR LEAFLET #########################################
##################################################################################################################

save(loss_agg_rec, natural_parks, defo_dist, all_rd_df, file = "data_app.RData")

           
