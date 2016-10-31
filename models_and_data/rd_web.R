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

########################################## STRATEGY 2: EFFECTIVE BORDERS ###############################################

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

#RD's with optimal bw's for all polygons (list) 

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

#Append all valid elements in the list 
l_all <- do.call("c", l)
keys <- unique(c(names(l), names(rd_robust_list)))
# keys <- unique(unlist(lapply(all_rd, names)))
# setNames(do.call(mapply, c(FUN = c, lapply(l, "[", keys))), keys)
all_rd <- setNames(mapply(c, rd_robust_list[keys], l_all[keys]), keys)

#Filter the data.frames distance list (get valid data.frames)
defo_dist_valid <- defo_dist_controls_l[names(all_rd)[sapply(all_rd, function(x) is.null(x) == FALSE)]]

#RD list to data.frame
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
    mutate(type = "Protected area")
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
  mutate(buffer_name = c("All", "National", "Regional")) %>%
  mutate(type = c("All protected areas", "All National protected areas", "All regional protected areas")) %>%
  mutate(defo_mean = c(NA, 0.219, 0.197))

rd_agg <- rd_agg[ ,c(11, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 12)]

#Merge individual results with aggregated results
all_rd_df <- rbind(all_rd_df, rd_agg)


# Make the graph with the 95% confidence interval
ggplot(all_rd_df[c(1, 123, 124), ], aes(x=type, y=LATE, group=1)) +
  geom_errorbar(width=.1, aes(ymin=ci_l, ymax=ci_u)) +
  geom_point(shape=21, size=3, fill="white")



#Misc
a <- names(l[[9]])[sapply(l[[9]], function(x) is.null(x) == TRUE)]
a <- table(sapply(rd_robust_list_nc, function(x) is.null(x) == TRUE))


count <- 2
covs1 <- matrix(1:49, 7, 7)
covs2 <- matrix(49, 7, 7)
covs <- cbind(covs1, covs2)


while(count < 7){
  covs <- covs[, -c(ncol(covs):ncol(covs) - count)]
  count <- count + 1
  print(dim(covs))
  print(count)
  print(covs)
  }






