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

########################################## STRATEGY 2: EFFECTIVE BORDERS ###############################################

#Import datasets (covariates)
setwd("~/GitHub/deforestation_app/data/")
defo <- read.csv("dataframe_deforestacion.csv") %>% dplyr::select(-X)
cov <- read.csv("geographic_covariates.csv")
treecover <- read.csv("treecover_2000.csv") %>% dplyr::select(ID, treecover_agg)
dist <- read.csv("distancia_dataframe_clean.csv") %>% dplyr::rename(dist = layer)

#Conflict covariates (municipal level)
# muni <- read.csv("colombia_municipios_code_r.csv") %>% dplyr::select(ID, layer)
# setwd("~/Dropbox/BANREP/Deforestacion/Datos/Conflicto")
# conflict <- read.dta("conflicto_pre2000.dta")
# conflict_muni <- merge(muni, conflict, by.x = "layer", by.y = "codmun", all = T)

#Aggregate deforestation (2001 - 2012)
defo$loss_sum <- rowSums(defo[, c(4:length(names(defo)) - 1 )])
loss_sum <- dplyr::select(defo, c(ID, loss_sum)) %>% mutate(loss_sum = loss_sum / 12)

#Merge data
data_dist <- function(x){
  merge(loss_sum, x, by.x = "ID", by.y = "ID") %>%
    mutate(., loss_sum = loss_sum * 100) %>%
    mutate(., dist_disc = ifelse(treatment == 1, 1, -1) * dist) %>%
    mutate(., dist_disc = dist_disc / 1000)  %>%
    merge(., cov, by = "ID", all.x = T) %>%
    merge(., treecover, by = "ID") %>%
    mutate(., buffer_name = as.character(buffer_name))
}

defo_dist <- data_dist(dist)








