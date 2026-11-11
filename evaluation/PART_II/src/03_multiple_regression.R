################################################################################
#################################### 03_stepAIC.R ##################
#################################### dast_homework #############################
#################################### Aitor Lopez ###############################
################################################################################

#dependencies
if(!require(DHARMa)) install.packages("DHARMa", ncpus = 15)
if(!require(ggeffects)) install.packages("ggeffects", ncpus = 15)
if(!require(ggplot2)) install.packages("ggplot2", ncpus = 15)
if(!require(tidyverse)) install.packages("tidyverse", ncpus = 15)
#if(!require(FactoMineR)) install.packages("FactoMineR", ncpus = 15)
if(!require(dplyr)) install.packages("dplyr", ncpus = 15)
#if(!require(factoextra)) install.packages("factoextra", ncpus = 15)
if(!require(MASS)) install.packages("MASS", ncpus = 15)

library(DHARMa)
library(ggeffects)
library(ggplot2)
library(tidyverse)
#library(FactoMineR)
library(dplyr)
#library(factoextra)
library(MASS)

#load data
dat <- readRDS("./data/lopez/processed/dat.rds")

# As a first attempt we remove non-numeric variables
dat_nona <- na.omit(dat)
dummy_biom_lm <- lm(biom ~ 
                    num_trees +
                    mean_dbh +
                    competition_total +
                    dem1000_altitude +
                    dem1000_hillshade +
                    dem1000_roughness +
                    perc_cut +
                    cut_intensity_nfi +
                    dist_road +
                    mgmt_intensity +
                    tmoy_win_mean_anomaly +
                    tmoy_win_min_anomaly +
                    tmoy_win_max_anomaly +
                    tmoy_spr_mean_anomaly +
                    tmoy_spr_min_anomaly +
                    tmoy_spr_max_anomaly +
                    tmoy_sum_mean_anomaly +
                    tmoy_sum_min_anomaly +
                    tmoy_sum_max_anomaly +
                    tmoy_aut_mean_anomaly +
                    tmoy_aut_min_anomaly +
                    tmoy_aut_max_anomaly +
                    soil_CN +
                    soil_pH +
                    soil_waterlogging_perm +
                    soil_waterlogging_temp +
                    soil_swhc +
                    year,
                    data = dat_nona)
stepAIC(dummy_biom_lm, direction="both")
#Returns the following best model:
#lm(formula = biom ~ num_trees + mean_dbh + competition_total + 
#dem1000_altitude + mgmt_intensity + tmoy_spr_mean_anomaly + 
#tmoy_sum_min_anomaly + soil_CN + soil_pH + soil_waterlogging_perm + 
#soil_swhc + year, data = dat_nona)

# We arbitrarily decide to keep given variables and remove year
aic_filtered_biom_lm <- lm(formula = biom ~ 
                      num_trees + 
                      mean_dbh + 
                      competition_total + 
                      dem1000_altitude + 
                      mgmt_intensity + 
                      tmoy_spr_mean_anomaly + 
                      tmoy_sum_min_anomaly + 
                      soil_CN + 
                      soil_pH + 
                      soil_waterlogging_perm + 
                      soil_swhc,
                      data = dat_nona)
stepAIC(aic_filtered_biom_lm, direction="both")

#returns the following model:
#lm(formula = biom ~ num_trees + mean_dbh + competition_total + 
#dem1000_altitude + mgmt_intensity + soil_CN + soil_pH + soil_waterlogging_perm + 
#soil_swhc, data = dat_nona)