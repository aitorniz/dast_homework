################################################################################
#################################### 03_multiple_regression.R ##################
#################################### dast_homework #############################
#################################### Aitor Lopez ###############################
################################################################################

#dependencies
if(!require(DHARMa)) install.packages("DHARMa", ncpus = 15)
if(!require(ggeffects)) install.packages("ggeffects", ncpus = 15)
if(!require(ggplot2)) install.packages("ggplot2", ncpus = 15)
if(!require(tidyverse)) install.packages("tidyverse", ncpus = 15)
library(DHARMa)
library(ggeffects)
library(ggplot2)
library(tidyverse)
#load data
dat <- readRDS("./data/lopez/processed/dat.rds")

# We have seen that number of trees has the biggest R² respect to biomass
# so now we try to build on top a model explaining biomass by number of trees
biomass <- dat$biom
num_trees <- dat$num_trees
mean_dbh <- dat$mean_dbh
soil_waterlogging_perm <- dat$soil_waterlogging_perm
nb_tree_mean_dbh_to_biom <- lm(biomass ~ num_trees + 
                                 mean_dbh + 
                                 soil_waterlogging_perm +
                                 , data = dat)
summary(nb_tree_mean_dbh_to_biom)

regIntercept <- lm(biomass ~ 1, data = dat)
length(num_trees)
length(mean_dbh)
length(soil_waterlogging_perm)
regSelected <- step(regIntercept,
                    scope = ~ num_trees + mean_dbh + soil_waterlogging_perm,
                    direction = "forward",
                    trace = 1)
