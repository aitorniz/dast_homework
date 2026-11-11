################################################################################
#################################### 04_PCA.R ##################
#################################### dast_homework #############################
#################################### Aitor Lopez ###############################
################################################################################

#dependencies
if(!require(DHARMa)) install.packages("DHARMa", ncpus = 15)
if(!require(ggeffects)) install.packages("ggeffects", ncpus = 15)
if(!require(ggplot2)) install.packages("ggplot2", ncpus = 15)
if(!require(tidyverse)) install.packages("tidyverse", ncpus = 15)
if(!require(FactoMineR)) install.packages("FactoMineR", ncpus = 15)
if(!require(dplyr)) install.packages("dplyr", ncpus = 15)
if(!require(factoextra)) install.packages("factoextra", ncpus = 15)
if(!require(MASS)) install.packages("MASS", ncpus = 15)

library(DHARMa)
library(ggeffects)
library(ggplot2)
library(tidyverse)
library(FactoMineR)
library(dplyr)
library(factoextra)
library(MASS)

#load data
dat <- readRDS("./data/lopez/processed/dat.rds")
dat_nona <- na.omit(dat)
# We use variables stepAIC method identified as relevant
  #biom ~ num_trees + mean_dbh + competition_total + 
  #dem1000_altitude + mgmt_intensity + soil_CN + soil_pH + soil_waterlogging_perm + 
  #soil_swhc
dat_stepAIC_filtered <- data.frame(
  dat$biom,
  dat$num_trees,
  dat$mean_dbh,
  dat$competition_total,
  dat$dem1000_altitude,
  dat$mgmt_intensity,
  dat$soil_CN,
  dat$soil_pH,
  dat$soil_waterlogging_perm,
  dat$soil_swhc
)

biomPCA <- PCA(dat_stepAIC_filtered,
               scale.unit = TRUE,
               ncp = 9,
               graph = TRUE)
get_eigenvalue(biomPCA)
fviz_eig(biomPCA, addlabels = TRUE, ylim = c(0, 30))
var = get_pca_var(biomPCA)
fviz_pca_var(biomPCA, geom = c("text", "arrow"), col.var = "cos2", axes = 1:2) +
  theme_classic()
fviz_pca_var(biomPCA, geom = c("text", "arrow"), col.var = "cos2", axes = 3:4) +
  theme_classic()

# Because we see a strong correlation between mean_dbh, competition and biom
# we group them
dat_basic_pca_filtered <- data.frame(
  dat$biom,
  dat$mean_dbh,
  dat$competition_total,
)
dbh_ct_biomPCA <- PCA(dat_basic_pca_filtered,
               scale.unit = TRUE,
               ncp = 2,
               graph = TRUE)
get_eigenvalue(dbh_ct_biomPCA)
fviz_eig(dbh_ct_biomPCA, addlabels = TRUE, ylim = c(0, 30))
var = get_pca_var(dbh_ct_biomPCA)
fviz_pca_var(dbh_ct_biomPCA, geom = c("text", "arrow"), col.var = "cos2", axes = 1:2) +
  theme_classic()
fviz_pca_var(dbh_ct_biomPCA, geom = c("text", "arrow"), col.var = "cos2", axes = 3:4) +
  theme_classic()

################################################################################
########### We group variables by level of correlation
################################################################################
