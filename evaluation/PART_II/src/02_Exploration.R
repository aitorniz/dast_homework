################################################################################
#################################### 02_Exploration.R ##########################
#################################### dast_homework #############################
#################################### Aitor Lopez ###############################
################################################################################

#dependencies
if(!require(DHARMa)) install.packages("DHARMa", ncpus = 15)
if(!require(ggeffects)) install.packages("ggeffects", ncpus = 15)
if(!require(ggplot2)) install.packages("ggplot2", ncpus = 15)
library(DHARMa)
library(ggeffects)
library(ggplot2)

#load data
dat <- readRDS("./data/lopez/processed/dat.rds")

################################################################################
######################### Perform exploratory analysis #########################
################################################################################
head(data)
biomass <- data$biom

plot(data$idp, biomass, 
     xlab= "Plot identifier",
     ylab = "Biomass (t/ha)",
     main = "Plot id and biomass (t/ha)")
idp_to_biom <- lm(biomass ~ data$idp, data = dat)
idp_r_squared <- summary(idp_to_biom)$r.squared

plot(data$dep, biomass, 
       xlab= "Department code",
       ylab = "Biomass (t/ha)",
       main = "Department code and biomass (t/ha)")
dep_to_biom <- lm(biomass ~ data$dep, data = dat)
dep_r_squared <- summary(dep_to_biom)$r.squared

plot(data$ser_86, biomass, 
     xlab= "Sylvoecoregion code",
     ylab = "Biomass (t/ha)",
     main = "Sylvoecoregion code and biomass (t/ha)")
ser_86_to_biom <- lm(biomass ~ data$ser_86, data = dat)
ser_86_r_squared <- summary(ser_86_to_biom)$r.squared

plot(data$espar, biomass, 
     xlab= "Species code",
     ylab = "Biomass (t/ha)",
     main = "Species code and biomass (t/ha)")
espar_to_biom <- lm(biomass ~ data$espar, data = dat)
espar_r_squared <- summary(espar_to_biom)$r.squared

plot(data$biom, biomass, 
     xlab= "Biomass (t/ha)",
     ylab = "Biomass (t/ha)",
     main = "Biomass and itself (t/ha)")
biom_to_biom <- lm(biomass ~ data$biom, data = dat)
biom_r_squared <- summary(biom_to_biom)$r.squared

plot(data$pro_psg, biomass, 
     xlab= "Management code",
     ylab = "Biomass (t/ha)",
     main = "Management code and biomass (t/ha)")
pro_psg_to_biom <- lm(biomass ~ data$pro_psg, data = dat)
pro_psg_r_squared <- summary(pro_psg_to_biom)$r.squared

plot(data$num_trees, biomass, 
     xlab= "Number of trees",
     ylab = "Biomass (t/ha)",
     main = "Number of trees and biomass (t/ha)")
nb_tree_to_biom <- lm(biomass ~ data$num_trees, data = dat)
num_trees_r_squared <- summary(nb_tree_to_biom)$r.squared

plot(data$mean_dbh, biomass, 
     xlab = "Plot-level average diameter at breast height in m² / ha", 
     ylab = "Biomass (t/ha)",
     main = "Mean dbh and biomass")
mean_dbh_to_biom <- lm(biomass ~ data$mean_dbh, data = dat)
mean_dbh_r_squared <- summary(mean_dbh_to_biom)$r.squared

plot(data$competition_total, biomass, 
     xlab = "Plot-level total basal area in m²/ha", 
     ylab = "Biomass (t/ha)",
     main = "Basal-area and biomass")
competition_to_biom <- lm(biomass ~ data$competition_total, data = dat)
competition_r_squared <- summary(competition_to_biom)$r.squared

plot(data$dem1000_altitude, biomass, 
     xlab = "Plot elevation in meter above sea level", 
     ylab = "Biomass (t/ha)",
     main = "Elevation and biomass")
altitude_to_biom <- lm(biomass ~ data$dem1000_altitude, data = dat)
altitude_r_squared <- summary(altitude_to_biom)$r.squared
plot(data$dem1000_hillshade, biomass, 
     xlab = "Plot hillshade in radian", 
     ylab = "Biomass (t/ha)",
     main = "Hillshade and biomass")
hillshade_to_biom <- lm(biomass ~ data$dem1000_hillshade, data = dat)
hillshade_r_squared <- summary(hillshade_to_biom)$r.squared
plot(data$dem1000_roughness, biomass,
     xlab = "Plot roughness (m)",
     ylab = "Biomass (t/ha)",
     main = "Roughness and biomass")
roughness_to_biom <- lm(biomass ~ data$dem1000_roughness, data = dat)
roughness_r_squared <- summary(roughness_to_biom)$r.squared
plot(data$perc_cut, biomass,
     xlab = "Percentage of cutted trees from one visit to the next one",
     ylab = "Biomass (t/ha)",
     main = "Cut percentage and biomass")
perc_cut_to_biom <- lm(biomass ~ data$perc_cut, data = dat)
perc_cut_r_squared <- summary(perc_cut_to_biom)$r.squared
plot(data$cut_intensity_nfi, biomass,
     xlab = "Cutting intensity class",
     ylab = "Biomass (t/ha)",
     main = "Cut intensity class and biomass")
cut_intensity_to_biom <- lm(biomass ~ data$cut_intensity_nfi, data = dat)
cut_intensity_r_squared <- summary(cut_intensity_to_biom)$r.squared
plot(data$dist_road, biomass,
     xlab = "Distance to road class",
     ylab = "Biomass (t/ha)",
     main = "Distance to road class and biomass")
dist_road_to_biom <- lm(biomass ~ data$dist_road, data = dat)
dist_road_r_squared <- summary(dist_road_to_biom)$r.squared
plot(data$mgmt_intensity, biomass,
     xlab = "Management intensity class",
     ylab = "Biomass (t/ha)",
     main = "Management intensity class and biomass")
mngmt_intensity_to_biom <- lm(biomass ~ data$mgmt_intensity, data = dat)
mngmt_intensity_r_squared <- summary(mngmt_intensity_to_biom)$r.squared

############ Seasons
plot(data$tmoy_win_mean_anomaly, biomass,
     xlab = "Average Winter T°C anomaly respect to mean",
     ylab = "Biomass (t/ha)",
     main = "Average Winter T°C anomaly respect to mean and biomass")
tmoy_win_mean_to_biom <- lm(biomass ~ data$tmoy_win_mean_anomaly, data = dat)
tmoy_win_mean_r_squared <- summary(tmoy_win_mean_to_biom)$r.squared

plot(data$tmoy_win_min_anomaly, biomass,
     xlab = "Average Winter T°C anomaly respect to minimum ",
     ylab = "Biomass (t/ha)",
     main = "Average T°C anomaly respect to the minimum and biomass")
tmoy_win_min_to_biom <- lm(biomass ~ data$tmoy_win_min_anomaly, data = dat)
tmoy_win_min_r_squared <- summary(tmoy_win_min_to_biom)$r.squared
plot(data$tmoy_win_max_anomaly, biomass,
     xlab = "Average Winter T°C anomaly respect to maximum ",
     ylab = "Biomass (t/ha)",
     main = "Average T°C anomaly respect to the maximum and biomass")
tmoy_win_max_to_biom <- lm(biomass ~ data$tmoy_win_max_anomaly, data = dat)
tmoy_win_max_r_squared <- summary(tmoy_win_max_to_biom)$r.squared
plot(data$tmoy_spr_mean_anomaly, biomass,
     xlab = "Average Spring T°C anomaly respect to mean ",
     ylab = "Biomass (t/ha)",
     main = "Average Spring T°C anomaly respect to the mean and biomass")
tmoy_spr_mean_to_biom <- lm(biomass ~ data$tmoy_spr_mean_anomaly, data = dat)
tmoy_spr_mean_r_squared <- summary(tmoy_spr_mean_to_biom)$r.squared
plot(data$tmoy_spr_min_anomaly, biomass,
     xlab = "Average Spring T°C anomaly respect to minimum ",
     ylab = "Biomass (t/ha)",
     main = "Average Spring T°C anomaly respect to the minimum and biomass")
tmoy_spr_min_to_biom <- lm(biomass ~ data$tmoy_spr_min_anomaly, data = dat)
tmoy_spr_min_r_squared <- summary(tmoy_spr_min_to_biom)$r.squared
plot(data$tmoy_spr_max_anomaly, biomass,
     xlab = "Average Spring T°C anomaly respect to maximum ",
     ylab = "Biomass (t/ha)",
     main = "Average Spring T°C anomaly respect to the maximum and biomass")
tmoy_spr_max_to_biom <- lm(biomass ~ data$tmoy_spr_max_anomaly, data = dat)
tmoy_spr_max_r_squared <- summary(tmoy_spr_max_to_biom)$r.squared

plot(data$tmoy_sum_mean_anomaly, biomass,
     xlab = "Average Summer T°C anomaly respect to the mean ",
     ylab = "Biomass (t/ha)",
     main = "Average Summer T°C anomaly respect to the mean and biomass")
tmoy_sum_mean_to_biom <- lm(biomass ~ data$tmoy_sum_mean_anomaly, data = dat)
tmoy_sum_mean_r_squared <- summary(tmoy_sum_mean_to_biom)$r.squared
plot(data$tmoy_sum_min_anomaly, biomass,
     xlab = "Average Summer T°C anomaly respect to the minimun ",
     ylab = "Biomass (t/ha)",
     main = "Average Summer T°C anomaly respect to the minimun and biomass")
tmoy_sum_min_to_biom <- lm(biomass ~ data$tmoy_sum_min_anomaly, data = dat)
tmoy_sum_min_r_squared <- summary(tmoy_sum_min_to_biom)$r.squared
plot(data$tmoy_sum_max_anomaly, biomass,
     xlab = "Average Summer T°C anomaly respect to the maximun ",
     ylab = "Biomass (t/ha)",
     main = "Average Summer T°C anomaly respect to the maximun and biomass")
tmoy_sum_max_to_biom <- lm(biomass ~ data$tmoy_sum_max_anomaly, data = dat)
tmoy_sum_max_r_squared <- summary(tmoy_sum_max_to_biom)$r.squared
plot(data$tmoy_aut_mean_anomaly, biomass,
     xlab = "Average Autumn T°C anomaly respect to the mean ",
     ylab = "Biomass (t/ha)",
     main = "Average Autumn T°C anomaly respect to the mean and biomass")
tmoy_aut_mean_to_biom <- lm(biomass ~ data$tmoy_aut_mean_anomaly, data = dat)
tmoy_aut_mean_r_squared <- summary(tmoy_aut_mean_to_biom)$r.squared
plot(data$tmoy_aut_min_anomaly, biomass,
     xlab = "Average Autumn T°C anomaly respect to the minimum ",
     ylab = "Biomass (t/ha)",
     main = "Average Autumn T°C anomaly respect to the minimun and biomass")
tmoy_aut_min_to_biom <- lm(biomass ~ data$tmoy_aut_min_anomaly, data = dat)
tmoy_aut_min_r_squared <- summary(tmoy_aut_min_to_biom)$r.squared

plot(data$tmoy_aut_max_anomaly, biomass,
     xlab = "Average Autumn T°C anomaly respect to the maximum ",
     ylab = "Biomass (t/ha)",
     main = "Average Autumn T°C anomaly respect to the maximum and biomass")
tmoy_aut_max_to_biom <- lm(biomass ~ data$tmoy_aut_max_anomaly, data = dat)
tmoy_aut_max_r_squared <- summary(tmoy_aut_max_to_biom)$r.squared
plot(data$soil_CN, biomass,
     xlab = "Bioindicated soil C/N ratio unitless",
     ylab = "Biomass (t/ha)",
     main = "C/N and biomass")
soil_CN_to_biom <- lm(biomass ~ data$soil_CN, data = dat)
soil_CN_r_squared <- summary(soil_CN_to_biom)$r.squared
plot(data$soil_pH, biomass,
     xlab = "Bioindicated soil pH",
     ylab = "Biomass (t/ha)",
     main = "pH and biomass")
soil_pH_to_biom <- lm(biomass ~ data$soil_pH, data = dat)
soil_pH_r_squared <- summary(soil_pH_to_biom)$r.squared
plot(data$soil_waterlogging_perm, biomass,
     xlab = "Permanent soil waterlogging in mm",
     ylab = "Biomass (t/ha)",
     main = "Soil permanent waterlogging and biomass")
soil_waterlogging_perm_to_biom <- lm(biomass ~ data$soil_waterlogging_perm, data = dat)
soil_waterlogging_perm_r_squared <- summary(soil_waterlogging_perm_to_biom)$r.squared
plot(data$soil_waterlogging_temp, biomass,
     xlab = "Temporary soil waterlogging in mm",
     ylab = "Biomass (t/ha)",
     main = "Soil temporary waterlogging and biomass")
soil_waterlogging_temp_to_biom <- lm(biomass ~ data$soil_waterlogging_temp, data = dat)
soil_waterlogging_temp_r_squared <- summary(soil_waterlogging_temp_to_biom)$r.squared
plot(data$soil_swhc, biomass,
     xlab = "Soil water holding capacity in mm",
     ylab = "Biomass (t/ha)",
     main = "Soil water holding capacity and biomass")
soil_swhc_to_biom <- lm(biomass ~ data$soil_swhc, data = dat)
soil_swhc_r_squared <- summary(soil_swhc_to_biom)$r.squared
plot(data$year, biomass,
     xlab = "Year",
     ylab = "Biomass (t/ha)",
     main = "Year and biomass")
year_to_biom <- lm(biomass ~ data$year, data = dat)
year_r_squared <- summary(year_to_biom)$r.squared

oneParamRsquaredVariables <- c("idp_r_squared",
                           "dep_r_squared",
                           "ser_86_r_squared",
                           "espar_r_squared",
                           "biom_r_squared",
                           "pro_psg_r_squared",
                           "num_trees_r_squared",
                           "mean_dbh_r_squared",
                           "competition_r_squared",
                           "altitude_r_squared",
                           "hillshade_r_squared",
                           "roughness_r_squared",
                           "perc_cut_r_squared",
                           "cut_intensity_r_squared",
                           "dist_road_r_squared",
                           "mngmt_intensity_r_squared",
                           "tmoy_win_min_r_squared",
                           "tmoy_win_mean_r_squared",
                           "tmoy_win_max_r_squared",
                           "tmoy_spr_min_r_squared",
                           "tmoy_spr_mean_r_squared",
                           "tmoy_spr_max_r_squared",
                           "tmoy_sum_min_r_squared",
                           "tmoy_sum_mean_r_squared",
                           "tmoy_sum_max_r_squared",
                           "tmoy_aut_min_r_squared",
                           "tmoy_aut_mean_r_squared",
                           "tmoy_aut_max_r_squared",
                           "soil_CN_r_squared",
                           "soil_pH_r_squared",
                           "soil_waterlogging_perm_r_squared",
                           "soil_waterlogging_temp_r_squared",
                           "soil_swhc_r_squared"
                           )
oneParamRsquaredList <- c(idp_r_squared,
                 dep_r_squared,
                 NaN,
                 NaN,
                 biom_r_squared,
                 pro_psg_r_squared,
                 num_trees_r_squared,
                 mean_dbh_r_squared,
                 competition_r_squared,
                 altitude_r_squared,
                 hillshade_r_squared,
                 roughness_r_squared,
                 perc_cut_r_squared,
                 cut_intensity_r_squared,
                 dist_road_r_squared,
                 mngmt_intensity_r_squared,
                 tmoy_win_min_r_squared,
                 tmoy_win_mean_r_squared,
                 tmoy_win_max_r_squared,
                 tmoy_spr_min_r_squared,
                 tmoy_spr_mean_r_squared,
                 tmoy_spr_max_r_squared,
                 tmoy_sum_min_r_squared,
                 tmoy_sum_mean_r_squared,
                 tmoy_sum_max_r_squared,
                 tmoy_aut_min_r_squared,
                 tmoy_aut_mean_r_squared,
                 tmoy_aut_max_r_squared,
                 soil_CN_r_squared,
                 soil_pH_r_squared,
                 soil_waterlogging_perm_r_squared,
                 soil_waterlogging_temp_r_squared,
                 soil_swhc_r_squared
                 )
oneParamRsquaredDf <- data.frame(oneParamRsquaredVariables, oneParamRsquaredList)
head(oneParamRsquaredDf)
summary(oneParamRsquaredDf)
oneParamRsquaredDfOrdered <- oneParamRsquaredDf[order(oneParamRsquaredDf$oneParamRsquaredList, decreasing = TRUE),]
head(oneParamRsquaredDfOrdered, 30)
saveRDS(oneParamRsquaredDfOrdered, file = "./data/lopez/processed/individual_r_squared_to_biom.rds")
################################################################################
