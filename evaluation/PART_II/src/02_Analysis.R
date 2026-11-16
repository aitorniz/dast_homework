################################################################################
#################################### 02_Analysis.R ##################
#################################### dast_homework #############################
#################################### Aitor Lopez ###############################
################################################################################

#dependencies
if(!require(DHARMa)) install.packages("DHARMa", ncpus = 15)
if(!require(ggeffects)) install.packages("ggeffects", ncpus = 15)
if(!require(ggplot2)) install.packages("ggplot2", ncpus = 15)
if(!require(tidyverse)) install.packages("tidyverse", ncpus = 15)
if(!require(dplyr)) install.packages("dplyr", ncpus = 15)
if(!require(factoextra)) install.packages("factoextra", ncpus = 15)
if(!require(MASS)) install.packages("MASS", ncpus = 15)
if(!require(ggpubr)) install.packages("ggpubr", ncpus = 15)
if(!require(magrittr)) install.packages("magrittr", ncpus = 15)
if(!require(rstatix)) install.packages("rstatix", ncpus = 15)
if(!require(corrplot)) install.packages("corrplot", ncpus = 15)
if(!require(Hmisc)) install.packages("Hmisc", ncpus = 15)
if(!require(GGally)) install.packages("GGally", ncpus = 15)
if(!require(RColorBrewer)) install.packages("RColorBrewer", ncpus = 15)
if(!require(reshape2)) install.packages("reshape2", ncpus = 15)
if(!require(lme4)) install.packages("lme4", ncpus = 15)
if(!require(gam)) install.packages("gam", ncpus = 15)
if(!require(sf)) install.packages("sf", ncpus = 15)


library(DHARMa)
library(ggeffects)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(factoextra)
library(magrittr)
library(rstatix)
library(corrplot)
library(Hmisc)
library(GGally)
library(RColorBrewer)
library(reshape2)
library(lme4)
library(gam)
library(sf)
#load the data
dat <- readRDS("./data/lopez/processed/dat.rds")
#avoid erasing thanks to value passing

#------------------------------------------------------------------------------#
NFI_SURVEY_RAW <- data.frame(dat)
NFI_SURVEY_FILTER <- subset(NFI_SURVEY_RAW, select = -c(ser_86, 
                                             espar))
NFI_SURVEY_NUM <- data.frame(lapply(NFI_SURVEY_FILTER, as.numeric))
NFI_SURVEY <- na.omit(NFI_SURVEY_NUM)

SER_l93 <- st_read("./data/lopez/raw/ser_l93/ser_l93.shp")

#------------------------------------------------------------------------------#

boxplot.stats(NFI_SURVEY$biom)
NFI_SURVEY_cor <-cor_mat(NFI_SURVEY)
NFI_SURVEY_p <- rcorr(as.matrix(NFI_SURVEY))$P
diag(NFI_SURVEY_p) <- 0
corrplot(
  NFI_SURVEY_p, p.mat = NFI_SURVEY_p, sig.level = 0.15
  , insig = "blank",
  diag = FALSE, tl.col = "black", tl.srt = 45,
  col = colorRampPalette(c("#b2182b", "white", "#3a86d4"))(200)
)
col <- colorRampPalette(c("#b2182b", "white", "#3a86d4"))(20)

hm_nfi_survey <- as.matrix(NFI_SURVEY_cor[, -1])
rownames(hm_nfi_survey) <- NFI_SURVEY_cor$rowname
hm_raw <- ggplot()
hm_raw + heatmap(hm_nfi_survey,
                  scale = 'none',
                  col = col,
                  RowSideColors = rep(c("blue", "pink"), each = 16),
                  ColSideColors = rep(c("purple", "orange"), each = 16)
                  )

ggsave("./output/hm_raw.png", plot = hm_raw)


#------------------------------------------------------------------------------#

temperatures_df <- data.frame(NFI_SURVEY$tmoy_win_min,
                              NFI_SURVEY$tmoy_win_mean,
                              NFI_SURVEY$tmoy_win_max,
                              NFI_SURVEY$tmoy_spr_mi,
                              NFI_SURVEY$tmoy_spr_mean,
                              NFI_SURVEY$tmoy_spr_max,
                              NFI_SURVEY$tmoy_sum_min,
                              NFI_SURVEY$tmoy_sum_mean,
                              NFI_SURVEY$tmoy_sum_ma,
                              NFI_SURVEY$tmoy_aut_min,
                              NFI_SURVEY$tmoy_aut_mean,
                              NFI_SURVEY$tmoy_aut_max)
temperatures_df <- set_colnames(temperatures_df, c("Winter Min",
                                                   "Winter Mean",
                                                   "Winter Max",
                                                   "Spring Min",
                                                   "Spring Mean",
                                                   "Spring Max",
                                                   "Summer Min",
                                                   "Summer Mean",
                                                   "Summer Max",
                                                   "Autumn Min",
                                                   "Autumn Mean",
                                                   "Autumn Max"))
temperatures_df_melt <- melt(temperatures_df)    

T_anomaly_plot <- ggplot(data = temperatures_df_melt, aes(x = variable, y = value))
T_anomaly_plot + geom_boxplot() + 
  theme_bw() + 
  xlab("Season/Type")+ 
  ylab("Temperature")+
  labs(title = "Anomalies in temperature in °C per seasons",
       caption = "French National Forest Inventory",
       x = "Season/Type", 
       y = "T°C")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("./output/anomalies_C_season.png", plot = T_anomaly_plot )

NFI_SURVEY <- na.omit(NFI_SURVEY)

mT_basic <- lm(biom ~ tmoy_win_mean_anomaly + tmoy_win_min_anomaly + tmoy_win_max_anomaly, data = NFI_SURVEY, na.action = na.omit)
ss_basic <- simulateResiduals(mT_basic)
pp_basic_min <- predict_response(mT_basic, terms = "tmoy_win_min_anomaly")
pp_basic_mean <- predict_response(mT_basic, terms = "tmoy_win_mean_anomaly")
pp_basic_max <- predict_response(mT_basic, terms = "tmoy_win_max_anomaly")
plot(pp_basic_min, show_data = TRUE)
plot(pp_basic_mean, show_data = TRUE)
plot(pp_basic_max, show_data = TRUE)
plot(biom ~ tmoy_win_mean_anomaly + tmoy_win_min_anomaly + tmoy_win_max_anomaly, data = NFI_SURVEY, na.action = na.omit)
mT_basic_AIC <- stepAIC(mT_basic)
#                       Df Sum of Sq        RSS    AIC
#<none>                               5.6771e+10 103984
#- tmoy_win_max_anomaly   1  36472253 5.6808e+10 103986
#- tmoy_win_mean_anomaly  1  99999177 5.6871e+10 103993
#
mT_rd <- lm(biom ~ ., data = NFI_SURVEY)
mT_rd_best <- stepAIC(mT_rd)
#Df  Sum of Sq        RSS    AIC
#<none>                                 4.3802e+10 102324
#- tmoy_aut_min_anomaly    1   14231738 4.3816e+10 102325
#- tmoy_sum_min_anomaly    1   16397670 4.3819e+10 102325
#- tmoy_aut_max_anomaly    1   17427440 4.3820e+10 102325
#- mgmt_intensity          1   25277547 4.3828e+10 102326
#- tmoy_win_mean_anomaly   1   27540004 4.3830e+10 102326
#- tmoy_spr_mean_anomaly   1   27779226 4.3830e+10 102327
#- dep                     1   30366611 4.3833e+10 102327
#- dem1000_altitude        1   33367902 4.3836e+10 102327
#- soil_waterlogging_perm  1   38247886 4.3840e+10 102328
#- soil_swhc               1   73292549 4.3876e+10 102333
#- soil_CN                 1  105262512 4.3908e+10 102338
#- soil_pH                 1  136773305 4.3939e+10 102343
#- pro_psg                 1  185424742 4.3988e+10 102350
#- mean_dbh                1  292857494 4.4095e+10 102366
#- num_trees               1 1204961064 4.5007e+10 102499
#- competition_total       1 2098684207 4.5901e+10 102627

reg_basic_df <- data.frame(Predicted_value = predict(mT_rd_best),
                           Observed_value = NFI_SURVEY$biom)
ggplot(reg_basic_df, aes(x = Predicted_value, y = Observed_value))+
  geom_point() +
  xlab("Predictions from Temperature based random model") +
  ylab("Observations")

mT_non_hiera <- lm(biom ~ ser_86 + tmoy_win_mean_anomaly + tmoy_win_min_anomaly + tmoy_win_max_anomaly, data = NFI_SURVEY_RAW, na.action = na.omit)
summary(mT_non_hiera)
mT_hiera <- lmer(biom ~ tmoy_win_mean_anomaly + tmoy_win_min_anomaly + tmoy_win_max_anomaly + (1 | ser_86), data = NFI_SURVEY_RAW, na.action = na.omit)
summary(mT_hiera)
fixef(mT_hiera)
ranef(mT_hiera)
predict_response(mT_hiera, terms = "ser_86") %>%
  plot()
ser_var <- ranef(mT_hiera)
ser_df <- data.frame(ser_86 = rownames(ser_var$ser_86),
                     ser_eff = ser_var$ser_86[,1],
                     ser_var = attr(ser_var$ser_86, "postVar")[,,1:82])
head(ser_df)
biom_ser <- ggplot(ser_df, aes(x = fct_reorder(ser_86, ser_eff),
                   y = ser_eff, 
                   ymin = ser_eff - sqrt(ser_var),
                   ymax = ser_eff + sqrt(ser_var))) +
  geom_point() +
  geom_linerange() +
  coord_flip() +
  xlab("Sylvoecoregion") +
  ylab("Biomass volume (m3)") +
  labs(title = "Variation of the mean Biomass according to Sylvoecoregion",
       caption = "Source: French National Forest Inventory")

ggsave("./output/biom_ser.png", plot = biom_ser)


ser_df$nb_plot <- count(dat, ser_86)$n
ggplot(ser_df, aes(x = nb_plot, y = ser_var)) +
  geom_point()

predict_response(mT_hiera, terms = "ser_86", type = "random") %>%
  plot() +
  coord_flip()

ser_pred <- as.data.frame(ggeffects::predict_response(mT_hiera, terms = "ser_86", type = "random"))
ser_plot <- inner_join(ser_pred, SER_l93, by = c("x" = "codeser"))

ggplot(ser_plot, aes(fill = predicted, geometry = geometry)) +
  geom_sf() +
  scale_fill_distiller(palette = "Greens", direction = -1) +
  theme_bw()

mT_hiera_prop <- lmer(biom ~ (1 | pro_psg), data = NFI_SURVEY_RAW, na.action = na.omit)
fixef(mT_hiera_prop)
prop_var <- ranef(mT_hiera_prop)
prop_df <- data.frame(pro_psg = rownames(prop_var$pro_psg),
                      prop_eff = prop_var$pro_psg[,1],
                      prop_var = attr(prop_var$pro_psg, "postVar")[,,1:4])
head(prop_df)

ggplot(prop_df, aes(x = pro_psg,
                               y = prop_eff, 
                               ymin = prop_eff - sqrt(prop_var),
                               ymax = prop_eff + sqrt(prop_var))) +
  geom_point() +
  geom_linerange() +
  coord_flip() +
  xlab("Property type") +
  ylab("Biomass volume (m3)") +
  labs(title = "Variation of the mean Biomass according to ownership type",
       caption = "Source: French National Forest Inventory")

ggsave("./output/biom_prop.png")

m_hiera_nl <- glmer(biom ~ scale(tmoy_win_mean_anomaly) + 
                            scale(tmoy_win_min_anomaly) + 
                            scale(tmoy_win_max_anomaly) +
                            scale(dem1000_altitude) +
                            scale(soil_waterlogging_perm) +
                            scale(soil_swhc) +
                            scale(soil_CN) +
                            scale(soil_pH) +
                            scale(mean_dbh) +
                            scale(num_trees) +
                            scale(competition_total) +
                            (1|ser_86) +
                            (1|dep) +
                            (1| mgmt_intensity) +
                            (1 | pro_psg), data = NFI_SURVEY_RAW, na.action = na.omit)
pp_hiera_nl_pH <- predict_response(m_hiera_nl, terms = "soil_pH")
plot(pp_hiera_nl_pH)
pp_hiera_nl_win_mean <- predict_response(m_hiera_nl, terms = "tmoy_win_mean_anomaly")
plot(pp_hiera_nl_win_mean)
pp_hiera_nl_swhc <- predict_response(m_hiera_nl, terms = "soil_swhc")
plot(pp_hiera_nl_swhc)
pp_hiera_nl_nb_tree <- predict_response(m_hiera_nl, terms = "num_trees")
plot(pp_hiera_nl_nb_tree)
pp_hiera_nl_mean_dbh <- predict_response(m_hiera_nl, terms = "mean_dbh")
plot(pp_hiera_nl_mean_dbh)

