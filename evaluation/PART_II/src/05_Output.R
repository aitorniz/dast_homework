################################################################################
########################### 05_Output.R ########################################
########################### dast_homework ######################################
########################### Aitor Lopez ########################################
################################################################################

################################# Dependencies #################################
#------------------------------------------------------------------------------#
if(!require(dplyr)) install.packages("dplyr", ncpus = 15)
if(!require(sf)) install.packages("sf", ncpus = 15)
if(!require(ggplot2)) install.packages("ggplot2", ncpus = 15)
if(!require(units)) install.packages("units", ncpus = 15)

library(dplyr)
library(sf)
library(ggplot2)
library(units)
################################# Global variables #############################
#------------------------------------------------------------------------------#
NFI_SURVEY <- readRDS("./data/lopez/processed/dat.rds")
NFI_SURVEY <- NFI_SURVEY %>% rename_at('ser_86', ~'codeser')
SER_SHP_RAW <- st_read("./data/lopez/raw/ser_l93")
BIOM_DF <- NFI_SURVEY %>%
  group_by(codeser) %>%
  summarise(biom = sum(biom))
SER_SHP_RAW_BIOM <- left_join(SER_SHP_RAW,
                              BIOM_DF,
                              by = "codeser")
SER_SHP_RAW_BIOM$serAreaM2 <- with(SER_SHP_RAW_BIOM, st_area(SER_SHP_RAW_BIOM$geometry))
SER_SHP_RAW_BIOM$biomSerDensity <- with(SER_SHP_RAW_BIOM, biom/serAreaM2)
SER_SHP <- SER_SHP_RAW_BIOM 
#------------------------------------------------------------------------------#

################################################################################
########################### Plot biomass volume per SER ########################
################################################################################

ggplot() +
  geom_sf(data = SER_SHP ,
          aes(fill = SER_SHP$biomSerDensity)) +
  xlab("Latitude") +
  ylab("Longitude") +
  labs(title = "Biomass density across french sylvoecoregions",
       x = "Latitude", 
       y = "Longitude", 
       caption = "French National Forest Inventory Data", 
       fill = "Biomass density m3/m2") +
  #guides(color = guide_legend(reverse = TRUE)) +
  theme(
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11),
    legend.key.size = unit(0.9, "cm"),
    legend.background = element_rect(fill = "grey95", color = NA)
  )
ggsave("./output/biom_density.png")

