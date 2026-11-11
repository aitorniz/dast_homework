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
if(!require(FactoMineR)) install.packages("FactoMineR", ncpus = 15)
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


library(DHARMa)
library(ggeffects)
library(ggplot2)
library(tidyverse)
library(FactoMineR)
library(dplyr)
library(factoextra)
library(MASS)
library(ggpubr)
library(magrittr)
library(rstatix)
library(corrplot)
library(Hmisc)
library(GGally)
library(RColorBrewer)
#load the data
dat <- readRDS("./data/lopez/processed/dat.rds")
#avoid erasing thanks to value passing

#------------------------------------------------------------------------------#
NFI_SURVEY <- data.frame(dat)
NFI_SURVEY_FILTER <- subset(NFI_SURVEY, select = -c(ser_86, 
                                             espar))
NFI_SURVEY_NUM <- data.frame(lapply(NFI_SURVEY_FILTER, as.numeric))



#------------------------------------------------------------------------------#

boxplot.stats(NFI_SURVEY_NUM$biom)
NFI_SURVEY_NUM_cor <-cor_mat(NFI_SURVEY_NUM)
NFI_SURVEY_NUM_p <- rcorr(as.matrix(NFI_SURVEY_NUM))$P
diag(NFI_SURVEY_NUM_p) <- 0
corrplot(
  NFI_SURVEY_NUM_p, p.mat = NFI_SURVEY_NUM_p, sig.level = 0.15
  , insig = "blank",
  diag = FALSE, tl.col = "black", tl.srt = 45,
  col = colorRampPalette(c("#b2182b", "white", "#3a86d4"))(200)
)
col <- colorRampPalette(c("#b2182b", "white", "#3a86d4"))(20)

hm_nfi_survey <- as.matrix(NFI_SURVEY_NUM_cor[, -1])
rownames(hm_nfi_survey) <- NFI_SURVEY_NUM_cor$rowname
hm_raw <- heatmap(hm_nfi_survey,
                  scale = 'none',
                  col = col,
                  RowSideColors = rep(c("blue", "pink"), each = 16),
                  ColSideColors = rep(c("purple", "orange"), each = 16))

ggsave("./output/hm_raw.png", plot = hm_raw)
