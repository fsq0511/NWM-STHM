#load libraries------
rm(list = ls(all.names = TRUE))
setwd("Q:/My Drive//")

{
  library(dataRetrieval)
  library(nhdplusTools)
  library(RNetCDF)
  library(sf)
  library(ztable)
  # library(plyr)      # for join(...)
  library(sdamr)
  library(lme4)
  library(sp)
  library(raster)
  library(rnoaa)
  library(lubridate)
  library(dplyr)
  library(GpGp)
  library(ggplot2)
  library(data.table)
  library(ggrepel)
  library(tidyverse)
  library(ggmap)
  library(cowplot)
  library(ggthemes)
  library(sbtools)
  library(data.table)
  library(maps)
  library(grid)
  library(ggpmisc)
  library(gridExtra)
  library(broom)
  library(ggforce)
  library(modelr)
  library(metR)
  '%!in%' <- function(x,y)!('%in%'(x,y))
  source('ForecastModel/otherfunctions.R', echo=TRUE)
  mapping    = readRDS("FlowForecast/mapping1.rds")
#colors to use----  
  BlueDarkRed18 <- read.csv("HABs/BlueDarkRed18.txt", sep = "")
  BlueDarkRed18 <- rgb(red = BlueDarkRed18$r, green = BlueDarkRed18$g, blue = BlueDarkRed18$b, maxColorValue = 255)
  col1 <-c("#FFFFEA", "#EAFFFF" ,"#BCF9FF" ,"#99EAFF" ,"#75D3FF", "#56B0FF", "#3D87FF" ,"#2857FF", "#181CF7", "#2400D8")
  cmp_b2r <- read.csv("colors/cmp_b2r.txt", sep = "")
  cmp_b2r <- rgb(red = cmp_b2r$r, green = cmp_b2r$g, blue = cmp_b2r$b, maxColorValue = 255)
  
  cmp_b2r_1 <- cmp_b2r[c(round(seq(1, 30, length.out = 9)), round(seq(35, 64, length.out = 8)))]
  cmp_b2r_2 <- cmp_b2r[c(round(seq(1, 30, length.out = 16)), round(seq(35, 64, length.out = 30)))]
# colorRamps::matlab.like2(n=3)
predictors <- readRDS("FlowForecast/predictors1.rds")

# get all USGS sites info-------
siteINFO <- readNWISsite(mapping$site_no)


huc2_south <- rgdal::readOGR(dsn = "HUC02", layer = "USCON_HUC02")
huc2_south@data$id <-huc2_south@data$OBJECTID    #rownames(huc2_south@data)
huc2south_Points <- fortify(huc2_south)
test <- rgeos::gCentroid(huc2_south,byid = T)
huc2_labels <- data.frame(test@coords) #huc2south_Points%>% group_by(id)%>%slice_head()%>% arrange(group )
huc2_labels$huc2=huc2_south@data$huc2 

rm(test,huc2_south)
HCDN_Station<-read.csv("FlowForecast/HCDN-2009_Station_Info.csv")
source("ForecastModel/extra_fun.R", echo=TRUE)

}


flow_data0 <- readRDS(file = "FlowForecast/flow_data_uscon_0822.rds")
gc()

#simple way to demonstrate
mod <- lme4::lmer(Flow_obs ~ Flow_NWM + (1+ Flow_NWM|daysev/regions), data=flow_data0)

# summary(mod)


#full model to run
modful <- lme4::lmer(Flow_obs_log ~ Flow_NWM_log +(1+ Flow_NWM_log|daysev/regions) + (1+ hucflow_3d_log|daysev/regions)+(1+mean.ai+total_storage+ mean.cor+imperv_per)|daysev/regions , data=flow_data0)
