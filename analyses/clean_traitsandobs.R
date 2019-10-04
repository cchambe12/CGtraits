## 4 October 2019 - Cat
# Cleaning 2019 data and joining with 2018 data

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Set Working Directory
setwd("~/Documents/git/CGtraits")

sla2019 <- read.csv("analyses/input/SLA2019.csv")
sla2019$plot <- sla2019$Plot

sla2019$rep <- substr(sla2019$Individual,(nchar(sla2019$Individual)+1)-1,nchar(sla2019$Individual))
sla2019$id <- substr(sla2019$Individual,1,nchar(sla2019$Individual)-2)

sla2019$sla <- sla2019$area/sla2019$dr.wt
sla2019$sla.avg <- ave(sla2019$sla, sla2019$id, sla2019$plot)
sla2019$year <- 2019


sla2018 <- read.csv("analyses/output/clean_traits.csv")
sla2018$rep <- substr(sla2018$id,(nchar(sla2018$id)+1)-1,nchar(sla2018$id))
sla2018$id <- substr(sla2018$id,1,nchar(sla2018$id)-2)

sla2018$sla <- sla2018$area/sla2018$dr.wt
sla2018$sla.avg <- ave(sla2018$sla, sla2018$id, sla2018$plot)
sla2018$year <- 2018

sla <- full_join(sla2018, sla2019)

sla$spp <- sla$species
sla <- subset(sla, select=c("spp", "site", "ind", "plot", "year", "sla.avg"))
sla <- sla[!duplicated(sla),]

#### Okay, now let's add in the observation data...
obs <- read.csv("analyses/output/clean_obs_bothyears.csv")

cg.stan <- left_join(sla, obs, by=c("spp","site","ind","plot","year"))

library(brms)
sla.stan <- cg.stan[!is.na(cg.stan$sla.avg),]
mod <- brm(sla.avg ~ leafout + provenance.lat, data=sla.stan)




