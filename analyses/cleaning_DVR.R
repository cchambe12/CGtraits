## 17 August 2018
# Duration of Vegetative Risk: Common Garden


# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Set Working Directory
setwd("~/Documents/git/CGtraits/analyses/input")
d <-read.csv("2018_CG_datasheet.csv", header=TRUE)

## Clean data
df<-gather(d, "date","bbch", -Ind, -Plot)
df<-na.omit(df)
df$date<-substr(df$date, 2,8)
df$date<-as.character(as.Date(df$date,"%m.%d.%y"))
df$doy<-yday(df$date)
df$species<-substr(df$Ind, 0,6)
df<-dplyr::select(df, -date)
df$species<-ifelse(df$species=="betpap", "BETPAP", df$species)
df$bbch<-gsub(",", " ", df$bbch, fixed=TRUE)


df<-df[!(df$bbch==""),]
dx<-separate(df, bbch, into = c("first", "second"), sep = " (?=[^ ]+$)")
dx<-separate(dx, first, into = c("first", "third"), sep = " (?=[^ ]+$)")

dx$bb<-NA
dx$bb<-ifelse(dx$first=="9" | dx$second=="9" | dx$third=="9", dx$doy, dx$bb)
dx$lo<-NA
dx$lo<-ifelse(dx$first=="19" | dx$second=="19" | dx$third=="19", dx$doy, dx$lo)

drisk<-dx%>%dplyr::select(Ind, Plot, bb, lo, species)
drisk<-drisk[!(is.na(drisk$bb) & is.na(drisk$lo)),]

