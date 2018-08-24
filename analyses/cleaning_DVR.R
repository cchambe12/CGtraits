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
library(lubridate)

# Set Working Directory
setwd("~/Documents/git/CGtraits/analyses")
d <-read.csv("input/2018_CG_datasheet.csv", header=TRUE)

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
dx$bb<-ifelse(dx$first=="9" | dx$first=="11" | dx$second=="9" | dx$second=="11" | dx$third=="9" | dx$third=="11" 
              | dx$first=="15" | dx$second=="15" | dx$third=="15", dx$doy, dx$bb)
dx$lo<-NA
dx$lo<-ifelse(dx$first=="19" | dx$second=="19" | dx$third=="19", dx$doy, dx$lo)

drisk<-dx%>%dplyr::select(Ind, Plot, bb, lo, species)
drisk<-drisk[!(is.na(drisk$bb) & is.na(drisk$lo)),]

bb<-drisk[!is.na(drisk$bb),]
bb$budburst<-ave(bb$bb, bb$Ind, bb$Plot, FUN=first)
bb<-subset(bb, select=c("Ind", "Plot", "budburst"))
bb<-bb[!duplicated(bb),]
lo<-drisk[!is.na(drisk$lo),]
lo$leafout<- ave(lo$lo, lo$Ind, lo$Plot, FUN=first) 
lo<-subset(lo, select=c("Ind", "Plot", "leafout"))
lo<-lo[!duplicated(lo),]

dvr<-full_join(bb, lo)
dvr$risk<-dvr$leafout-dvr$budburst 


### Let's add in traits data now!### 
traits<-read.csv("output/clean_traits.csv", header=TRUE)


