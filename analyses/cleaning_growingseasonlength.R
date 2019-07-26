## 25 July 2019
# Length of growing season 


# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(weathermetrics)
library(geosphere)
library(rstanarm)
library(brms)
library(rstan)

# Set Working Directory
setwd("~/Documents/git/CGtraits/analyses")
d <-read.csv("input/2018_CG_datasheet.csv", header=TRUE)

## Clean data
df<-gather(d, "date","bbch", -Ind, -Plot)
df<-na.omit(df)
df$date<-substr(df$date, 2,8)
df$date<-as.character(as.Date(df$date,"%m.%d.%y"))
df$doy<-yday(df$date)
df<-dplyr::select(df, -date)
df$bbch<-gsub(",", " ", df$bbch, fixed=TRUE)


df<-df[!(df$bbch==""),]
dx<-separate(df, bbch, into = c("first", "second"), sep = " (?=[^ ]+$)")
dx<-separate(dx, first, into = c("first", "third"), sep = " (?=[^ ]+$)")


dx$eos<-NA
dx$eos<-ifelse(dx$first=="99" | dx$first=="102" | dx$second=="99" | dx$second=="102" | dx$third=="99" | dx$third=="102", dx$doy, dx$eos)

dx <- dx[!is.na(dx$eos),]
dx$Individual <- dx$Ind

deos <- subset(dx, select=c("Individual", "Plot", "eos"))

#write.csv(deos, file="output/cg_endofseason.csv", row.names=FALSE)

