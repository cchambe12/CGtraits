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
df$species<-substr(df$Ind, 0,6)
df<-dplyr::select(df, -date)
df$species<-ifelse(df$species=="betpap", "BETPAP", df$species)
df$bbch<-gsub(",", " ", df$bbch, fixed=TRUE)


df<-df[!(df$bbch==""),]
dx<-separate(df, bbch, into = c("first", "second"), sep = " (?=[^ ]+$)")
dx<-separate(dx, first, into = c("first", "third"), sep = " (?=[^ ]+$)")

dx$bb<-NA
dx$bb<-ifelse(dx$first=="9" | dx$first=="11" | dx$second=="9" | dx$second=="11" | dx$third=="9" | dx$third=="11", dx$doy, dx$bb)
dx$lo<-NA
dx$lo<-ifelse(dx$first=="19" | dx$second=="19" | dx$third=="19", dx$doy, dx$lo)

drisk<-dx%>%dplyr::select(Ind, Plot, bb, lo, species)
drisk<-drisk[!(is.na(drisk$bb) & is.na(drisk$lo)),]

bb<-drisk[!is.na(drisk$bb),]
bb$budburst<-ave(bb$bb, bb$Ind, bb$Plot, FUN=min)
bb<-subset(bb, select=c("Ind", "Plot", "budburst"))
bb<-bb[!duplicated(bb),]
lo<-drisk[!is.na(drisk$lo),]
lo$leafout<- ave(lo$lo, lo$Ind, lo$Plot, FUN=min) 
lo<-subset(lo, select=c("Ind", "Plot", "leafout"))
lo<-lo[!duplicated(lo),]

dvr<-full_join(bb, lo)
dvr$risk<-dvr$leafout-dvr$budburst 

dvr<-na.omit(dvr)
### Starting here, need to check code... ####
dvr<- dvr[order(dvr$Ind, dvr$risk), ]
dvr$Ind<-paste(dvr$Ind, dvr$Plot, sep="_")
dvr$ind.risk<-paste(dvr$Ind, dvr$risk, sep=",")
days.btw <- Map(seq, dvr$budburst, dvr$leafout, by = 1)

dxx <- data.frame(ind.risk = rep.int(dvr$ind.risk, vapply(days.btw, length, 1L)), 
                  doy = do.call(c, days.btw))

dxx$Ind<-gsub(",.*", "", dxx$ind.risk)
dxx$risk<-gsub(".*,", "", dxx$ind.risk)
dxx<-dplyr::select(dxx, -ind.risk)
dxx$budburst<-ave(dxx$doy, dxx$Ind, FUN=min)
dxx$leafout<-ave(dxx$doy, dxx$Ind, FUN=max)

dvr<-dplyr::select(dvr, Ind, Plot)

dvr<-full_join(dxx, dvr)

### Let's add in traits data now!### 
traits<-read.csv("output/clean_traits.csv", header=TRUE)
traits$d.index<-traits$perim/(2*sqrt(traits$area*pi))
traits$Ind<-paste(traits$species, traits$site, traits$ind, traits$plot, sep="_")
traits$d.index<-ave(traits$d.index, traits$Ind)

test<-full_join(dvr, traits)
test$sla<-test$area/test$dr.wt

test<-subset(test, select=c("doy", "risk", "Ind", "budburst", "d.index", "species", "area", "sla"))
test<-test[!duplicated(test),]
test<-test[!(test$risk<0),]
test<-na.omit(test)
test$area<-ave(test$area, test$Ind)
test$sla<-ave(test$sla, test$Ind)
test$d.index<-ave(test$d.index, test$Ind)
test<-test[!duplicated(test),]


## Now bring in climate data...
cc<-read.csv("input/weldhill.csv", header=TRUE)
cc<-cc%>%
  rename(date.time=Eastern.daylight.time)
cc$date<-gsub("\\s* .*$", '', cc$date.time)
cc$date<- as.Date(cc$date, "%m/%d/%Y")
cc$year<-substr(cc$date, 0, 4)
cc$doy<-yday(cc$date)
cc$hour<-gsub("^.* \\s*|\\s*:.*$", '', cc$date.time)

cc<-dplyr::select(cc, Temp..F, date, year, doy, hour)
cc$tmin<-ave(cc$Temp..F, cc$date, FUN=min)
cc$tmin<-fahrenheit.to.celsius(cc$tmin)
cc$tmax<-ave(cc$Temp..F, cc$date, FUN=max)
cc$tmax<-fahrenheit.to.celsius(cc$tmax)
cc$tmean<-ave(cc$Temp..F, cc$date)
cc$tmean<-fahrenheit.to.celsius(cc$tmean)
cc$tchill<-ave(cc$Temp..F, cc$date, cc$hour)
cc$tchill<-fahrenheit.to.celsius(cc$tchill)

ccx<-dplyr::select(cc, -hour)
ccx<-ccx[!duplicated(ccx),]

ccx$tchill<-ifelse(ccx$tchill>=0&ccx$tchill<=5, 1, 0)
ccx$chill<-ave(
  ccx$tchill, ccx$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
ccx$chill.day<-ave(ccx$tchill, ccx$date, FUN = sum)

ccwarm<-ccx%>%dplyr::select(doy, year, tmean)
ccwarm<-ccwarm[!duplicated(ccwarm),]
ccwarm$twarm<-ccwarm$tmean
ccwarm$twarm<-ifelse(ccwarm$twarm>=5, ccwarm$twarm, 0)
ccwarm$gdd<-ave(
  ccwarm$twarm, ccwarm$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
ccx<-full_join(ccx, ccwarm)

ccfrz<-ccx%>%dplyr::select(doy, year, tmin)
ccfrz<-ccfrz[!duplicated(ccfrz),]
ccfrz$frost<-ifelse(ccfrz$tmin<=-2.2, 1, 0)
ccfrz$frost <- ave(
  ccfrz$frost, ccfrz$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
ccx<-full_join(ccx, ccfrz)
ccx<-dplyr::select(ccx, -Temp..F)
ccx$lat<-42.296
ccx$photo<-daylength(ccx$lat, ccx$dat)
ccx$chill<-ave(ccx$chill, ccx$doy, FUN=max)
ccx<-subset(ccx, ccx$year==2018)

ccx<-subset(ccx, select=c(doy, chill, tmean, photo, tmin))
ccx<-ccx[!duplicated(ccx),]

test$chill<-NA
test$force<-NA
test$photo<-NA
test$tmin<-NA

for(i in c(1:nrow(test))){
  for(j in c(1:nrow(ccx)))
    test$chill[i]<-ifelse(test$budburst[i]==ccx$doy[j], ccx$chill[j], test$chill[i])
}
for(i in c(1:nrow(test))){
  for(j in c(1:nrow(ccx)))
    test$force[i]<-ifelse(test$doy[i]==ccx$doy[j], ccx$tmean[j], test$force[i])
}
for(i in c(1:nrow(test))){
  for(j in c(1:nrow(ccx)))
    test$photo[i]<-ifelse(test$budburst[i]==ccx$doy[j], ccx$photo[j], test$photo[i])
}
for(i in c(1:nrow(test))){
  for(j in c(1:nrow(ccx)))
    test$tmin[i]<-ifelse(test$budburst[i]==ccx$doy[j], ccx$tmin[j], test$tmin[i])
}

test$force<-ave(test$force, test$Ind)
test$photo<-ave(test$photo, test$Ind)
test$frz<-ifelse(test$tmin<=-2.2, 1, 0)
test$frz<-ave(test$frz, test$Ind, FUN=max)
test<-dplyr::select(test, -doy)
test<-test[!duplicated(test),]

test$sm.sla<-test$sla/10
test$sm.chill<-test$chill/100
test$risk<-as.numeric(test$risk)
test$site<-substr(test$Ind, 8,9)
test$numinds<-ave(test$Ind, test$species, test$site, FUN=length)

x<-subset(test, test$numinds>=6)

dvr.stan<-subset(x, select=c("risk", "d.index", "budburst", "sm.sla", "frz", "species", "sm.chill", 
                            "force", "photo", "site"))
dvr.stan<-dvr.stan[!duplicated(dvr.stan),]
dvr.stan<-na.omit(dvr.stan)


## Maybe one model with serrations, bb date and frz and trichomes and another with classic force, chill and photo?
traits<-stan_glmer(risk ~ d.index + budburst + (d.index+budburst|species), 
                 data=dvr.stan, chains=4, control = list(max_treedepth = 12))

plotting <- as.data.frame(summary(traits)$summary)
simple<-plotting
simple$var<- rownames(simple)

ser.bud<-ggplot(dvr.stan, aes(x=d.index, y=budburst)) + geom_point(aes(col=as.factor(species))) + 
  facet_wrap(~species) + theme(legend.position = "none")

## Not sure how to do chilling...
bb<-brm(budburst~sm.chill+force+photo+(sm.chill+force+photo|species/site), data=dvr.stan, chains=4, control = list(max_treedepth = 12))
dvr<-brm(risk~sm.chill+force+photo+(sm.chill+force+photo|species), data=dvr.stan, chains=4, control = list(max_treedepth = 12))

quartz()
cf<-ggplot(dvr.stan, aes(x=sm.chill, y=force)) + geom_point(aes(col=as.factor(species))) + 
  facet_wrap(~species) + theme(legend.position = "none")
cp<-ggplot(dvr.stan, aes(x=sm.chill, y=force)) + geom_point(aes(col=as.factor(species))) + 
  facet_wrap(~species) + theme(legend.position = "none")
fp<-ggplot(dvr.stan, aes(x=force, y=photo)) + geom_point(aes(col=as.factor(species))) + 
  facet_wrap(~species) + theme(legend.position = "none")


phen<-as.data.frame(tidy(dvr,robust = TRUE))

bb<-stan_glmer(budburst~d.index+(d.index|species), data=dvr.stan)

fit2<-stan_glmer(risk~d.index+(1|species), data=dvr.stan)

library(brms)
fit.brm<-brm(risk~d.index+budburst+(d.index+budburst|species), data=dvr.stan)



