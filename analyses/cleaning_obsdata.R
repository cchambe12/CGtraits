## 16 July 2019 - Cat
# Aim is to investigate the relationship between growth traits and the duration of vegetative risk
# Below is code for 2018 and then initial code for 2019

## Housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

### Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

### First start with 2018 data...
# Set Working Directory
setwd("~/Documents/git/CGtraits/analyses")
cg18 <-read.csv("input/2018_CG_datasheet.csv", header=TRUE)

## Clean data
cg18<-gather(cg18, "date","bbch", -Ind, -Plot)
cg18<-na.omit(cg18)
cg18$date<-substr(cg18$date, 2,8)
cg18$date<-as.character(as.Date(cg18$date,"%m.%d.%y"))
cg18$doy<-yday(cg18$date)
cg18$species<-substr(cg18$Ind, 0,6)
cg18<-dplyr::select(cg18, -date)
cg18$species<-ifelse(cg18$species=="betpap", "BETPAP", cg18$species)
#cg18$bbch<-gsub(",", " ", cg18$bbch, fixed=TRUE)


cg18<-cg18[!(cg18$bbch==""),]
dx<-separate(cg18, bbch, into = c("first", "second"), sep = "\\,")
dx<-separate(dx, first, into = c("first", "third"), sep = "\\,")

#dx$first <- substr(dx$first, 0, 2)
#dx$second <- substr(dx$second, 0, 2)
#dx$third <- substr(dx$third, 0, 2)

dx$bb<-NA
dx$bb<-ifelse(dx$first=="9" | dx$first=="9-" | dx$first=="11" | dx$second=="9" | dx$second=="9-" |
                dx$second=="11" | dx$third=="9" | dx$third=="9-" | dx$third=="11", dx$doy, dx$bb)
dx$lo<-NA
dx$lo<-ifelse(dx$first=="19" | dx$second=="19" | dx$third=="19", dx$doy, dx$lo)

dx$first <- as.numeric(dx$first)
dx$second <- as.numeric(dx$second)
dx$third <- as.numeric(dx$third)
dx$flobuds<-NA
dx$flobuds<-ifelse(dx$first==c(51:59) | dx$second==c(51:59) | dx$third==c(51:59), dx$doy, dx$flobuds)

dx$flobudburst<-NA
dx$flobudburst<-ifelse(dx$first==c(60:63) | dx$second==c(60:63) | dx$third==c(60:63), dx$doy, dx$flobudburst)

dx$flowers<-NA
dx$flowers<-ifelse(dx$first==69 | dx$second==69 | dx$third==69, dx$doy, dx$flowers)

dx$fruit<-NA
dx$fruit<-ifelse(dx$first==c(70:73) | dx$second==c(70:73) | dx$third==c(70:73), dx$doy, dx$fruit)

dx$ripefruit<-NA
dx$ripefruit<-ifelse(dx$first==79 | dx$second==79 | dx$third==79, dx$doy, dx$ripefruit)

dx$budset<-NA
dx$budset<-ifelse(dx$first=="102" | dx$second=="102" | dx$third=="102", dx$doy, dx$budset)

drisk<-dx%>%dplyr::select(Ind, Plot, bb, lo, flobuds, flobudburst, flowers, fruit, ripefruit, budset, species)
#drisk<-drisk[!(is.na(drisk$bb) & is.na(drisk$lo)),]

bb<-drisk[!is.na(drisk$bb),]
bb$budburst<-ave(bb$bb, bb$Ind, bb$Plot, FUN=min)
bb<-subset(bb, select=c("Ind", "Plot", "budburst"))
bb<-bb[!duplicated(bb),]
lo<-drisk[!is.na(drisk$lo),]
lo$leafout<- ave(lo$lo, lo$Ind, lo$Plot, FUN=min) 
lo<-subset(lo, select=c("Ind", "Plot", "leafout"))
lo<-lo[!duplicated(lo),]
fbud<-drisk[!is.na(drisk$flobuds),]
fbud$flobuds<- ave(fbud$flobuds, fbud$Ind, fbud$Plot, FUN=min) 
fbud<-subset(fbud, select=c("Ind", "Plot", "flobuds"))
fbud<-fbud[!duplicated(fbud),]
fbb<-drisk[!is.na(drisk$fbb),]
fbb$flobudburst<- ave(fbb$flobudburst, fbb$Ind, fbb$Plot, FUN=min) 
fbb<-subset(fbb, select=c("Ind", "Plot", "flobudburst"))
fbb<-fbb[!duplicated(fbb),]
flos<-drisk[!is.na(drisk$flos),]
flos$flowers<- ave(flos$flowers, lo$Ind, lo$Plot, FUN=min) 
flos<-subset(flos, select=c("Ind", "Plot", "flowers"))
flos<-flos[!duplicated(flos),]
fru<-drisk[!is.na(drisk$fruit),]
fru$fruit<- ave(fru$fruit, fru$Ind, fru$Plot, FUN=min) 
fru<-subset(fru, select=c("Ind", "Plot", "fruit"))
fru<-fru[!duplicated(fru),]
ripe<-drisk[!is.na(drisk$ripefruit),]
ripe$ripefruit<- ave(ripe$ripefruit, lo$Ind, lo$Plot, FUN=min) 
ripe<-subset(ripe, select=c("Ind", "Plot", "ripefruit"))
ripe<-ripe[!duplicated(ripe),]
bset<-drisk[!is.na(drisk$budset),]
bset$budset<- ave(bset$budset, lo$Ind, lo$Plot, FUN=min) 
bset<-subset(bset, select=c("Ind", "Plot", "budset"))
bset<-bset[!duplicated(bset),]

cg18<-full_join(bb, lo)
cg18<-full_join(cg18, bset)
cg18$risk<-cg18$leafout-cg18$budburst 
cg18$year <- 2018
#write.csv(cg18, file="output/clean_cg_2018.csv", row.names=FALSE)

if(FALSE){
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
dvr$doy <- NULL
dvr <- dvr[!duplicated(dvr),]
dvr$year <- 2018
dvr <- dvr %>% rename(id = Ind)

dvr <- separate(data = dvr, col = id, into = c("spp", "site", "ind", "plot"), sep = "\\_")

## fix warning
dvr[is.na(dvr$plot), c("ind", "plot")] <- dvr[is.na(dvr$plot), c("plot", "ind")] 

dvr$year <- 2018
#dvr$last.obs <- 274
dvr$Plot <- NULL
dvr$risk <- as.numeric(dvr$risk)

#write.csv(dvr, file="output/dvr_cg_2018.csv", row.names=FALSE)


### Let's add in traits data now!### 
traits<-read.csv("output/clean_traits.csv", header=TRUE)

traits$d.index<-traits$perim/(2*sqrt(traits$area*pi))
traits$Ind<-paste(traits$species, traits$site, traits$ind, traits$plot, sep="_")
traits$d.index<-ave(traits$d.index, traits$Ind)
traits$plot <- as.character(traits$plot)

traits.clean<-full_join(dvr, traits)
traits.clean$sla<-traits.clean$area/traits.clean$dr.wt

traits.clean<-subset(traits.clean, select=c("id","budburst", "leafout", "risk", "d.index", "species", "area", "sla"))
traits.clean<-traits.clean[!duplicated(traits.clean),]
traits.clean<-traits.clean[!(traits.clean$risk<0),]
traits.clean<-na.omit(traits.clean)
traits.clean$area<-ave(traits.clean$area, traits.clean$id)
traits.clean$sla<-ave(traits.clean$sla, traits.clean$id)
traits.clean$d.index<-ave(traits.clean$d.index, traits.clean$id)
traits.clean<-traits.clean[!duplicated(traits.clean),]
}

### Now some starter code for 2019!
# Set Working Directory
setwd("~/Documents/git/wildhellgarden/analyses/") ## adjust as necessary!
cg19 <-read.csv("2019_data/2019_CG_dataupdated.csv", header=TRUE) 

## Now let's clean 2019 data
cg19$id <- paste(cg19$ID, cg19$Plot, sep="_")
cg19$Ind<-NULL
cg19$Plot<-NULL
cg19 <- gather(cg19, "date", "bbch", -id, -Phase)
cg19 <- na.omit(cg19)
cg19 <- cg19[!(cg19$bbch==""),]

cg19$date <- gsub("X", "", cg19$date)
cg19$date <- as.Date(cg19$date, format="%m.%d.%Y")
cg19$doy <- yday(cg19$date)

cg19leaves <- cg19[(cg19$Phase=="Leaves"),]

cg19leaves <- subset(cg19leaves, select=c("id", "doy", "bbch"))
cg19leaves <- separate(data = cg19leaves, col = id, into = c("spp", "site", "ind", "plot"), sep = "\\_")
cg19leaves$ind <- ifelse(is.na(cg19leaves$ind), substr(cg19leaves$spp, 7,8), cg19leaves$ind)
cg19leaves$ind <- ifelse(cg19leaves$ind=="", "XX", cg19leaves$ind)
cg19leaves$spp <- substr(cg19leaves$spp, 0, 6)
cg19leaves$year <- 2019

cg19leaves$bbch <- ifelse(cg19leaves$bbch==10, 9, cg19leaves$bbch)

cg19leaves <-cg19leaves%>% 
  group_by(spp, site, ind, plot, bbch, year) %>% 
  slice(which.min(doy))
cg19leaves<-cg19leaves[!duplicated(cg19leaves),]

cg19leaves$bb <- ifelse(cg19leaves$bbch==c(9:11), cg19leaves$doy, NA)
cg19leaves$lo <- ifelse(cg19leaves$bbch==19, cg19leaves$doy, NA)


cg19leaves$spindplot <- paste(cg19leaves$spp, cg19leaves$site, cg19leaves$ind, cg19leaves$plot)
for(i in c(unique(cg19leaves$spindplot))){ 
  
  budburst <- which(cg19leaves$bb[i==cg19leaves$spindplot])[1]
  cg19leaves$budburst[i==cg19leaves$spindplot] <- budburst
  
}
for(i in c(unique(cg19leaves$spindplot))){ 
  
  leafout <- which(cg19leaves$lo[i==cg19leaves$spindplot])[1]
  cg19leaves$leafout[i==cg19leaves$spindplot] <- leafout
  
}

cg19leaves <- subset(cg19leaves, select=c("spp", "year", "site", "ind", "budburst", "leafout", "plot"))
cg19leaves$plot <- as.character(cg19leaves$plot)

cg19budset <- cg19[(cg19$Phase=="Budset"),]

cg19budset <- subset(cg19budset, select=c("id", "doy", "bbch"))
cg19budset <- separate(data = cg19budset, col = id, into = c("spp", "site", "ind", "plot"), sep = "\\_")
cg19budset$ind <- ifelse(is.na(cg19budset$ind), substr(cg19budset$spp, 7,8), cg19budset$ind)
cg19budset$ind <- ifelse(cg19budset$ind=="", "XX", cg19budset$ind)
cg19budset$spp <- substr(cg19budset$spp, 0, 6)
cg19budset$year <- 2019

cg19budset <-cg19budset%>% 
  group_by(spp, site, ind, plot, bbch, year) %>% 
  slice(which.min(doy))
cg19budset<-cg19budset[!duplicated(cg19budset),]

cg19budset$bbch <- gsub("\\.", "\\,", cg19budset$bbch)
cg19budset <- separate(data = cg19budset, col = bbch, into = c("bbch", "bbchmf"), sep = "\\,")

cg19budset$budset <- ifelse(cg19budset$bbch==102, cg19budset$doy, NA)

cg19budset <- cg19budset %>% 
  group_by(spp, site, ind, plot, year) %>% 
  summarise_all(list(~first(na.omit(.))))

cg19budset <- subset(cg19budset, select=c("spp", "year", "site", "ind", "budset", "plot"))
cg19budset$plot <- as.character(cg19budset$plot)

cg19budset <- cg19budset[!duplicated(cg19budset),]

cg19 <- full_join(cg19leaves, cg19budset)

cg19flowers <- cg19[(cg19$Phase=="Flowers"),]

cg19flowers <- subset(cg19flowers, select=c("id", "doy", "bbch"))
cg19flowers <- separate(data = cg19flowers, col = id, into = c("spp", "site", "ind", "plot"), sep = "\\_")
cg19flowers$ind <- ifelse(is.na(cg19flowers$ind), substr(cg19flowers$spp, 7,8), cg19flowers$ind)
cg19flowers$ind <- ifelse(cg19flowers$ind=="", "XX", cg19flowers$ind)
cg19flowers$spp <- substr(cg19flowers$spp, 0, 6)
cg19flowers$year <- 2019

cg19flowers <-cg19flowers%>% 
  group_by(spp, site, ind, plot, bbch, year) %>% 
  slice(which.min(doy))
cg19flowers<-cg19flowers[!duplicated(cg19flowers),]

cg19flowers$bbch <- gsub("\\.", "\\,", cg19flowers$bbch)
cg19flowers <- separate(data = cg19flowers, col = bbch, into = c("bbch", "bbchmf"), sep = "\\,")

cg19flowers$flobuds <- ifelse(cg19flowers$bbch==c(51:59), cg19flowers$doy, NA)
cg19flowers$flobudburst <- ifelse(cg19flowers$bbch==c(60:62), cg19flowers$doy, NA)
cg19flowers$flowers <- ifelse(cg19flowers$bbch==69, cg19flowers$doy, NA)

cg19leaves$spindplot <- paste(cg19leaves$spp, cg19leaves$site, cg19leaves$ind, cg19leaves$plot)
for(i in c(unique(cg19flowers$spindplot))){ 
  
  flobuds <- which(cg19flowers$flobuds[i==cg19flowers$spindplot])[1]
  cg19floswer$flobuds[i==cg19flowers$spindplot] <- flobuds
  
}
for(i in c(unique(cg19flowers$spindplot))){ 
  
  flobudburst <- which(cg19flowers$flobudburst[i==cg19flowers$spindplot])[1]
  cg19floswer$flobudburst[i==cg19flowers$spindplot] <- flobudburst
  
}
for(i in c(unique(cg19flowers$spindplot))){ 
  
  flowers <- which(cg19flowers$flowers[i==cg19flowers$spindplot])[1]
  cg19floswer$flowers[i==cg19flowers$spindplot] <- flowers
  
}

cg19flowers <- subset(cg19flowers, select=c("spp", "year", "site", "ind", "flobuds", "flobudburst", "flowers", "plot"))
cg19flowers$plot <- as.character(cg19flowers$plot)

cg19flowers <- cg19flowers[!duplicated(cg19flowers),]

cg19 <- full_join(cg19, cg19flowers)

cg19fruits <- cg19[(cg19$Phase=="Flowers"),]

cg19fruits <- subset(cg19fruits, select=c("id", "doy", "bbch"))
cg19fruits <- separate(data = cg19fruits, col = id, into = c("spp", "site", "ind", "plot"), sep = "\\_")
cg19fruits$ind <- ifelse(is.na(cg19fruits$ind), substr(cg19fruits$spp, 7,8), cg19fruits$ind)
cg19fruits$ind <- ifelse(cg19fruits$ind=="", "XX", cg19fruits$ind)
cg19fruits$spp <- substr(cg19fruits$spp, 0, 6)
cg19fruits$year <- 2019

cg19fruits <-cg19fruits%>% 
  group_by(spp, site, ind, plot, bbch, year) %>% 
  slice(which.min(doy))
cg19fruits<-cg19fruits[!duplicated(cg19fruits),]

cg19fruits$bbch <- gsub("\\.", "\\,", cg19fruits$bbch)
cg19fruits <- separate(data = cg19fruits, col = bbch, into = c("bbch", "bbchmf"), sep = "\\,")

cg19fruits$fru <- ifelse(cg19fruits$bbch==c(70:74), cg19fruits$doy, NA)
cg19fruits$ripe <- ifelse(cg19fruits$bbch==79, cg19fruits$doy, NA)

cg19leaves$spindplot <- paste(cg19leaves$spp, cg19leaves$site, cg19leaves$ind, cg19leaves$plot)
for(i in c(unique(cg19fruits$spindplot))){ 
  
  fruit <- which(cg19fruits$fru[i==cg19fruits$spindplot])[1]
  cg19floswer$fruit[i==cg19fruits$spindplot] <- fruit
  
}
for(i in c(unique(cg19fruits$spindplot))){ 
  
  ripefruit <- which(cg19fruits$ripe[i==cg19fruits$spindplot])[1]
  cg19floswer$ripefruit[i==cg19fruits$spindplot] <- ripefruit
  
}

cg19fruits <- subset(cg19fruits, select=c("spp", "year", "site", "ind", "fruit", "ripefruitt", "plot"))
cg19fruits$plot <- as.character(cg19fruits$plot)

cg19fruits <- cg19fruits[!duplicated(cg19fruits),]
cg19 <- full_join(cg19, cg19fruits)

cg18 <- separate(data = cg18, col = Ind, into = c("spp", "site", "ind"), sep = "\\_")
cg18$plot <- as.character(cg18$Plot)
cg18$Plot <- NA

cg <- full_join(cg19, cg18)


### Now some starter code for 2020!
# Set Working Directory
setwd("~/Documents/git/wildhellgarden/analyses/") ## adjust as necessary!
cg20 <-read.csv("2020_data/2020_CG_datasheet.csv", header=TRUE) 

## Now let's clean 2019 data
cg20$id <- paste(cg20$ID, cg20$Plot, sep="_")
cg20$Ind<-NULL
cg20$Plot<-NULL
cg20 <- gather(cg20, "date", "bbch", -id, -Phase)
cg20 <- na.omit(cg20)
cg20 <- cg20[!(cg20$bbch==""),]

cg20$date <- gsub("X", "", cg20$date)
cg20$date <- as.Date(cg20$date, format="%m.%d.%Y")
cg20$doy <- yday(cg20$date)

cg20leaves <- cg20[(cg20$Phase=="Leaves"),]

cg20leaves <- subset(cg20leaves, select=c("id", "doy", "bbch"))
cg20leaves <- separate(data = cg20leaves, col = id, into = c("spp", "site", "ind", "plot"), sep = "\\_")
cg20leaves$ind <- ifelse(is.na(cg20leaves$ind), substr(cg20leaves$spp, 7,8), cg20leaves$ind)
cg20leaves$ind <- ifelse(cg20leaves$ind=="", "XX", cg20leaves$ind)
cg20leaves$spp <- substr(cg20leaves$spp, 0, 6)
cg20leaves$year <- 2020

cg20leaves$bbch <- ifelse(cg20leaves$bbch==10, 9, cg20leaves$bbch)

cg20leaves <-cg20leaves%>% 
  group_by(spp, site, ind, plot, bbch, year) %>% 
  slice(which.min(doy))
cg20leaves<-cg20leaves[!duplicated(cg20leaves),]

cg20leaves$bb <- ifelse(cg20leaves$bbch==c(9:11), cg20leaves$doy, NA)
cg20leaves$lo <- ifelse(cg20leaves$bbch==19, cg20leaves$doy, NA)


cg20leaves$spindplot <- paste(cg20leaves$spp, cg20leaves$site, cg20leaves$ind, cg20leaves$plot)
for(i in c(unique(cg20leaves$spindplot))){ 
  
  budburst <- which(cg20leaves$bb[i==cg20leaves$spindplot])[1]
  cg20leaves$budburst[i==cg20leaves$spindplot] <- budburst
  
}
for(i in c(unique(cg20leaves$spindplot))){ 
  
  leafout <- which(cg20leaves$lo[i==cg20leaves$spindplot])[1]
  cg20leaves$leafout[i==cg20leaves$spindplot] <- leafout
  
}

cg20leaves <- subset(cg20leaves, select=c("spp", "year", "site", "ind", "budburst", "leafout", "plot"))
cg20leaves$plot <- as.character(cg20leaves$plot)

cg20budset <- cg20[(cg20$Phase=="Budset"),]

cg20budset <- subset(cg20budset, select=c("id", "doy", "bbch"))
cg20budset <- separate(data = cg20budset, col = id, into = c("spp", "site", "ind", "plot"), sep = "\\_")
cg20budset$ind <- ifelse(is.na(cg20budset$ind), substr(cg20budset$spp, 7,8), cg20budset$ind)
cg20budset$ind <- ifelse(cg20budset$ind=="", "XX", cg20budset$ind)
cg20budset$spp <- substr(cg20budset$spp, 0, 6)
cg20budset$year <- 2020

cg20budset <-cg20budset%>% 
  group_by(spp, site, ind, plot, bbch, year) %>% 
  slice(which.min(doy))
cg20budset<-cg20budset[!duplicated(cg20budset),]

cg20budset$bbch <- gsub("\\.", "\\,", cg20budset$bbch)
cg20budset <- separate(data = cg20budset, col = bbch, into = c("bbch", "bbchmf"), sep = "\\,")

cg20budset$budset <- ifelse(cg20budset$bbch==102, cg20budset$doy, NA)

cg20budset <- cg20budset %>% 
  group_by(spp, site, ind, plot, year) %>% 
  summarise_all(list(~first(na.omit(.))))

cg20budset <- subset(cg20budset, select=c("spp", "year", "site", "ind", "budset", "plot"))
cg20budset$plot <- as.character(cg20budset$plot)

cg20budset <- cg20budset[!duplicated(cg20budset),]

cg20 <- full_join(cg20leaves, cg20budset)

cg20flowers <- cg20[(cg20$Phase=="Flowers"),]

cg20flowers <- subset(cg20flowers, select=c("id", "doy", "bbch"))
cg20flowers <- separate(data = cg20flowers, col = id, into = c("spp", "site", "ind", "plot"), sep = "\\_")
cg20flowers$ind <- ifelse(is.na(cg20flowers$ind), substr(cg20flowers$spp, 7,8), cg20flowers$ind)
cg20flowers$ind <- ifelse(cg20flowers$ind=="", "XX", cg20flowers$ind)
cg20flowers$spp <- substr(cg20flowers$spp, 0, 6)
cg20flowers$year <- 2020

cg20flowers <-cg20flowers%>% 
  group_by(spp, site, ind, plot, bbch, year) %>% 
  slice(which.min(doy))
cg20flowers<-cg20flowers[!duplicated(cg20flowers),]

cg20flowers$bbch <- gsub("\\.", "\\,", cg20flowers$bbch)
cg20flowers <- separate(data = cg20flowers, col = bbch, into = c("bbch", "bbchmf"), sep = "\\,")

cg20flowers$flobuds <- ifelse(cg20flowers$bbch==c(51:59), cg20flowers$doy, NA)
cg20flowers$flobudburst <- ifelse(cg20flowers$bbch==c(60:62), cg20flowers$doy, NA)
cg20flowers$flowers <- ifelse(cg20flowers$bbch==69, cg20flowers$doy, NA)

cg20flowers$spindplot <- paste(cg20flowers$spp, cg20flowers$site, cg20flowers$ind, cg20flowers$plot)
for(i in c(unique(cg20flowers$spindplot))){ 
  
  flobuds <- which(cg20flowers$flobuds[i==cg20flowers$spindplot])[1]
  cg20floswer$flobuds[i==cg20flowers$spindplot] <- flobuds
  
}
for(i in c(unique(cg20flowers$spindplot))){ 
  
  flobudburst <- which(cg20flowers$flobudburst[i==cg20flowers$spindplot])[1]
  cg20floswer$flobudburst[i==cg20flowers$spindplot] <- flobudburst
  
}
for(i in c(unique(cg20flowers$spindplot))){ 
  
  flowers <- which(cg20flowers$flowers[i==cg20flowers$spindplot])[1]
  cg20floswer$flowers[i==cg20flowers$spindplot] <- flowers
  
}

cg20flowers <- subset(cg20flowers, select=c("spp", "year", "site", "ind", "flobuds", "flobudburst", "flowers", "plot"))
cg20flowers$plot <- as.character(cg20flowers$plot)

cg20flowers <- cg20flowers[!duplicated(cg20flowers),]

cg20 <- full_join(cg20, cg20flowers)

cg20fruits <- cg20[(cg20$Phase=="Flowers"),]

cg20fruits <- subset(cg20fruits, select=c("id", "doy", "bbch"))
cg20fruits <- separate(data = cg20fruits, col = id, into = c("spp", "site", "ind", "plot"), sep = "\\_")
cg20fruits$ind <- ifelse(is.na(cg20fruits$ind), substr(cg20fruits$spp, 7,8), cg20fruits$ind)
cg20fruits$ind <- ifelse(cg20fruits$ind=="", "XX", cg20fruits$ind)
cg20fruits$spp <- substr(cg20fruits$spp, 0, 6)
cg20fruits$year <- 2020

cg20fruits <-cg20fruits%>% 
  group_by(spp, site, ind, plot, bbch, year) %>% 
  slice(which.min(doy))
cg20fruits<-cg20fruits[!duplicated(cg20fruits),]

cg20fruits$bbch <- gsub("\\.", "\\,", cg20fruits$bbch)
cg20fruits <- separate(data = cg20fruits, col = bbch, into = c("bbch", "bbchmf"), sep = "\\,")

cg20fruits$fru <- ifelse(cg20fruits$bbch==c(70:74), cg20fruits$doy, NA)
cg20fruits$ripe <- ifelse(cg20fruits$bbch==79, cg20fruits$doy, NA)

cg20leaves$spindplot <- paste(cg20leaves$spp, cg20leaves$site, cg20leaves$ind, cg20leaves$plot)
for(i in c(unique(cg20fruits$spindplot))){ 
  
  fruit <- which(cg20fruits$fru[i==cg20fruits$spindplot])[1]
  cg20floswer$fruit[i==cg20fruits$spindplot] <- fruit
  
}
for(i in c(unique(cg20fruits$spindplot))){ 
  
  ripefruit <- which(cg20fruits$ripe[i==cg20fruits$spindplot])[1]
  cg20floswer$ripefruit[i==cg20fruits$spindplot] <- ripefruit
  
}

cg20fruits <- subset(cg20fruits, select=c("spp", "year", "site", "ind", "fruit", "ripefruitt", "plot"))
cg20fruits$plot <- as.character(cg20fruits$plot)

cg20fruits <- cg20fruits[!duplicated(cg20fruits),]
cg20 <- full_join(cg20, cg20fruits)

cg <- full_join(cg, cg20)

cg$provenance.lat <- NA
cg$provenance.long <- NA

cg$provenance.lat <- ifelse(cg$site == "HF", 42.531705, cg$provenance.lat)
cg$provenance.long <- ifelse(cg$site == "HF", -72.189920, cg$provenance.long)
cg$provenance.lat <- ifelse(cg$site == "WM", 44.112337, cg$provenance.lat)
cg$provenance.long <- ifelse(cg$site == "WM", -71.230138, cg$provenance.long)
cg$provenance.lat <- ifelse(cg$site == "GR", 44.794942, cg$provenance.lat)
cg$provenance.long <- ifelse(cg$site == "GR", -71.146683, cg$provenance.long)
cg$provenance.lat <- ifelse(cg$site == "SH", 45.932675, cg$provenance.lat)
cg$provenance.long <- ifelse(cg$site == "SH", -74.025070, cg$provenance.long)


#write.csv(cg, file="~/Documents/git/CGtraits/analyses/output/clean_obs_bothyears.csv", row.names=FALSE)
#write.csv(cg, file="~/Documents/git/CGtraits/analyses/output/clean_obs_allyrs.csv", row.names=FALSE)



