### 24 July 2019 - Cat

#### Housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

### Load libraries
library(RColorBrewer)
library(ggplot2)
library(egg)

### First start with 2018 data...
# Set Working Directory
setwd("~/Documents/git/CGtraits/analyses")
df <- read.csv("output/clean_phenandtraits_growthform.csv", header=TRUE)

colz <- colorRampPalette(brewer.pal(8,"Accent"))(7) ### you can mess with different palettes here: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes/

plot.lat <- ggplot(df, aes(x=prov.lat, y=ht.diff, col=spp)) + geom_point(aes(col=spp), alpha=0.2) +
  geom_smooth(aes(col=spp), method="lm", se=FALSE, position="jitter") + theme_classic() +
  xlab("Provenance latitude") + ylab("Height difference (cm)") + ggtitle("A. Your title here") +
  scale_color_manual(name="Species", values=colz,
                     labels=c("BETPAP"=expression(paste(italic("Betula papyrifera"))),
                              "BETPOP"=expression(paste(italic("Betula populifolia"))),
                              "BETALL"=expression(paste(italic("Betula alleghaniensis"))),
                              "ACEPEN"=expression(paste(italic("Acer pensylvanicum"))),
                              "ALNINC"=expression(paste(italic("Alnus incana"))),
                              "QUERUB"=expression(paste(italic("Quercus rubra"))),
                              "QUEALB"=expression(paste(italic("Quercus alba"))))) +
  theme(legend.position="none")

plot.dvr <- ggplot(trees, aes(x=budburst, y=ht.diff)) + geom_point(aes(col=spp), alpha=0.2) +
  geom_smooth(aes(col=spp), method="lm", se=FALSE) + theme_classic() +
  xlab("Day of budburst") + ylab("Height difference (cm)") + ggtitle("B. Your title here") +
  scale_color_manual(name="Species", values=colz,
                     labels=c("BETPAP"=expression(paste(italic("Betula papyrifera"))),
                              "BETPOP"=expression(paste(italic("Betula populifolia"))),
                              "BETALL"=expression(paste(italic("Betula alleghaniensis"))),
                              "ACEPEN"=expression(paste(italic("Acer pensylvanicum"))),
                              "ALNINC"=expression(paste(italic("Alnus incana"))),
                              "QUERUB"=expression(paste(italic("Quercus rubra"))),
                              "QUEALB"=expression(paste(italic("Quercus alba"))))) 

quartz()
ggarrange(plot.lat, plot.dvr, ncol=2)

