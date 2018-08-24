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
setwd("~/Documents/git/CGtraits")
a<-read.csv("scans/Processed/Perim&Area.csv", header=TRUE)
b<-read.csv("scans/Processed/Results.csv", header=TRUE)
x<-full_join(a, b)

c<-read.csv("scans/Processed/Results2.csv", header=TRUE)
x<-full_join(x, c)

d<-read.csv("scans/Processed/Results3.csv", header=TRUE)
x<-full_join(x, d)

e<-read.csv("scans/Processed/resultsBETPOPS.csv", header=TRUE)
x<-full_join(x, e)

f<-read.csv("scans/Processed/ResultsDIELON.csv", header=TRUE)
x<-full_join(x, f)

g<-read.csv("scans/Processed/ResultsMYRGAL.csv", header=TRUE)
x<-full_join(x, g)

h<-read.csv("scans/Processed/ResultsSAMRAC2.csv", header=TRUE)
x<-full_join(x, h)


i<-read.csv("scans/Processed/ResultsSORAME.csv", header=TRUE)
x<-full_join(x, i)

j<-read.csv("scans/Processed/ResultsSPIALB.csv", header=TRUE)
x<-full_join(x, j)

k<-read.csv("scans/Processed/ResultsSPITOM.csv", header=TRUE)
x<-full_join(x, k)

l<-read.csv("scans/Processed/ResultsVIBCAS.csv", header=TRUE)
x<-full_join(x, l)

m<-read.csv("scans/Processed/SAMRACResults.csv", header=TRUE)
x<-full_join(x, m)

dups<-x[duplicated(x$Label),]

xx<-x[!duplicated(x$Label),]

xx$Label<-gsub('.jpg','',xx$Label)
xx$Label<-gsub('.jpeg','',xx$Label)

xx<-subset(xx, select=c("Label", "Area", "Perim.", "Angle", "X.1"))

xx<-xx[!(xx$Label=="DIELON_SH_4_P7"),]
xx$Label<-ifelse(xx$Label=="VACMYR_10C_1_P13", "VACMYR_GR_10C_1_P13", xx$Label)
xx$Label<-ifelse(xx$Label=="VACMYR_10C_2_P13", "VACMYR_GR_10C_2_P13", xx$Label)
xx$Label<-ifelse(xx$Label=="QUERUB_9_GR_1_P5", "QUERUB_GR_9_1_P5", xx$Label)
xx$Label<-ifelse(xx$Label=="QUERUB_9_GR_2_P5", "QUERUB_GR_9_2_P5", xx$Label)
xx$Angle<-ifelse(xx$Angle==0, NA, xx$Angle)
xx$X.1<-ifelse(xx$X.1==0, NA, xx$X.1)
xx$species<-substr(xx$Label, 0,6)
xx$Perim.<-ifelse(xx$species=="SAMRAC" & !(is.na(xx$X.1)), xx$X.1, xx$Perim.)
xx$Perim.<-ifelse(xx$species=="SORAME" & !(is.na(xx$Angle)), xx$Angle, xx$Perim.)

ap<-subset(xx, select=c("Label", "Area", "Perim.", "species"))
ap<-ap%>%rename(id=Label)%>%rename(area=Area)%>%rename(perim=Perim.)

ap$site<-substr(ap$id, 8,9)
ap$plot<-as.numeric(gsub(".*_P","",ap$id))
ap$id<-gsub('_P.*','',ap$id)

ap$id<-ifelse(ap$id=="ALNINC_SH_06_1", "ALNINC_SH_6_1", ap$id)
ap$id<-ifelse(ap$id=="ALNINC_SH_06_2", "ALNINC_SH_6_2", ap$id)
ap$id<-ifelse(ap$id=="BETPAP_SH_1_1", "BETPAP_SH_1B_1", ap$id)
ap$id<-ifelse(ap$id=="BETPAP_SH_1_2", "BETPAP_SH_1B_2", ap$id)

ap<-ap[!(ap$id=="SPITOM_GR_1_1" & ap$plot==11),]
ap<-ap[!(ap$id=="SPITOM_GR_1_2" & ap$plot==11),]

ap$id<-ifelse(ap$id=="SPITOM_GR_2D_1", "SPITOM_GR_2E_1", ap$id)
ap$id<-ifelse(ap$id=="SPITOM_GR_2D_2", "SPITOM_GR_2E_2", ap$id)
ap$id<-ifelse(ap$id=="SPITOM_GR_2C_1", "SPITOM_GR_2D_1", ap$id)
ap$id<-ifelse(ap$id=="SPITOM_GR_2C_2", "SPITOM_GR_2D_2", ap$id)
ap$id<-ifelse(ap$id=="SPITOM_GR_2B_1", "SPITOM_GR_2C_1", ap$id)
ap$id<-ifelse(ap$id=="SPITOM_GR_2B_2", "SPITOM_GR_2C_2", ap$id)
ap$id<-ifelse(ap$id=="SPITOM_GR_2A_1", "SPITOM_GR_2B_1", ap$id)
ap$id<-ifelse(ap$id=="SPITOM_GR_2A_2", "SPITOM_GR_2B_2", ap$id)
ap$id<-ifelse(ap$id=="SPITOM_SH_99_1" & ap$plot==11, "SPITOM_SH_99F_1", ap$id)
ap$id<-ifelse(ap$id=="SPITOM_SH_99_2" & ap$plot==11, "SPITOM_SH_99F_2", ap$id)
ap$id<-ifelse(ap$id=="VIBCAS_SH_1_1" & ap$plot==8, "VIBCAS_SH_1A_1", ap$id)
ap$id<-ifelse(ap$id=="VIBCAS_SH_1_2" & ap$plot==8, "VIBCAS_SH_1A_2", ap$id)

ap$ind<-NA
ap$ind<-ifelse(ap$site=="HF", gsub('.*HF_','',ap$id), ap$ind)
ap$ind<-ifelse(ap$site=="GR", gsub('.*GR_','',ap$id), ap$ind)
ap$ind<-ifelse(ap$site=="SH", gsub('.*SH_','',ap$id), ap$ind)
ap$ind<-ifelse(ap$site=="WM", gsub('.*WM_','',ap$id), ap$ind)

ap$leaf<-gsub(".*_","",ap$ind)
ap$ind<-gsub("_.*","",ap$ind)

### Load Weight Data ###
wt<-read.csv("analyses/input/CG_SLA.csv", header=TRUE)
wt$label<-paste(wt$IND, wt$Plot)
wt<-wt[!duplicated(wt$label),]

wt<-wt%>%rename(id=IND)%>%rename(plot=Plot)%>%rename(wet.wt=Wet.Weight)%>%rename(dr.wt=Dry.Mass)
wt<-subset(wt, select=c("id","plot", "wet.wt", "dr.wt"))
wt$plot<-as.numeric(wt$plot)

wt$id<-ifelse(wt$id=="*SPITOM_GR_2E_1***mislabeld coin envelope", "SPITOM_GR_2E_1", wt$id)
wt$id<-ifelse(wt$id=="**SPITOM_GR_2E_2", "SPITOM_GR_2E_2", wt$id)
wt$id<-ifelse(wt$id=="SPITOM_GR_2D_1 - change from C to D!", "SPITOM_GR_2D_1", wt$id)
wt$id<-ifelse(wt$id=="SPITOM_GR_2D_2 - change from C to D!!", "SPITOM_GR_2D_2", wt$id)
wt$id<-ifelse(wt$id=="SPITOM_GR_2B_1", "SPITOM_GR_2C_1", wt$id)
wt$id<-ifelse(wt$id=="SPITOM_GR_2B_2", "SPITOM_GR_2C_2", wt$id)
wt$id<-ifelse(wt$id=="SPITOM_GR_2A_1", "SPITOM_GR_2B_1", wt$id)
wt$id<-ifelse(wt$id=="SPITOM_GR_2A_1", "SPITOM_GR_2B_1", wt$id)


ap$wet.wt<-NA
ap$dr.wt<-NA
for(i in c(1:nrow(ap))){
  for(j in c(1:nrow(wt)))
    ap$wet.wt[i]<-ifelse(ap$id[i]==wt$id[j] & ap$plot[i]==wt$plot[j], wt$wet.wt[j], ap$wet.wt[i])
    
}

for(i in c(1:nrow(ap))){
  for(j in c(1:nrow(wt)))
    ap$dr.wt[i]<-ifelse(ap$id[i]==wt$id[j] & ap$plot[i]==wt$plot[j], wt$dr.wt[j], ap$dr.wt[i])
  
}

write.csv(ap, file="~/Documents/git/CGtraits/analyses/output/clean_traits.csv", row.names = FALSE)



