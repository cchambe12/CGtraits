### Started 25 July 2019 - Cat
## Building stan models to investigate growth

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(RColorBrewer)
library(rstan)
library(brms)
library(broom)
library(egg)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Set working directory
setwd("~/Documents/git/microclimates/analyses")

## Load the data
cg <- read.csv("output/clean_phenandtraits_growthform.csv", header=TRUE)

cg19 <- subset(cg, cg$year==2019)
cg19 <- subset(cg19, select=c("ht.diff", "spp", "provenance.lat", "budburst", "leafout", "risk"))
cg19 <- cg19[!duplicated(cg19),]

if(FALSE){
cg19.risk <- cg19[!is.na(cg19$risk),]
ht.mod <- brm(ht.diff ~ risk + provenance.lat + (risk + provenance.lat|spp), data=cg,
              control=list(max_treedepth = 15,adapt_delta = 0.99),
              iter=4000, warmup=2500)
}

cg19.lo <- cg19[!is.na(cg19$leafout),]
ht.mod.lo <- brm(ht.diff ~ leafout + provenance.lat + (leafout + provenance.lat|spp), data=cg,
              control=list(max_treedepth = 15,adapt_delta = 0.99),
              iter=4000, warmup=2500)

save(ht.mod.lo, file="~/Documents/git/microclimates/analyses/stan/htcglo.Rdata")

if(FALSE){
cg19.bb <- cg19[!is.na(cg19$budburst),]
ht.mod.bb <- brm(ht.diff ~ budburst + provenance.lat + (budburst + provenance.lat|spp), data=cg,
                 control=list(max_treedepth = 15,adapt_delta = 0.99),
                 iter=4000, warmup=2500)
}





