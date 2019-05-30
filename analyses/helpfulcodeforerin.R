## 30 May 2019- Cat
# Some initial example code for Erin to work with

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)

# Set Working Directory
setwd("~/Documents/git/flowersandfruits")

cg <-read.csv("input/2018_CG_datasheet.csv", header=TRUE)
cg <-read.csv("~/Documents/git/wildhellgarden/analyses/2019_data/2019_CG_datasheet.csv", header=TRUE)
