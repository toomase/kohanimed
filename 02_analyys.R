library(extrafont)
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(scales)
library(ggthemes)
library(ggalt)
library(sp)
library(viridis)
library(gridExtra)


setwd("C:/Users/toomase/Dropbox/DataScience/R/kohanimed")

load("output/algandmed.RData")
source("R/eesti_hexabin.R")



# sõnad, mida otsida:
# -vere
# -pera
# -na
# -te
# ranna

vaike <- eesti_hexabin("väike")
suur <- eesti_hexabin("suur")

grid.arrange(vaike, suur, ncol=2)


