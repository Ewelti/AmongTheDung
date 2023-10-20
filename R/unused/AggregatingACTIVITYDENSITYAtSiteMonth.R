##Set working directory
#Ellen's working directory:
setwd("C:/Users/elwel/OneDrive/Desktop/AmongTheDung")
#Ben's working directory:

##load libraries
library(lme4)
library(stringr)
library(plotrix)
library(vegan)
library(BiodiversityR)
library(tidyverse)
library(modelr)
library(brms)

# attach dung beetle size data
dens <- read.csv("rawdata/DB_densities.csv")
head(dens)
##
dens$code <- paste(dens$trt,dens$site_specific,dens$month)
unique(dens$code)

###############################
#######calculate activity density estimates################

############CanPil
ests <- NULL
for(i in unique(dens$code)){
  sub <- dens[dens$code == i, ]
  fit <- brm(CanPil ~ 1 + (1|sample), data = sub)
	#pull out fixed effects
	fixed_95 <- fixef(fit, probs = c(0.05, 0.95))[1,]
	fixed_90 <- fixef(fit, probs = c(0.10, 0.90))[1,3:4]
	fixed_80 <- fixef(fit, probs = c(0.20, 0.80))[1,3:4]
  ests.i <- data.frame(code_spp = i, t(fixed_95), t(fixed_90), t(fixed_80))
  ests <- rbind(ests, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests

ests[c('trt', 'site', 'month')] <- str_split_fixed(ests$code, ' ', 3)
colnames(ests)[2] ="CanPil_ActDens_est"
colnames(ests)[3] ="CanPil_ActDens_SE"
ests$month <-as.numeric(ests$month)
write.csv(ests, "outputs/CanPil_ActDens.csv")
