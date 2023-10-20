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

###############################
#######calculate activity density estimates for trt################

############CanPil
ests <- NULL
for(i in unique(dens$trt)){
  sub <- dens[dens$trt == i, ]
  fit <- brm(CanPil ~ 1 + (1|site_specific), data = sub)
	#pull out fixed effects
	fixed_95 <- fixef(fit, probs = c(0.05, 0.95))[1,]
	fixed_90 <- fixef(fit, probs = c(0.10, 0.90))[1,3:4]
	fixed_80 <- fixef(fit, probs = c(0.20, 0.80))[1,3:4]
  ests.i <- data.frame(code_spp = i, t(fixed_95), t(fixed_90), t(fixed_80))
  ests <- rbind(ests, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests

colnames(ests)[2] ="CanPil_ActDens_est"
colnames(ests)[3] ="CanPil_ActDens_SE"
write.csv(ests, "outputs/CanPil_TRT_ActDens.csv")

############OntNuc
ests <- NULL
for(i in unique(dens$trt)){
  sub <- dens[dens$trt == i, ]
  fit <- brm(OntNuc ~ 1 + (1|site_specific), data = sub)
	#pull out fixed effects
	fixed_95 <- fixef(fit, probs = c(0.05, 0.95))[1,]
	fixed_90 <- fixef(fit, probs = c(0.10, 0.90))[1,3:4]
	fixed_80 <- fixef(fit, probs = c(0.20, 0.80))[1,3:4]
  ests.i <- data.frame(code_spp = i, t(fixed_95), t(fixed_90), t(fixed_80))
  ests <- rbind(ests, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests

colnames(ests)[2] ="OntNuc_ActDens_est"
colnames(ests)[3] ="OntNuc_ActDens_SE"
write.csv(ests, "outputs/OntNuc_TRT_ActDens.csv")

###############################
#######calculate activity density estimates for trt & mo################

dens$trt_mo <- paste(dens$trt,dens$month)

############CanPil
ests <- NULL
for(i in unique(dens$trt_mo)){
  sub <- dens[dens$trt_mo == i, ]
  fit <- brm(CanPil ~ 1 + (1|site_specific), data = sub)
	#pull out fixed effects
	fixed_95 <- fixef(fit, probs = c(0.05, 0.95))[1,]
	fixed_90 <- fixef(fit, probs = c(0.10, 0.90))[1,3:4]
	fixed_80 <- fixef(fit, probs = c(0.20, 0.80))[1,3:4]
  ests.i <- data.frame(trt_mo = i, t(fixed_95), t(fixed_90), t(fixed_80))
  ests <- rbind(ests, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests
ests[c('trt', 'month')] <- str_split_fixed(ests$trt_mo, ' ', 2)

colnames(ests)[2] ="CanPil_ActDens_est"
colnames(ests)[3] ="CanPil_ActDens_SE"
write.csv(ests, "outputs/CanPil_TRTmo_ActDens.csv")

############OntNuc
ests <- NULL
for(i in unique(dens$trt_mo)){
  sub <- dens[dens$trt_mo == i, ]
  fit <- brm(OntNuc ~ 1 + (1|site_specific), data = sub)
	#pull out fixed effects
	fixed_95 <- fixef(fit, probs = c(0.05, 0.95))[1,]
	fixed_90 <- fixef(fit, probs = c(0.10, 0.90))[1,3:4]
	fixed_80 <- fixef(fit, probs = c(0.20, 0.80))[1,3:4]
  ests.i <- data.frame(trt_mo = i, t(fixed_95), t(fixed_90), t(fixed_80))
  ests <- rbind(ests, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests
ests[c('trt', 'month')] <- str_split_fixed(ests$trt_mo, ' ', 2)

colnames(ests)[2] ="OntNuc_ActDens_est"
colnames(ests)[3] ="OntNuc_ActDens_SE"
write.csv(ests, "outputs/OntNuc_TRTmo_ActDens.csv")