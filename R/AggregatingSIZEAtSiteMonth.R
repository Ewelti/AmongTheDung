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
dungb <- read.csv("rawdata/DungersAll.csv")
head(dungb)
unique(dungb$species)
dungb <- dungb[which(dungb$site_rep==1),]

#rename columns
colnames(dungb)[colnames(dungb) == "wing.center..mm."] ="wing"
colnames(dungb)[colnames(dungb) == "thorax.length..mm."] ="thorax"
colnames(dungb)[colnames(dungb) == "head.length..mm."] ="head"
colnames(dungb)[colnames(dungb) == "forearm.length..mm."] ="forearm"
colnames(dungb)[colnames(dungb) == "horn.length..mm."] ="horn"
colnames(dungb)[colnames(dungb) == "body.length..mm."] ="body"
head(dungb)
dim(dungb)

##
dungb$code_spp <- paste(dungb$trt,dungb$site_broadly,dungb$month,dungb$species)
unique(dungb$code_spp)

###############################STILL WORKING ON THIS SECTION TO UPDATE to BAYESIAN
#######calculate body size estimates
ests <- NULL
for(i in unique(dungb$code_spp)){
  sub <- dungb[dungb$code_spp == i, ]
  fit <- brm(body ~ 1 + (1|trap), data = sub)
	#pull out fixed effects
	fixed_95 <- fixef(fit, probs = c(0.05, 0.95))[1,]
	fixed_90 <- fixef(fit, probs = c(0.10, 0.90))[1,3:4]
	fixed_80 <- fixef(fit, probs = c(0.20, 0.80))[1,3:4]
  ests.i <- data.frame(code_spp = i, t(fixed_95), t(fixed_90), t(fixed_80))
  ests <- rbind(ests, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests

ests[c('trt', 'site', 'month', 'spp')] <- str_split_fixed(ests$code_spp, ' ', 4)
colnames(ests)[2] ="body_mm_est"
colnames(ests)[3] ="body_mm_SE"
ests$month <-as.numeric(ests$month)
write.csv(ests, "outputs/BodyEsts_SiteMonth.csv")

#######calculate wing size estimates
ests <- NULL
for(i in unique(dungb$code_spp)){
  sub <- dungb[dungb$code_spp == i, ]
  fit <- brm(wing ~ 1 + (1|trap), data = sub)
	#pull out fixed effects
	fixed_95 <- fixef(fit, probs = c(0.05, 0.95))[1,]
	fixed_90 <- fixef(fit, probs = c(0.10, 0.90))[1,3:4]
	fixed_80 <- fixef(fit, probs = c(0.20, 0.80))[1,3:4]
  ests.i <- data.frame(code_spp = i, t(fixed_95), t(fixed_90), t(fixed_80))
  ests <- rbind(ests, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests

ests[c('trt', 'site', 'month', 'spp')] <- str_split_fixed(ests$code_spp, ' ', 4)
colnames(ests)[2] ="wing_mm_est"
colnames(ests)[3] ="wing_mm_SE"
ests$month <-as.numeric(ests$month)
write.csv(ests, "outputs/WingEsts_SiteMonth.csv")

#######calculate thorax size estimates
ests <- NULL
for(i in unique(dungb$code_spp)){
  sub <- dungb[dungb$code_spp == i, ]
  fit <- brm(thorax ~ 1 + (1|trap), data = sub)
	#pull out fixed effects
	fixed_95 <- fixef(fit, probs = c(0.05, 0.95))[1,]
	fixed_90 <- fixef(fit, probs = c(0.10, 0.90))[1,3:4]
	fixed_80 <- fixef(fit, probs = c(0.20, 0.80))[1,3:4]
  ests.i <- data.frame(code_spp = i, t(fixed_95), t(fixed_90), t(fixed_80))
  ests <- rbind(ests, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests

ests[c('trt', 'site', 'month', 'spp')] <- str_split_fixed(ests$code_spp, ' ', 4)
colnames(ests)[2] ="thorax_mm_est"
colnames(ests)[3] ="thorax_mm_SE"
ests$month <-as.numeric(ests$month)
write.csv(ests, "outputs/ThoraxEsts_SiteMonth.csv")

#######calculate head size estimates
ests <- NULL
for(i in unique(dungb$code_spp)){
  sub <- dungb[dungb$code_spp == i, ]
  fit <- brm(head ~ 1 + (1|trap), data = sub)
	#pull out fixed effects
	fixed_95 <- fixef(fit, probs = c(0.05, 0.95))[1,]
	fixed_90 <- fixef(fit, probs = c(0.10, 0.90))[1,3:4]
	fixed_80 <- fixef(fit, probs = c(0.20, 0.80))[1,3:4]
  ests.i <- data.frame(code_spp = i, t(fixed_95), t(fixed_90), t(fixed_80))
  ests <- rbind(ests, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests

ests[c('trt', 'site', 'month', 'spp')] <- str_split_fixed(ests$code_spp, ' ', 4)
colnames(ests)[2] ="head_mm_est"
colnames(ests)[3] ="head_mm_SE"
ests$month <-as.numeric(ests$month)
write.csv(ests, "outputs/HeadEsts_SiteMonth.csv")

#######calculate forearm size estimates
ests <- NULL
for(i in unique(dungb$code_spp)){
  sub <- dungb[dungb$code_spp == i, ]
  fit <- brm(forearm ~ 1 + (1|trap), data = sub)
	#pull out fixed effects
	fixed_95 <- fixef(fit, probs = c(0.05, 0.95))[1,]
	fixed_90 <- fixef(fit, probs = c(0.10, 0.90))[1,3:4]
	fixed_80 <- fixef(fit, probs = c(0.20, 0.80))[1,3:4]
  ests.i <- data.frame(code_spp = i, t(fixed_95), t(fixed_90), t(fixed_80))
  ests <- rbind(ests, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests

ests[c('trt', 'site', 'month', 'spp')] <- str_split_fixed(ests$code_spp, ' ', 4)
colnames(ests)[2] ="forearm_mm_est"
colnames(ests)[3] ="forearm_mm_SE"
ests$month <-as.numeric(ests$month)
write.csv(ests, "outputs/ForearmEsts_SiteMonth.csv")

#######calculate horn size estimates
dbhorn <- dungb[complete.cases(dungb$horn), ]
head(dbhorn)
ests <- NULL
for(i in unique(dbhorn$code_spp)){
  sub <- dungb[dbhorn$code_spp == i, ]
  fit <- brm(horn ~ 1 + (1|trap), data = sub)
	#pull out fixed effects
	fixed_95 <- fixef(fit, probs = c(0.05, 0.95))[1,]
	fixed_90 <- fixef(fit, probs = c(0.10, 0.90))[1,3:4]
	fixed_80 <- fixef(fit, probs = c(0.20, 0.80))[1,3:4]
  ests.i <- data.frame(code_spp = i, t(fixed_95), t(fixed_90), t(fixed_80))
  ests <- rbind(ests, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests

ests[c('trt', 'site', 'month', 'spp')] <- str_split_fixed(ests$code_spp, ' ', 4)
colnames(ests)[2] ="horn_mm_est"
colnames(ests)[3] ="horn_mm_SE"
ests$month <-as.numeric(ests$month)
write.csv(ests, "outputs/HornEsts_SiteMonth.csv")

#######calculate horn size estimates for trt month level
dbhorn <- dungb[complete.cases(dungb$horn), ]
dungb$code_tm <- paste(dungb$trt,dungb$month)
unique(dungb$code_tm)
head(dbhorn)
ests <- NULL
for(i in unique(dbhorn$code_tm)){
  sub <- dungb[dbhorn$code_tm == i, ]
  fit <- brm(horn ~ 1 + (1|site:trap), data = sub)
	#pull out fixed effects
	fixed_95 <- fixef(fit, probs = c(0.05, 0.95))[1,]
	fixed_90 <- fixef(fit, probs = c(0.10, 0.90))[1,3:4]
	fixed_80 <- fixef(fit, probs = c(0.20, 0.80))[1,3:4]
  ests.i <- data.frame(code_tm = i, t(fixed_95), t(fixed_90), t(fixed_80))
  ests <- rbind(ests, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests

ests[c('trt', 'month')] <- str_split_fixed(ests$code_tm, ' ', 2)
colnames(ests)[2] ="horn_mm_est"
colnames(ests)[3] ="horn_mm_SE"
ests$month <-as.numeric(ests$month)
write.csv(ests, "outputs/HornEsts_TrtMonth.csv")