##Set working directory
#Ellen's working directory:
setwd("C:/Users/elwel/OneDrive/Desktop/AmongTheDung")
#Ben's working directory:

##load libraries
library(brms)
library(stringr)

# attach data
dungb <- read.csv("rawdata/DungersAll.csv")
head(dungb)
unique(dungb$species)
cp <- dungb[which(dungb$species=='Canthon_pilularius'),]
onF <- dungb[which(dungb$species=='Onthophagus_nuchicornis_female'),]
onM <- dungb[which(dungb$species=='Onthophagus_nuchicornis_male'),]
##
head(cp)
unique(cp$code)
boxplot(onM$horn.length..mm.~onM$trt)
boxplot(onM$horn.length..mm.~onM$site)
boxplot(onM$body.length..mm.~onM$trt)
boxplot(onF$body.length..mm.~onF$trt)
boxplot(cp$body.length..mm.~cp$trt)
boxplot(onM$wing.center..mm.~onM$trt)
boxplot(onF$wing.center..mm.~onF$trt)
boxplot(cp$wing.center..mm.~cp$trt)

#######calculate estimates
##Canthon body size
cp$trt_mo <- paste(cp$trt,cp$month)
ests_cp <- NULL
for(i in unique(cp$trt_mo)){
  sub <- cp[cp$trt_mo == i, ]
  fit <- brm(body.length..mm. ~ 1 + (1|trap:site), data = sub)
	#pull out fixed effects
	cp_fixed_95 <- fixef(fit, probs = c(0.05, 0.95))[1,]
	cp_fixed_90 <- fixef(fit, probs = c(0.10, 0.90))[1,3:4]
	cp_fixed_80 <- fixef(fit, probs = c(0.20, 0.80))[1,3:4]
  ests_cp.i <- data.frame(trt_mo = i, t(cp_fixed_95), t(cp_fixed_90), t(cp_fixed_80))
  ests_cp <- rbind(ests_cp, ests_cp.i) ; rm(ests_cp.i, sub)
} ; rm(i)
ests_cp

ests_cp[c('trt', 'month')] <- str_split_fixed(ests_cp$trt_mo, ' ', 2)
colnames(ests_cp)[2] ="body_mm_est"
colnames(ests_cp)[3] ="body_mm_SE"
ests_cp$month <-as.numeric(ests_cp$month)
ests_cp <- ests_cp[order(ests_cp$month,ests_cp$trt),]
ests_cp$mo_jit <- (ests_cp$month + c(-0.1,-0.2,0,0.2,0.1))
ests_cp$spp <- rep("CP", 15)

##
##Onthophagus_nuchicornis_female
onF$trt_mo <- paste(onF$trt,onM$month)
ests_onF <- NULL
for(i in unique(onF$trt_mo)){
  sub <- onF[onF$trt_mo == i, ]
  fit <- brm(body.length..mm. ~ 1 + (1|trap:site), data = sub)
	#pull out fixed effects
	onF_fixed_95 <- fixef(fit, probs = c(0.05, 0.95))[1,]
	onF_fixed_90 <- fixef(fit, probs = c(0.10, 0.90))[1,3:4]
	onF_fixed_80 <- fixef(fit, probs = c(0.20, 0.80))[1,3:4]
  ests_onF.i <- data.frame(trt_mo = i, t(onF_fixed_95), t(onF_fixed_90), t(onF_fixed_80))
  ests_onF <- rbind(ests_onF, ests_onF.i) ; rm(ests_onF.i, sub)
} ; rm(i)
ests_onF

ests_onF[c('trt', 'month')] <- str_split_fixed(ests_onF$trt_mo, ' ', 2)
colnames(ests_onF)[2] ="body_mm_est"
colnames(ests_onF)[3] ="body_mm_SE"
ests_onF$month <-as.numeric(ests_onF$month)
ests_onF <- ests_onF[order(ests_onF$month,ests_onF$trt),]
ests_onF$mo_jit <- (ests_onF$month + c(-0.1,-0.2,0,0.2,0.1))
ests_onF$spp <- rep("ONf", 15)

all <- rbind(ests_cp, ests_onF)

##
##Onthophagus_nuchicornis_male
onM$trt_mo <- paste(onM$trt,onM$month)
ests_onM <- NULL
for(i in unique(onM$trt_mo)){
  sub <- onM[onM$trt_mo == i, ]
  fit <- brm(body.length..mm. ~ 1 + (1|trap:site), data = sub)
	#pull out fixed effects
	onM_fixed_95 <- fixef(fit, probs = c(0.05, 0.95))[1,]
	onM_fixed_90 <- fixef(fit, probs = c(0.10, 0.90))[1,3:4]
	onM_fixed_80 <- fixef(fit, probs = c(0.20, 0.80))[1,3:4]
  ests_onM.i <- data.frame(trt_mo = i, t(onM_fixed_95), t(onM_fixed_90), t(onM_fixed_80))
  ests_onM <- rbind(ests_onM, ests_onM.i) ; rm(ests_onM.i, sub)
} ; rm(i)
ests_onM

ests_onM[c('trt', 'month')] <- str_split_fixed(ests_onM$trt_mo, ' ', 2)
colnames(ests_onM)[2] ="body_mm_est"
colnames(ests_onM)[3] ="body_mm_SE"
ests_onM$month <-as.numeric(ests_onM$month)
ests_onM <- ests_onM[order(ests_onM$month,ests_onM$trt),]
ests_onM$mo_jit <- (ests_onM$month + c(-0.1,-0.2,0,0.2,0.1))
ests_onM$spp <- rep("ONm", 15)

all <- rbind(all, ests_onM)
head(all)

write.csv(all, "outputs/BodySizeEsts_TrtMo.csv")
##########################################################################

#######calculate estimates for trt level
##Canthon body size
ests_cp <- NULL
for(i in unique(cp$trt)){
  sub <- cp[cp$trt == i, ]
  fit <- brm(body.length..mm. ~ 1 + (1|trap:site), data = sub)
	#pull out fixed effects
	cp_fixed_95 <- fixef(fit, probs = c(0.05, 0.95))[1,]
	cp_fixed_90 <- fixef(fit, probs = c(0.10, 0.90))[1,3:4]
	cp_fixed_80 <- fixef(fit, probs = c(0.20, 0.80))[1,3:4]
  ests_cp.i <- data.frame(trt = i, t(cp_fixed_95), t(cp_fixed_90), t(cp_fixed_80))
  ests_cp <- rbind(ests_cp, ests_cp.i) ; rm(ests_cp.i, sub)
} ; rm(i)
ests_cp

colnames(ests_cp)[2] ="body_mm_est"
colnames(ests_cp)[3] ="body_mm_SE"
ests_cp <- ests_cp[order(ests_cp$trt),]
ests_cp$spp <- rep("CP", 5)

##
##Onthophagus_nuchicornis_female
ests_onF <- NULL
for(i in unique(onF$trt)){
  sub <- onF[onF$trt == i, ]
  fit <- brm(body.length..mm. ~ 1 + (1|trap:site), data = sub)
	#pull out fixed effects
	onF_fixed_95 <- fixef(fit, probs = c(0.05, 0.95))[1,]
	onF_fixed_90 <- fixef(fit, probs = c(0.10, 0.90))[1,3:4]
	onF_fixed_80 <- fixef(fit, probs = c(0.20, 0.80))[1,3:4]
  ests_onF.i <- data.frame(trt = i, t(onF_fixed_95), t(onF_fixed_90), t(onF_fixed_80))
  ests_onF <- rbind(ests_onF, ests_onF.i) ; rm(ests_onF.i, sub)
} ; rm(i)
ests_onF

colnames(ests_onF)[2] ="body_mm_est"
colnames(ests_onF)[3] ="body_mm_SE"
ests_onF <- ests_onF[order(ests_onF$trt),]
ests_onF$spp <- rep("ONf", 5)

all <- rbind(ests_cp, ests_onF)

##
##Onthophagus_nuchicornis_male
ests_onM <- NULL
for(i in unique(onM$trt)){
  sub <- onM[onM$trt == i, ]
  fit <- brm(body.length..mm. ~ 1 + (1|trap:site), data = sub)
	#pull out fixed effects
	onM_fixed_95 <- fixef(fit, probs = c(0.05, 0.95))[1,]
	onM_fixed_90 <- fixef(fit, probs = c(0.10, 0.90))[1,3:4]
	onM_fixed_80 <- fixef(fit, probs = c(0.20, 0.80))[1,3:4]
  ests_onM.i <- data.frame(trt = i, t(onM_fixed_95), t(onM_fixed_90), t(onM_fixed_80))
  ests_onM <- rbind(ests_onM, ests_onM.i) ; rm(ests_onM.i, sub)
} ; rm(i)
ests_onM

colnames(ests_onM)[2] ="body_mm_est"
colnames(ests_onM)[3] ="body_mm_SE"
ests_onM <- ests_onM[order(ests_onM$trt),]
ests_onM$spp <- rep("ONm", 5)

all <- rbind(all, ests_onM)
head(all)

write.csv(all, "outputs/BodySizeEsts_TRT.csv")
########################################
