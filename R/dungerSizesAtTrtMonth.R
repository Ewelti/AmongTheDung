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

write.csv(all, "outputs/BodySizeEsts.csv")
########################################
######################
library(scales)
bs <- read.csv("outputs/BodySizeEsts.csv")

tiff(filename = "plots/ThreeSppBodySizes.tiff", width = 12, height = 4, units = 'in', res = 600, compression = 'lzw')

par(mar=c(2.5,5,0.4,0.2),mfrow=c(1,3))
plot(1, type="n", xlim=c(5.8,8.2), ylim=c(13.5,16.9),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8),cex.axis=1.1,labels=c("June","July","August"))
box(lwd=2)
title(ylab=substitute(paste(italic('Canthon praticola'), ' body size (mm)')), line=3, cex.lab=1.6)
##conecting lines
points(bs$body_mm_est[bs$trt=="bison" & bs$spp=="CP"] ~ bs$mo_jit[bs$trt=="bison" & bs$spp=="CP"],type="l",col="sienna",lwd=1)
points(bs$body_mm_est[bs$trt=="cattle" & bs$spp=="CP"] ~ bs$mo_jit[bs$trt=="cattle" & bs$spp=="CP"],type="l",col="gray20",lwd=1)
points(bs$body_mm_est[bs$trt=="ungrazed" & bs$spp=="CP"] ~ bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="CP"],type="l",col="dodgerblue",lwd=1)
points(bs$body_mm_est[bs$trt=="trtpd" & bs$spp=="CP"] ~ bs$mo_jit[bs$trt=="trtpd" & bs$spp=="CP"],type="l",col="firebrick2",lwd=1)
points(bs$body_mm_est[bs$trt=="untrtpd" & bs$spp=="CP"] ~ bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="CP"],type="l",col="goldenrod2",lwd=1)
##CIs
arrows(bs$mo_jit[bs$trt=="bison" & bs$spp=="CP"], bs$Q5[bs$trt=="bison" & bs$spp=="CP"], bs$mo_jit[bs$trt=="bison" & bs$spp=="CP"], bs$Q95[bs$trt=="bison" & bs$spp=="CP"],col=alpha("sienna",0.5),lwd=1,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="bison" & bs$spp=="CP"], bs$Q10[bs$trt=="bison" & bs$spp=="CP"], bs$mo_jit[bs$trt=="bison" & bs$spp=="CP"], bs$Q90[bs$trt=="bison" & bs$spp=="CP"],col=alpha("sienna",0.5),lwd=3,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="bison" & bs$spp=="CP"], bs$Q20[bs$trt=="bison" & bs$spp=="CP"], bs$mo_jit[bs$trt=="bison" & bs$spp=="CP"], bs$Q80[bs$trt=="bison" & bs$spp=="CP"],col=alpha("sienna",0.5),lwd=5,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="cattle" & bs$spp=="CP"], bs$Q5[bs$trt=="cattle" & bs$spp=="CP"], bs$mo_jit[bs$trt=="cattle" & bs$spp=="CP"], bs$Q95[bs$trt=="cattle" & bs$spp=="CP"],col=alpha("gray20",0.5),lwd=1,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="cattle" & bs$spp=="CP"], bs$Q10[bs$trt=="cattle" & bs$spp=="CP"], bs$mo_jit[bs$trt=="cattle" & bs$spp=="CP"], bs$Q90[bs$trt=="cattle" & bs$spp=="CP"],col=alpha("gray20",0.5),lwd=3,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="cattle" & bs$spp=="CP"], bs$Q20[bs$trt=="cattle" & bs$spp=="CP"], bs$mo_jit[bs$trt=="cattle" & bs$spp=="CP"], bs$Q80[bs$trt=="cattle" & bs$spp=="CP"],col=alpha("gray20",0.5),lwd=5,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="CP"], bs$Q5[bs$trt=="ungrazed" & bs$spp=="CP"], bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="CP"], bs$Q95[bs$trt=="ungrazed" & bs$spp=="CP"],col=alpha("dodgerblue",0.5),lwd=1,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="CP"], bs$Q10[bs$trt=="ungrazed" & bs$spp=="CP"], bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="CP"], bs$Q90[bs$trt=="ungrazed" & bs$spp=="CP"],col=alpha("dodgerblue",0.5),lwd=3,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="CP"], bs$Q20[bs$trt=="ungrazed" & bs$spp=="CP"], bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="CP"], bs$Q80[bs$trt=="ungrazed" & bs$spp=="CP"],col=alpha("dodgerblue",0.5),lwd=5,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="trtpd" & bs$spp=="CP"], bs$Q5[bs$trt=="trtpd" & bs$spp=="CP"], bs$mo_jit[bs$trt=="trtpd" & bs$spp=="CP"], bs$Q95[bs$trt=="trtpd" & bs$spp=="CP"],col=alpha("firebrick2",0.5),lwd=1,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="trtpd" & bs$spp=="CP"], bs$Q10[bs$trt=="trtpd" & bs$spp=="CP"], bs$mo_jit[bs$trt=="trtpd" & bs$spp=="CP"], bs$Q90[bs$trt=="trtpd" & bs$spp=="CP"],col=alpha("firebrick2",0.5),lwd=3,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="trtpd" & bs$spp=="CP"], bs$Q20[bs$trt=="trtpd" & bs$spp=="CP"], bs$mo_jit[bs$trt=="trtpd" & bs$spp=="CP"], bs$Q80[bs$trt=="trtpd" & bs$spp=="CP"],col=alpha("firebrick2",0.5),lwd=5,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="CP"], bs$Q5[bs$trt=="untrtpd" & bs$spp=="CP"], bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="CP"], bs$Q95[bs$trt=="untrtpd" & bs$spp=="CP"],col=alpha("goldenrod2",0.5),lwd=1,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="CP"], bs$Q10[bs$trt=="untrtpd" & bs$spp=="CP"], bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="CP"], bs$Q90[bs$trt=="untrtpd" & bs$spp=="CP"],col=alpha("goldenrod2",0.5),lwd=3,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="CP"], bs$Q20[bs$trt=="untrtpd" & bs$spp=="CP"], bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="CP"], bs$Q80[bs$trt=="untrtpd" & bs$spp=="CP"],col=alpha("goldenrod2",0.5),lwd=5,length=0, angle=90, code=3)
##points
points(bs$body_mm_est[bs$trt=="bison" & bs$spp=="CP"] ~ bs$mo_jit[bs$trt=="bison" & bs$spp=="CP"],pch=21,col="sienna",bg="sienna",cex=2.5)
points(bs$body_mm_est[bs$trt=="cattle" & bs$spp=="CP"] ~ bs$mo_jit[bs$trt=="cattle" & bs$spp=="CP"],pch=22,col="gray20",bg="gray20",cex=2.5)
points(bs$body_mm_est[bs$trt=="ungrazed" & bs$spp=="CP"] ~ bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="CP"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2.5)
points(bs$body_mm_est[bs$trt=="trtpd" & bs$spp=="CP"] ~ bs$mo_jit[bs$trt=="trtpd" & bs$spp=="CP"],pch=24,col="firebrick2",bg="firebrick2",cex=2.5)
points(bs$body_mm_est[bs$trt=="untrtpd" & bs$spp=="CP"] ~ bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="CP"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2.5)

##
##Onthophagus nuchicornis female
plot(1, type="n", xlim=c(5.8,8.2), ylim=c(7.22,8.29),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8),cex.axis=1.1,labels=c("June","July","August"))
box(lwd=2)
title(ylab=substitute(paste(italic('Onthophagus nuchicornis'), ' F body size (mm)')), line=3, cex.lab=1.6)
##conecting lines
points(bs$body_mm_est[bs$trt=="bison" & bs$spp=="ONf"] ~ bs$mo_jit[bs$trt=="bison" & bs$spp=="ONf"],type="l",col="sienna",lwd=1)
points(bs$body_mm_est[bs$trt=="cattle" & bs$spp=="ONf"] ~ bs$mo_jit[bs$trt=="cattle" & bs$spp=="ONf"],type="l",col="gray20",lwd=1)
points(bs$body_mm_est[bs$trt=="ungrazed" & bs$spp=="ONf"] ~ bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="ONf"],type="l",col="dodgerblue",lwd=1)
points(bs$body_mm_est[bs$trt=="trtpd" & bs$spp=="ONf"] ~ bs$mo_jit[bs$trt=="trtpd" & bs$spp=="ONf"],type="l",col="firebrick2",lwd=1)
points(bs$body_mm_est[bs$trt=="untrtpd" & bs$spp=="ONf"] ~ bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="ONf"],type="l",col="goldenrod2",lwd=1)
##CIs
arrows(bs$mo_jit[bs$trt=="bison" & bs$spp=="ONf"], bs$Q5[bs$trt=="bison" & bs$spp=="ONf"], bs$mo_jit[bs$trt=="bison" & bs$spp=="ONf"], bs$Q95[bs$trt=="bison" & bs$spp=="ONf"],col=alpha("sienna",0.5),lwd=1,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="bison" & bs$spp=="ONf"], bs$Q10[bs$trt=="bison" & bs$spp=="ONf"], bs$mo_jit[bs$trt=="bison" & bs$spp=="ONf"], bs$Q90[bs$trt=="bison" & bs$spp=="ONf"],col=alpha("sienna",0.5),lwd=3,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="bison" & bs$spp=="ONf"], bs$Q20[bs$trt=="bison" & bs$spp=="ONf"], bs$mo_jit[bs$trt=="bison" & bs$spp=="ONf"], bs$Q80[bs$trt=="bison" & bs$spp=="ONf"],col=alpha("sienna",0.5),lwd=5,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="cattle" & bs$spp=="ONf"], bs$Q5[bs$trt=="cattle" & bs$spp=="ONf"], bs$mo_jit[bs$trt=="cattle" & bs$spp=="ONf"], bs$Q95[bs$trt=="cattle" & bs$spp=="ONf"],col=alpha("gray20",0.5),lwd=1,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="cattle" & bs$spp=="ONf"], bs$Q10[bs$trt=="cattle" & bs$spp=="ONf"], bs$mo_jit[bs$trt=="cattle" & bs$spp=="ONf"], bs$Q90[bs$trt=="cattle" & bs$spp=="ONf"],col=alpha("gray20",0.5),lwd=3,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="cattle" & bs$spp=="ONf"], bs$Q20[bs$trt=="cattle" & bs$spp=="ONf"], bs$mo_jit[bs$trt=="cattle" & bs$spp=="ONf"], bs$Q80[bs$trt=="cattle" & bs$spp=="ONf"],col=alpha("gray20",0.5),lwd=5,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="ONf"], bs$Q5[bs$trt=="ungrazed" & bs$spp=="ONf"], bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="ONf"], bs$Q95[bs$trt=="ungrazed" & bs$spp=="ONf"],col=alpha("dodgerblue",0.5),lwd=1,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="ONf"], bs$Q10[bs$trt=="ungrazed" & bs$spp=="ONf"], bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="ONf"], bs$Q90[bs$trt=="ungrazed" & bs$spp=="ONf"],col=alpha("dodgerblue",0.5),lwd=3,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="ONf"], bs$Q20[bs$trt=="ungrazed" & bs$spp=="ONf"], bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="ONf"], bs$Q80[bs$trt=="ungrazed" & bs$spp=="ONf"],col=alpha("dodgerblue",0.5),lwd=5,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="trtpd" & bs$spp=="ONf"], bs$Q5[bs$trt=="trtpd" & bs$spp=="ONf"], bs$mo_jit[bs$trt=="trtpd" & bs$spp=="ONf"], bs$Q95[bs$trt=="trtpd" & bs$spp=="ONf"],col=alpha("firebrick2",0.5),lwd=1,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="trtpd" & bs$spp=="ONf"], bs$Q10[bs$trt=="trtpd" & bs$spp=="ONf"], bs$mo_jit[bs$trt=="trtpd" & bs$spp=="ONf"], bs$Q90[bs$trt=="trtpd" & bs$spp=="ONf"],col=alpha("firebrick2",0.5),lwd=3,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="trtpd" & bs$spp=="ONf"], bs$Q20[bs$trt=="trtpd" & bs$spp=="ONf"], bs$mo_jit[bs$trt=="trtpd" & bs$spp=="ONf"], bs$Q80[bs$trt=="trtpd" & bs$spp=="ONf"],col=alpha("firebrick2",0.5),lwd=5,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="ONf"], bs$Q5[bs$trt=="untrtpd" & bs$spp=="ONf"], bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="ONf"], bs$Q95[bs$trt=="untrtpd" & bs$spp=="ONf"],col=alpha("goldenrod2",0.5),lwd=1,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="ONf"], bs$Q10[bs$trt=="untrtpd" & bs$spp=="ONf"], bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="ONf"], bs$Q90[bs$trt=="untrtpd" & bs$spp=="ONf"],col=alpha("goldenrod2",0.5),lwd=3,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="ONf"], bs$Q20[bs$trt=="untrtpd" & bs$spp=="ONf"], bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="ONf"], bs$Q80[bs$trt=="untrtpd" & bs$spp=="ONf"],col=alpha("goldenrod2",0.5),lwd=5,length=0, angle=90, code=3)
##points
points(bs$body_mm_est[bs$trt=="bison" & bs$spp=="ONf"] ~ bs$mo_jit[bs$trt=="bison" & bs$spp=="ONf"],pch=21,col="sienna",bg="sienna",cex=2.5)
points(bs$body_mm_est[bs$trt=="cattle" & bs$spp=="ONf"] ~ bs$mo_jit[bs$trt=="cattle" & bs$spp=="ONf"],pch=22,col="gray20",bg="gray20",cex=2.5)
points(bs$body_mm_est[bs$trt=="ungrazed" & bs$spp=="ONf"] ~ bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="ONf"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2.5)
points(bs$body_mm_est[bs$trt=="trtpd" & bs$spp=="ONf"] ~ bs$mo_jit[bs$trt=="trtpd" & bs$spp=="ONf"],pch=24,col="firebrick2",bg="firebrick2",cex=2.5)
points(bs$body_mm_est[bs$trt=="untrtpd" & bs$spp=="ONf"] ~ bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="ONf"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2.5)

##Onthophagus nuchicornis male
plot(1, type="n", xlim=c(5.8,8.2), ylim=c(7.48,9.2),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8),cex.axis=1.1,labels=c("June","July","August"))
box(lwd=2)
title(ylab=substitute(paste(italic('Onthophagus nuchicornis'), ' M body size (mm)')), line=3, cex.lab=1.6)
##conecting lines
points(bs$body_mm_est[bs$trt=="bison" & bs$spp=="ONm"] ~ bs$mo_jit[bs$trt=="bison" & bs$spp=="ONm"],type="l",col="sienna",lwd=1)
points(bs$body_mm_est[bs$trt=="cattle" & bs$spp=="ONm"] ~ bs$mo_jit[bs$trt=="cattle" & bs$spp=="ONm"],type="l",col="gray20",lwd=1)
points(bs$body_mm_est[bs$trt=="ungrazed" & bs$spp=="ONm"] ~ bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="ONm"],type="l",col="dodgerblue",lwd=1)
points(bs$body_mm_est[bs$trt=="trtpd" & bs$spp=="ONm"] ~ bs$mo_jit[bs$trt=="trtpd" & bs$spp=="ONm"],type="l",col="firebrick2",lwd=1)
points(bs$body_mm_est[bs$trt=="untrtpd" & bs$spp=="ONm"] ~ bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="ONm"],type="l",col="goldenrod2",lwd=1)
##CIs
arrows(bs$mo_jit[bs$trt=="bison" & bs$spp=="ONm"], bs$Q5[bs$trt=="bison" & bs$spp=="ONm"], bs$mo_jit[bs$trt=="bison" & bs$spp=="ONm"], bs$Q95[bs$trt=="bison" & bs$spp=="ONm"],col=alpha("sienna",0.5),lwd=1,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="bison" & bs$spp=="ONm"], bs$Q10[bs$trt=="bison" & bs$spp=="ONm"], bs$mo_jit[bs$trt=="bison" & bs$spp=="ONm"], bs$Q90[bs$trt=="bison" & bs$spp=="ONm"],col=alpha("sienna",0.5),lwd=3,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="bison" & bs$spp=="ONm"], bs$Q20[bs$trt=="bison" & bs$spp=="ONm"], bs$mo_jit[bs$trt=="bison" & bs$spp=="ONm"], bs$Q80[bs$trt=="bison" & bs$spp=="ONm"],col=alpha("sienna",0.5),lwd=5,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="cattle" & bs$spp=="ONm"], bs$Q5[bs$trt=="cattle" & bs$spp=="ONm"], bs$mo_jit[bs$trt=="cattle" & bs$spp=="ONm"], bs$Q95[bs$trt=="cattle" & bs$spp=="ONm"],col=alpha("gray20",0.5),lwd=1,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="cattle" & bs$spp=="ONm"], bs$Q10[bs$trt=="cattle" & bs$spp=="ONm"], bs$mo_jit[bs$trt=="cattle" & bs$spp=="ONm"], bs$Q90[bs$trt=="cattle" & bs$spp=="ONm"],col=alpha("gray20",0.5),lwd=3,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="cattle" & bs$spp=="ONm"], bs$Q20[bs$trt=="cattle" & bs$spp=="ONm"], bs$mo_jit[bs$trt=="cattle" & bs$spp=="ONm"], bs$Q80[bs$trt=="cattle" & bs$spp=="ONm"],col=alpha("gray20",0.5),lwd=5,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="ONm"], bs$Q5[bs$trt=="ungrazed" & bs$spp=="ONm"], bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="ONm"], bs$Q95[bs$trt=="ungrazed" & bs$spp=="ONm"],col=alpha("dodgerblue",0.5),lwd=1,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="ONm"], bs$Q10[bs$trt=="ungrazed" & bs$spp=="ONm"], bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="ONm"], bs$Q90[bs$trt=="ungrazed" & bs$spp=="ONm"],col=alpha("dodgerblue",0.5),lwd=3,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="ONm"], bs$Q20[bs$trt=="ungrazed" & bs$spp=="ONm"], bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="ONm"], bs$Q80[bs$trt=="ungrazed" & bs$spp=="ONm"],col=alpha("dodgerblue",0.5),lwd=5,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="trtpd" & bs$spp=="ONm"], bs$Q5[bs$trt=="trtpd" & bs$spp=="ONm"], bs$mo_jit[bs$trt=="trtpd" & bs$spp=="ONm"], bs$Q95[bs$trt=="trtpd" & bs$spp=="ONm"],col=alpha("firebrick2",0.5),lwd=1,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="trtpd" & bs$spp=="ONm"], bs$Q10[bs$trt=="trtpd" & bs$spp=="ONm"], bs$mo_jit[bs$trt=="trtpd" & bs$spp=="ONm"], bs$Q90[bs$trt=="trtpd" & bs$spp=="ONm"],col=alpha("firebrick2",0.5),lwd=3,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="trtpd" & bs$spp=="ONm"], bs$Q20[bs$trt=="trtpd" & bs$spp=="ONm"], bs$mo_jit[bs$trt=="trtpd" & bs$spp=="ONm"], bs$Q80[bs$trt=="trtpd" & bs$spp=="ONm"],col=alpha("firebrick2",0.5),lwd=5,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="ONm"], bs$Q5[bs$trt=="untrtpd" & bs$spp=="ONm"], bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="ONm"], bs$Q95[bs$trt=="untrtpd" & bs$spp=="ONm"],col=alpha("goldenrod2",0.5),lwd=1,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="ONm"], bs$Q10[bs$trt=="untrtpd" & bs$spp=="ONm"], bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="ONm"], bs$Q90[bs$trt=="untrtpd" & bs$spp=="ONm"],col=alpha("goldenrod2",0.5),lwd=3,length=0, angle=90, code=3)
arrows(bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="ONm"], bs$Q20[bs$trt=="untrtpd" & bs$spp=="ONm"], bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="ONm"], bs$Q80[bs$trt=="untrtpd" & bs$spp=="ONm"],col=alpha("goldenrod2",0.5),lwd=5,length=0, angle=90, code=3)
##points
points(bs$body_mm_est[bs$trt=="bison" & bs$spp=="ONm"] ~ bs$mo_jit[bs$trt=="bison" & bs$spp=="ONm"],pch=21,col="sienna",bg="sienna",cex=2.5)
points(bs$body_mm_est[bs$trt=="cattle" & bs$spp=="ONm"] ~ bs$mo_jit[bs$trt=="cattle" & bs$spp=="ONm"],pch=22,col="gray20",bg="gray20",cex=2.5)
points(bs$body_mm_est[bs$trt=="ungrazed" & bs$spp=="ONm"] ~ bs$mo_jit[bs$trt=="ungrazed" & bs$spp=="ONm"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2.5)
points(bs$body_mm_est[bs$trt=="trtpd" & bs$spp=="ONm"] ~ bs$mo_jit[bs$trt=="trtpd" & bs$spp=="ONm"],pch=24,col="firebrick2",bg="firebrick2",cex=2.5)
points(bs$body_mm_est[bs$trt=="untrtpd" & bs$spp=="ONm"] ~ bs$mo_jit[bs$trt=="untrtpd" & bs$spp=="ONm"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2.5)

legend("topleft",legend=c("Cattle","Bison","Trt PD","Untrt PD","Ungrazed"), bty="n", pt.cex=2,cex=1.3, pch=c(22,21,24,25,23), pt.bg=c("gray20","sienna","firebrick2","goldenrod2","dodgerblue"),col=c("gray20","sienna","firebrick2","goldenrod2","dodgerblue"))
##


dev.off()

##

##############horn to body ratio:
head(onM)
max(na.omit(onM$body.length..mm.))
## Onthophagus nuchicornis male horn
par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.4,10.2), ylim=c(0.2,1.82),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Horn length (mm)", line=3, cex.lab=1.6)
title(xlab="Body length (mm)", line=2.5, cex.lab=1.6)
##bison
points(onM$horn.length..mm.[onM$trt=="bison"] ~ onM$body.length..mm.[onM$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=0.5)
#cattle
points(onM$horn.length..mm.[onM$trt=="cattle"] ~ onM$body.length..mm.[onM$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=0.5)
#ungrazed
points(onM$horn.length..mm.[onM$trt=="ungrazed"] ~ onM$body.length..mm.[onM$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=0.5)
#trtpd
points(onM$horn.length..mm.[onM$trt=="trtpd"] ~ onM$body.length..mm.[onM$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=0.5)
#untrtpd
points(onM$horn.length..mm.[onM$trt=="untrtpd"] ~ onM$body.length..mm.[onM$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=0.5)

abline(lm(onM$horn.length..mm.[onM$trt=="bison"] ~ onM$body.length..mm.[onM$trt=="bison"]),col="sienna", lwd=2)
#cattle
abline(lm(onM$horn.length..mm.[onM$trt=="cattle"] ~ onM$body.length..mm.[onM$trt=="cattle"]),col="gray0", lwd=2)
#ungrazed
abline(lm(onM$horn.length..mm.[onM$trt=="ungrazed"] ~ onM$body.length..mm.[onM$trt=="ungrazed"]),col="dodgerblue", lwd=2)
#trtpd
abline(lm(onM$horn.length..mm.[onM$trt=="trtpd"] ~ onM$body.length..mm.[onM$trt=="trtpd"]),col="firebrick2", lwd=2)
#untrtpd
abline(lm(onM$horn.length..mm.[onM$trt=="untrtpd"] ~ onM$body.length..mm.[onM$trt=="untrtpd"]),col="goldenrod2", lwd=2)
legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##