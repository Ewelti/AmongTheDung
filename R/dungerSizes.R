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

########################################
#calculate trt effects on pasture meter
mo <- lmer(body.length..mm.  ~  trt + (1|trap:site),data=cp)
summary(mo)

# extract coefficients
coefs <- data.frame(coef(summary(mo)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
######################
tiff(filename = "plots/ThreeSppBodySizes.tiff", width = 9, height = 4.5, units = 'in', res = 600, compression = 'lzw')

par(mar=c(4,5,0.2,0.2),mfrow=c(1,3))
plot(1, type="n", xlim=c(5.5,8.5), ylim=c(14,17),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8),cex.axis=1.1,labels=c("June","July","August"))
box(lwd=2)
title(ylab=substitute(paste(italic('Canthon praticola'), ' body size (mm)')), line=3, cex.lab=1.6)
##bison
points(ests_cp$body_mm_est[ests_cp$trt=="bison"] ~ ests_cp$mo_jit[ests_cp$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(ests_cp$body_mm_est[ests_cp$trt=="bison"] ~ ests_cp$mo_jit[ests_cp$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(ests_cp$mo_jit[ests_cp$trt=="bison"], ests_cp$body_mm_est[ests_cp$trt=="bison"]-ests_cp$body_mm_SE[ests_cp$trt=="bison"], ests_cp$mo_jit[ests_cp$trt=="bison"], ests_cp$body_mm_est[ests_cp$trt=="bison"]+ests_cp$body_mm_SE[ests_cp$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(ests_cp$body_mm_est[ests_cp$trt=="cattle"] ~ ests_cp$mo_jit[ests_cp$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(ests_cp$body_mm_est[ests_cp$trt=="cattle"] ~ ests_cp$mo_jit[ests_cp$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(ests_cp$mo_jit[ests_cp$trt=="cattle"], ests_cp$body_mm_est[ests_cp$trt=="cattle"]-ests_cp$body_mm_SE[ests_cp$trt=="cattle"], ests_cp$mo_jit[ests_cp$trt=="cattle"], ests_cp$body_mm_est[ests_cp$trt=="cattle"]+ests_cp$body_mm_SE[ests_cp$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(ests_cp$body_mm_est[ests_cp$trt=="ungrazed"] ~ ests_cp$mo_jit[ests_cp$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(ests_cp$body_mm_est[ests_cp$trt=="ungrazed"] ~ ests_cp$mo_jit[ests_cp$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(ests_cp$mo_jit[ests_cp$trt=="ungrazed"], ests_cp$body_mm_est[ests_cp$trt=="ungrazed"]-ests_cp$body_mm_SE[ests_cp$trt=="ungrazed"], ests_cp$mo_jit[ests_cp$trt=="ungrazed"], ests_cp$body_mm_est[ests_cp$trt=="ungrazed"]+ests_cp$body_mm_SE[ests_cp$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(ests_cp$body_mm_est[ests_cp$trt=="trtpd"] ~ ests_cp$mo_jit[ests_cp$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(ests_cp$body_mm_est[ests_cp$trt=="trtpd"] ~ ests_cp$mo_jit[ests_cp$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(ests_cp$mo_jit[ests_cp$trt=="trtpd"], ests_cp$body_mm_est[ests_cp$trt=="trtpd"]-ests_cp$body_mm_SE[ests_cp$trt=="trtpd"], ests_cp$mo_jit[ests_cp$trt=="trtpd"], ests_cp$body_mm_est[ests_cp$trt=="trtpd"]+ests_cp$body_mm_SE[ests_cp$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(ests_cp$body_mm_est[ests_cp$trt=="untrtpd"] ~ ests_cp$mo_jit[ests_cp$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(ests_cp$body_mm_est[ests_cp$trt=="untrtpd"] ~ ests_cp$mo_jit[ests_cp$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(ests_cp$mo_jit[ests_cp$trt=="untrtpd"], ests_cp$body_mm_est[ests_cp$trt=="untrtpd"]-ests_cp$body_mm_SE[ests_cp$trt=="untrtpd"], ests_cp$mo_jit[ests_cp$trt=="untrtpd"], ests_cp$body_mm_est[ests_cp$trt=="untrtpd"]+ests_cp$body_mm_SE[ests_cp$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

##
##Onthophagus nuchicornis female
plot(1, type="n", xlim=c(5.5,8.5), ylim=c(7.35,8.25),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8),cex.axis=1.1,labels=c("June","July","August"))
box(lwd=2)
title(ylab=substitute(paste(italic('Onthophagus nuchicornis'), ' F body size (mm)')), line=3, cex.lab=1.6)
##bison
points(ests_onF$body_mm_est[ests_onF$trt=="bison"] ~ ests_onF$mo_jit[ests_onF$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(ests_onF$body_mm_est[ests_onF$trt=="bison"] ~ ests_onF$mo_jit[ests_onF$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(ests_onF$mo_jit[ests_onF$trt=="bison"], ests_onF$body_mm_est[ests_onF$trt=="bison"]-ests_onF$body_mm_SE[ests_onF$trt=="bison"], ests_onF$mo_jit[ests_onF$trt=="bison"], ests_onF$body_mm_est[ests_onF$trt=="bison"]+ests_onF$body_mm_SE[ests_onF$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(ests_onF$body_mm_est[ests_onF$trt=="cattle"] ~ ests_onF$mo_jit[ests_onF$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(ests_onF$body_mm_est[ests_onF$trt=="cattle"] ~ ests_onF$mo_jit[ests_onF$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(ests_onF$mo_jit[ests_onF$trt=="cattle"], ests_onF$body_mm_est[ests_onF$trt=="cattle"]-ests_onF$body_mm_SE[ests_onF$trt=="cattle"], ests_onF$mo_jit[ests_onF$trt=="cattle"], ests_onF$body_mm_est[ests_onF$trt=="cattle"]+ests_onF$body_mm_SE[ests_onF$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(ests_onF$body_mm_est[ests_onF$trt=="ungrazed"] ~ ests_onF$mo_jit[ests_onF$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(ests_onF$body_mm_est[ests_onF$trt=="ungrazed"] ~ ests_onF$mo_jit[ests_onF$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(ests_onF$mo_jit[ests_onF$trt=="ungrazed"], ests_onF$body_mm_est[ests_onF$trt=="ungrazed"]-ests_onF$body_mm_SE[ests_onF$trt=="ungrazed"], ests_onF$mo_jit[ests_onF$trt=="ungrazed"], ests_onF$body_mm_est[ests_onF$trt=="ungrazed"]+ests_onF$body_mm_SE[ests_onF$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(ests_onF$body_mm_est[ests_onF$trt=="trtpd"] ~ ests_onF$mo_jit[ests_onF$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(ests_onF$body_mm_est[ests_onF$trt=="trtpd"] ~ ests_onF$mo_jit[ests_onF$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(ests_onF$mo_jit[ests_onF$trt=="trtpd"], ests_onF$body_mm_est[ests_onF$trt=="trtpd"]-ests_onF$body_mm_SE[ests_onF$trt=="trtpd"], ests_onF$mo_jit[ests_onF$trt=="trtpd"], ests_onF$body_mm_est[ests_onF$trt=="trtpd"]+ests_onF$body_mm_SE[ests_onF$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(ests_onF$body_mm_est[ests_onF$trt=="untrtpd"] ~ ests_onF$mo_jit[ests_onF$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(ests_onF$body_mm_est[ests_onF$trt=="untrtpd"] ~ ests_onF$mo_jit[ests_onF$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(ests_onF$mo_jit[ests_onF$trt=="untrtpd"], ests_onF$body_mm_est[ests_onF$trt=="untrtpd"]-ests_onF$body_mm_SE[ests_onF$trt=="untrtpd"], ests_onF$mo_jit[ests_onF$trt=="untrtpd"], ests_onF$body_mm_est[ests_onF$trt=="untrtpd"]+ests_onF$body_mm_SE[ests_onF$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

##Onthophagus nuchicornis male
plot(1, type="n", xlim=c(5.5,8.5), ylim=c(7.7,8.72),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8),cex.axis=1.1,labels=c("June","July","August"))
box(lwd=2)
title(ylab=substitute(paste(italic('Onthophagus nuchicornis'), ' M body size (mm)')), line=3, cex.lab=1.6)
##bison
points(ests_onM$body_mm_est[ests_onM$trt=="bison"] ~ ests_onM$mo_jit[ests_onM$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(ests_onM$body_mm_est[ests_onM$trt=="bison"] ~ ests_onM$mo_jit[ests_onM$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(ests_onM$mo_jit[ests_onM$trt=="bison"], ests_onM$body_mm_est[ests_onM$trt=="bison"]-ests_onM$body_mm_SE[ests_onM$trt=="bison"], ests_onM$mo_jit[ests_onM$trt=="bison"], ests_onM$body_mm_est[ests_onM$trt=="bison"]+ests_onM$body_mm_SE[ests_onM$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(ests_onM$body_mm_est[ests_onM$trt=="cattle"] ~ ests_onM$mo_jit[ests_onM$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(ests_onM$body_mm_est[ests_onM$trt=="cattle"] ~ ests_onM$mo_jit[ests_onM$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(ests_onM$mo_jit[ests_onM$trt=="cattle"], ests_onM$body_mm_est[ests_onM$trt=="cattle"]-ests_onM$body_mm_SE[ests_onM$trt=="cattle"], ests_onM$mo_jit[ests_onM$trt=="cattle"], ests_onM$body_mm_est[ests_onM$trt=="cattle"]+ests_onM$body_mm_SE[ests_onM$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(ests_onM$body_mm_est[ests_onM$trt=="ungrazed"] ~ ests_onM$mo_jit[ests_onM$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(ests_onM$body_mm_est[ests_onM$trt=="ungrazed"] ~ ests_onM$mo_jit[ests_onM$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(ests_onM$mo_jit[ests_onM$trt=="ungrazed"], ests_onM$body_mm_est[ests_onM$trt=="ungrazed"]-ests_onM$body_mm_SE[ests_onM$trt=="ungrazed"], ests_onM$mo_jit[ests_onM$trt=="ungrazed"], ests_onM$body_mm_est[ests_onM$trt=="ungrazed"]+ests_onM$body_mm_SE[ests_onM$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(ests_onM$body_mm_est[ests_onM$trt=="trtpd"] ~ ests_onM$mo_jit[ests_onM$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(ests_onM$body_mm_est[ests_onM$trt=="trtpd"] ~ ests_onM$mo_jit[ests_onM$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(ests_onM$mo_jit[ests_onM$trt=="trtpd"], ests_onM$body_mm_est[ests_onM$trt=="trtpd"]-ests_onM$body_mm_SE[ests_onM$trt=="trtpd"], ests_onM$mo_jit[ests_onM$trt=="trtpd"], ests_onM$body_mm_est[ests_onM$trt=="trtpd"]+ests_onM$body_mm_SE[ests_onM$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(ests_onM$body_mm_est[ests_onM$trt=="untrtpd"] ~ ests_onM$mo_jit[ests_onM$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(ests_onM$body_mm_est[ests_onM$trt=="untrtpd"] ~ ests_onM$mo_jit[ests_onM$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(ests_onM$mo_jit[ests_onM$trt=="untrtpd"], ests_onM$body_mm_est[ests_onM$trt=="untrtpd"]-ests_onM$body_mm_SE[ests_onM$trt=="untrtpd"], ests_onM$mo_jit[ests_onM$trt=="untrtpd"], ests_onM$body_mm_est[ests_onM$trt=="untrtpd"]+ests_onM$body_mm_SE[ests_onM$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)
legend("topleft",legend=c("Ungrazed","Bison","Cattle","Untrt PD","Trt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,25,24), pt.bg=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"),col=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"))

dev.off()


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