##Set working directory
#Ellen's working directory:
setwd("C:/Users/elwel/OneDrive/Desktop/AmongTheDung/rawdata")
#Ben's working directory:

##load libraries
library(lme4)
library(stringr)

# attach data
dungb <- read.csv("DungersAll.csv")
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
ests <- NULL
for(i in unique(cp$trt_mo)){
  sub <- cp[cp$trt_mo == i, ]
  ests.i <- coef(summary(lmer(body.length..mm. ~ 1 + (1|trap:site), data = sub, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests

ests[c('trt', 'month')] <- str_split_fixed(ests$trt_mo, ' ', 2)
colnames(ests)[2] ="body_mm_est"
colnames(ests)[3] ="body_mm_SE"
ests$month <-as.numeric(ests$month)
ests <- ests[order(ests$month,ests$trt),]
ests$mo_jit <- (ests$month + c(-0.1,-0.2,0,0.2,0.1))

########################################
#calculate trt effects on pasture meter
mo <- lmer(g_per_m2  ~  trt + (1|line:month:site),data=pm)
summary(mo)

# extract coefficients
coefs <- data.frame(coef(summary(mo)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
######################

par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.5,8.5), ylim=c(14,17),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab=expression("Canthon praticola body size (mm)"), line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(ests$body_mm_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(ests$body_mm_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(ests$mo_jit[ests$trt=="bison"], ests$body_mm_est[ests$trt=="bison"]-ests$body_mm_SE[ests$trt=="bison"], ests$mo_jit[ests$trt=="bison"], ests$body_mm_est[ests$trt=="bison"]+ests$body_mm_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(ests$body_mm_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(ests$body_mm_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(ests$mo_jit[ests$trt=="cattle"], ests$body_mm_est[ests$trt=="cattle"]-ests$body_mm_SE[ests$trt=="cattle"], ests$mo_jit[ests$trt=="cattle"], ests$body_mm_est[ests$trt=="cattle"]+ests$body_mm_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(ests$body_mm_est[ests$trt=="ungrazed"] ~ ests$mo_jit[ests$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(ests$body_mm_est[ests$trt=="ungrazed"] ~ ests$mo_jit[ests$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(ests$mo_jit[ests$trt=="ungrazed"], ests$body_mm_est[ests$trt=="ungrazed"]-ests$body_mm_SE[ests$trt=="ungrazed"], ests$mo_jit[ests$trt=="ungrazed"], ests$body_mm_est[ests$trt=="ungrazed"]+ests$body_mm_SE[ests$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(ests$body_mm_est[ests$trt=="trtpd"] ~ ests$mo_jit[ests$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(ests$body_mm_est[ests$trt=="trtpd"] ~ ests$mo_jit[ests$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(ests$mo_jit[ests$trt=="trtpd"], ests$body_mm_est[ests$trt=="trtpd"]-ests$body_mm_SE[ests$trt=="trtpd"], ests$mo_jit[ests$trt=="trtpd"], ests$body_mm_est[ests$trt=="trtpd"]+ests$body_mm_SE[ests$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(ests$body_mm_est[ests$trt=="untrtpd"] ~ ests$mo_jit[ests$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(ests$body_mm_est[ests$trt=="untrtpd"] ~ ests$mo_jit[ests$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(ests$mo_jit[ests$trt=="untrtpd"], ests$body_mm_est[ests$trt=="untrtpd"]-ests$body_mm_SE[ests$trt=="untrtpd"], ests$mo_jit[ests$trt=="untrtpd"], ests$body_mm_est[ests$trt=="untrtpd"]+ests$body_mm_SE[ests$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)







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