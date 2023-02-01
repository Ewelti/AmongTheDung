##Set working directory
#Ellen's working directory:
setwd("C:/Users/elwel/OneDrive/Desktop/AmongTheDung/rawdata")
#Ben's working directory:

# attach data
dungb <- read.csv("DungersAll.csv")
head(dungb)
unique(dungb$species)
cp <- dungb[which(dungb$species=='Canthon_pilularius'),]
onF <- dungb[which(dungb$species=='Onthophagus_nuchicornis_female'),]
onM <- dungb[which(dungb$species=='Onthophagus_nuchicornis_male'),]
##

boxplot(onM$horn.length..mm.~onM$trt)
boxplot(onM$horn.length..mm.~onM$site)
boxplot(onM$body.length..mm.~onM$trt)
boxplot(onF$body.length..mm.~onF$trt)
boxplot(cp$body.length..mm.~cp$trt)
boxplot(onM$wing.center..mm.~onM$trt)
boxplot(onF$wing.center..mm.~onF$trt)
boxplot(cp$wing.center..mm.~cp$trt)



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