##Set working directory
#Ellen's working directory:
setwd("C:/Users/elwel/OneDrive/Desktop/AmongTheDung")
#Ben's working directory:

######################
library(scales)
horn <- read.csv("outputs/Trt_Mo/HornEsts_TrtMonth.csv")

tiff(filename = "plots/HornLength_TrtMo.tiff", width = 10, height = 5, units = 'in', res = 600, compression = 'lzw')

par(mar=c(2.5,5,0.4,0),mfrow=c(1,2))
plot(1, type="n", xlim=c(5.8,8.2), ylim=c(0.3,2.3),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8),cex.axis=1.1,labels=c("June","July","August"))
box(lwd=2)
title(ylab=substitute(paste(italic('O. nuchicornis'), ' M horn length (mm)')), line=3, cex.lab=1.6)
##connecting lines
points(horn$horn_mm_est[horn$trt=="bison"] ~ horn$mo_jit[horn$trt=="bison"],type="l",col="sienna",lwd=1)
points(horn$horn_mm_est[horn$trt=="cattle"] ~ horn$mo_jit[horn$trt=="cattle"],type="l",col="gray20",lwd=1)
points(horn$horn_mm_est[horn$trt=="ungrazed"] ~ horn$mo_jit[horn$trt=="ungrazed"],type="l",col="dodgerblue",lwd=1)
points(horn$horn_mm_est[horn$trt=="trtpd"] ~ horn$mo_jit[horn$trt=="trtpd"],type="l",col="firebrick2",lwd=1)
points(horn$horn_mm_est[horn$trt=="untrtpd"] ~ horn$mo_jit[horn$trt=="untrtpd"],type="l",col="goldenrod2",lwd=1)
##CIs
arrows(horn$mo_jit[horn$trt=="bison"], horn$Q5[horn$trt=="bison"], horn$mo_jit[horn$trt=="bison"], horn$Q95[horn$trt=="bison"],col=alpha("sienna",0.5),lwd=1,length=0, angle=90, code=3)
arrows(horn$mo_jit[horn$trt=="bison"], horn$Q10[horn$trt=="bison"], horn$mo_jit[horn$trt=="bison"], horn$Q90[horn$trt=="bison"],col=alpha("sienna",0.5),lwd=3,length=0, angle=90, code=3)
arrows(horn$mo_jit[horn$trt=="bison"], horn$Q20[horn$trt=="bison"], horn$mo_jit[horn$trt=="bison"], horn$Q80[horn$trt=="bison"],col=alpha("sienna",0.5),lwd=5,length=0, angle=90, code=3)
arrows(horn$mo_jit[horn$trt=="cattle"], horn$Q5[horn$trt=="cattle"], horn$mo_jit[horn$trt=="cattle"], horn$Q95[horn$trt=="cattle"],col=alpha("gray20",0.5),lwd=1,length=0, angle=90, code=3)
arrows(horn$mo_jit[horn$trt=="cattle"], horn$Q10[horn$trt=="cattle"], horn$mo_jit[horn$trt=="cattle"], horn$Q90[horn$trt=="cattle"],col=alpha("gray20",0.5),lwd=3,length=0, angle=90, code=3)
arrows(horn$mo_jit[horn$trt=="cattle"], horn$Q20[horn$trt=="cattle"], horn$mo_jit[horn$trt=="cattle"], horn$Q80[horn$trt=="cattle"],col=alpha("gray20",0.5),lwd=5,length=0, angle=90, code=3)
arrows(horn$mo_jit[horn$trt=="ungrazed"], horn$Q5[horn$trt=="ungrazed"], horn$mo_jit[horn$trt=="ungrazed"], horn$Q95[horn$trt=="ungrazed"],col=alpha("dodgerblue",0.5),lwd=1,length=0, angle=90, code=3)
arrows(horn$mo_jit[horn$trt=="ungrazed"], horn$Q10[horn$trt=="ungrazed"], horn$mo_jit[horn$trt=="ungrazed"], horn$Q90[horn$trt=="ungrazed"],col=alpha("dodgerblue",0.5),lwd=3,length=0, angle=90, code=3)
arrows(horn$mo_jit[horn$trt=="ungrazed"], horn$Q20[horn$trt=="ungrazed"], horn$mo_jit[horn$trt=="ungrazed"], horn$Q80[horn$trt=="ungrazed"],col=alpha("dodgerblue",0.5),lwd=5,length=0, angle=90, code=3)
arrows(horn$mo_jit[horn$trt=="trtpd"], horn$Q5[horn$trt=="trtpd"], horn$mo_jit[horn$trt=="trtpd"], horn$Q95[horn$trt=="trtpd"],col=alpha("firebrick2",0.5),lwd=1,length=0, angle=90, code=3)
arrows(horn$mo_jit[horn$trt=="trtpd"], horn$Q10[horn$trt=="trtpd"], horn$mo_jit[horn$trt=="trtpd"], horn$Q90[horn$trt=="trtpd"],col=alpha("firebrick2",0.5),lwd=3,length=0, angle=90, code=3)
arrows(horn$mo_jit[horn$trt=="trtpd"], horn$Q20[horn$trt=="trtpd"], horn$mo_jit[horn$trt=="trtpd"], horn$Q80[horn$trt=="trtpd"],col=alpha("firebrick2",0.5),lwd=5,length=0, angle=90, code=3)
arrows(horn$mo_jit[horn$trt=="untrtpd"], horn$Q5[horn$trt=="untrtpd"], horn$mo_jit[horn$trt=="untrtpd"], horn$Q95[horn$trt=="untrtpd"],col=alpha("goldenrod2",0.5),lwd=1,length=0, angle=90, code=3)
arrows(horn$mo_jit[horn$trt=="untrtpd"], horn$Q10[horn$trt=="untrtpd"], horn$mo_jit[horn$trt=="untrtpd"], horn$Q90[horn$trt=="untrtpd"],col=alpha("goldenrod2",0.5),lwd=3,length=0, angle=90, code=3)
arrows(horn$mo_jit[horn$trt=="untrtpd"], horn$Q20[horn$trt=="untrtpd"], horn$mo_jit[horn$trt=="untrtpd"], horn$Q80[horn$trt=="untrtpd"],col=alpha("goldenrod2",0.5),lwd=5,length=0, angle=90, code=3)
##points
points(horn$horn_mm_est[horn$trt=="bison"] ~ horn$mo_jit[horn$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2.5)
points(horn$horn_mm_est[horn$trt=="cattle"] ~ horn$mo_jit[horn$trt=="cattle"],pch=22,col="gray20",bg="gray20",cex=2.5)
points(horn$horn_mm_est[horn$trt=="ungrazed"] ~ horn$mo_jit[horn$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2.5)
points(horn$horn_mm_est[horn$trt=="trtpd"] ~ horn$mo_jit[horn$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2.5)
points(horn$horn_mm_est[horn$trt=="untrtpd"] ~ horn$mo_jit[horn$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2.5)
legend("topright",legend=c("Cattle","Bison","Trt PD","Untrt PD","Ungrazed"), bty="n", pt.cex=2,cex=1.3, pch=c(22,21,24,25,23), pt.bg=c("gray20","sienna","firebrick2","goldenrod2","dodgerblue"),col=c("gray20","sienna","firebrick2","goldenrod2","dodgerblue"))
##
legend("topleft", legend="A", bty="n", cex=2)
##

##############horn to body ratio:
head(onM)
max(na.omit(onM$body.length..mm.))
## Onthophagus nuchicornis male horn
par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.4,10.2), ylim=c(0.2,1.82),las=1,ylab="",xlab="")
box(lwd=2)
#title(ylab="Horn length (mm)", line=3, cex.lab=1.6)
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
legend("topleft", legend="B", bty="n", cex=2)

dev.off()

##