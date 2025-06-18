##Set working directory
#Ellen's working directory:
setwd("C:/Users/elwel/OneDrive/Desktop/AmongTheDung")

######################
library(scales)
bs <- read.csv("outputs/Trt_Mo/BodySizeEsts_TrtMonth.csv")

tiff(filename = "plots/ThreeSppBodySizes.tiff", width = 11, height = 7, units = 'in', res = 600, compression = 'lzw')

par(mar=c(2.5,5,0.4,0.2),mfrow=c(2,3))
plot(1, type="n", xlim=c(5.8,8.2), ylim=c(13.5,16.9),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8),cex.axis=1.1,labels=c("June","July","August"))
box(lwd=2)
title(ylab=substitute(paste(italic('C. pilularius'), ' body size (mm)')), line=3, cex.lab=1.6)
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
legend("topright", legend="A", bty="n", cex=2)

##
##Onthophagus nuchicornis female
plot(1, type="n", xlim=c(5.8,8.2), ylim=c(7.22,8.29),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8),cex.axis=1.1,labels=c("June","July","August"))
box(lwd=2)
title(ylab=substitute(paste(italic('O. nuchicornis'), ' F body size (mm)')), line=3, cex.lab=1.6)
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
legend("topright", legend="B", bty="n", cex=2)

##Onthophagus nuchicornis male
plot(1, type="n", xlim=c(5.8,8.2), ylim=c(7.48,9.2),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8),cex.axis=1.1,labels=c("June","July","August"))
box(lwd=2)
title(ylab=substitute(paste(italic('O. nuchicornis'), ' M body size (mm)')), line=3, cex.lab=1.6)
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
legend("topright", legend="C", bty="n", cex=2)

legend("topleft",legend=c("Cattle","Bison","Trt PD","Untrt PD","Ungrazed"), bty="n", pt.cex=2,cex=1.3, pch=c(22,21,24,25,23), pt.bg=c("gray20","sienna","firebrick2","goldenrod2","dodgerblue"),col=c("gray20","sienna","firebrick2","goldenrod2","dodgerblue"))
##

# attach raw dung beetle body size data
all <- read.csv("rawdata/DungersAll.csv")
head(all)
unique(all$species)
unique(all$trt)

all$trt <- factor(all$trt, levels=c("bison", "cattle", "trtpd", "untrtpd", "ungrazed"))

boxplot(all$body.length..mm.[all$species=="Canthon_pilularius"]~all$trt[all$species=="Canthon_pilularius"], xlab="", ylab="", col=c("sienna", "gray20", "firebrick2","goldenrod2","dodgerblue"), las=1)
box(lwd=2)
title(ylab=substitute(paste(italic('C. pilularius'), ' body size (mm)')), line=3, cex.lab=1.6)
legend("topright", legend="D", bty="n", cex=2)
tr<-(aov(all$body.length..mm.[all$species=="Canthon_pilularius"]~all$trt[all$species=="Canthon_pilularius"]))
tukey.test <- TukeyHSD(tr)
tukey.test

boxplot(all$body.length..mm.[all$species=="Onthophagus_nuchicornis_female"]~all$trt[all$species=="Onthophagus_nuchicornis_female"], xlab="", ylab="", col=c("sienna", "gray20", "firebrick2","goldenrod2","dodgerblue"), las=1)
box(lwd=2)
title(ylab=substitute(paste(italic('O. nuchicornis'), ' F body size (mm)')), line=3, cex.lab=1.6)
legend("topright", legend="E", bty="n", cex=2)
tr<-(aov(all$body.length..mm.[all$species=="Onthophagus_nuchicornis_female"]~bs$trt[all$species=="Onthophagus_nuchicornis_female"]))
tukey.test <- TukeyHSD(tr)
tukey.test

boxplot(all$body.length..mm.[all$species=="Onthophagus_nuchicornis_male"]~all$trt[all$species=="Onthophagus_nuchicornis_male"], xlab="", ylab="", col=c("sienna", "gray20", "firebrick2","goldenrod2","dodgerblue"), las=1)
box(lwd=2)
title(ylab=substitute(paste(italic('O. nuchicornis'), ' M body size (mm)')), line=3, cex.lab=1.6)
legend("topright", legend="F", bty="n", cex=2)
tr<-(aov(all$body.length..mm.[all$species=="Onthophagus_nuchicornis_male"]~all$trt[all$species=="Onthophagus_nuchicornis_male"]))
tukey.test <- TukeyHSD(tr)
tukey.test

dev.off()

##
##