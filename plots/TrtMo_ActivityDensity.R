##Set working directory
#Ellen's working directory:
setwd("C:/Users/elwel/OneDrive/Desktop/AmongTheDung")
#Ben's working directory:

######################
library(scales)
cp_d <- read.csv("outputs/Trt_Mo/CanPil_TRTmo_ActDens.csv")
cp_d$spp <- rep("CP", 15)
colnames(cp_d)[3] ="AcDe"
colnames(cp_d)[4] ="SE"
on_d <- read.csv("outputs/Trt_Mo/OntNuc_TRTmo_ActDens.csv")
on_d$spp <- rep("ON", 15)
colnames(on_d)[3] ="AcDe"
colnames(on_d)[4] ="SE"
dens <- rbind(cp_d,on_d)

tiff(filename = "plots/ActivityDensities_TrtMo.tiff", width = 10, height = 5, units = 'in', res = 600, compression = 'lzw')

par(mar=c(2.5,5,0.4,0.2),mfrow=c(1,2))
plot(1, type="n", xlim=c(5.8,8.2), ylim=c(0,165),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8),cex.axis=1.1,labels=c("June","July","August"))
box(lwd=2)
title(ylab=substitute(paste(italic('C. pilularius'), ' beetles/trap')), line=3, cex.lab=1.6)
##connecting lines
points(dens$AcDe[dens$trt=="bison" & dens$spp=="CP"] ~ dens$mo_jit[dens$trt=="bison" & dens$spp=="CP"],type="l",col="sienna",lwd=1)
points(dens$AcDe[dens$trt=="cattle" & dens$spp=="CP"] ~ dens$mo_jit[dens$trt=="cattle" & dens$spp=="CP"],type="l",col="gray20",lwd=1)
points(dens$AcDe[dens$trt=="ungrazed" & dens$spp=="CP"] ~ dens$mo_jit[dens$trt=="ungrazed" & dens$spp=="CP"],type="l",col="dodgerblue",lwd=1)
points(dens$AcDe[dens$trt=="trtpd" & dens$spp=="CP"] ~ dens$mo_jit[dens$trt=="trtpd" & dens$spp=="CP"],type="l",col="firebrick2",lwd=1)
points(dens$AcDe[dens$trt=="untrtpd" & dens$spp=="CP"] ~ dens$mo_jit[dens$trt=="untrtpd" & dens$spp=="CP"],type="l",col="goldenrod2",lwd=1)
##CIs
arrows(dens$mo_jit[dens$trt=="bison" & dens$spp=="CP"], dens$Q5[dens$trt=="bison" & dens$spp=="CP"], dens$mo_jit[dens$trt=="bison" & dens$spp=="CP"], dens$Q95[dens$trt=="bison" & dens$spp=="CP"],col=alpha("sienna",0.5),lwd=1,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="bison" & dens$spp=="CP"], dens$Q10[dens$trt=="bison" & dens$spp=="CP"], dens$mo_jit[dens$trt=="bison" & dens$spp=="CP"], dens$Q90[dens$trt=="bison" & dens$spp=="CP"],col=alpha("sienna",0.5),lwd=3,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="bison" & dens$spp=="CP"], dens$Q20[dens$trt=="bison" & dens$spp=="CP"], dens$mo_jit[dens$trt=="bison" & dens$spp=="CP"], dens$Q80[dens$trt=="bison" & dens$spp=="CP"],col=alpha("sienna",0.5),lwd=5,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="cattle" & dens$spp=="CP"], dens$Q5[dens$trt=="cattle" & dens$spp=="CP"], dens$mo_jit[dens$trt=="cattle" & dens$spp=="CP"], dens$Q95[dens$trt=="cattle" & dens$spp=="CP"],col=alpha("gray20",0.5),lwd=1,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="cattle" & dens$spp=="CP"], dens$Q10[dens$trt=="cattle" & dens$spp=="CP"], dens$mo_jit[dens$trt=="cattle" & dens$spp=="CP"], dens$Q90[dens$trt=="cattle" & dens$spp=="CP"],col=alpha("gray20",0.5),lwd=3,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="cattle" & dens$spp=="CP"], dens$Q20[dens$trt=="cattle" & dens$spp=="CP"], dens$mo_jit[dens$trt=="cattle" & dens$spp=="CP"], dens$Q80[dens$trt=="cattle" & dens$spp=="CP"],col=alpha("gray20",0.5),lwd=5,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="ungrazed" & dens$spp=="CP"], dens$Q5[dens$trt=="ungrazed" & dens$spp=="CP"], dens$mo_jit[dens$trt=="ungrazed" & dens$spp=="CP"], dens$Q95[dens$trt=="ungrazed" & dens$spp=="CP"],col=alpha("dodgerblue",0.5),lwd=1,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="ungrazed" & dens$spp=="CP"], dens$Q10[dens$trt=="ungrazed" & dens$spp=="CP"], dens$mo_jit[dens$trt=="ungrazed" & dens$spp=="CP"], dens$Q90[dens$trt=="ungrazed" & dens$spp=="CP"],col=alpha("dodgerblue",0.5),lwd=3,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="ungrazed" & dens$spp=="CP"], dens$Q20[dens$trt=="ungrazed" & dens$spp=="CP"], dens$mo_jit[dens$trt=="ungrazed" & dens$spp=="CP"], dens$Q80[dens$trt=="ungrazed" & dens$spp=="CP"],col=alpha("dodgerblue",0.5),lwd=5,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="trtpd" & dens$spp=="CP"], dens$Q5[dens$trt=="trtpd" & dens$spp=="CP"], dens$mo_jit[dens$trt=="trtpd" & dens$spp=="CP"], dens$Q95[dens$trt=="trtpd" & dens$spp=="CP"],col=alpha("firebrick2",0.5),lwd=1,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="trtpd" & dens$spp=="CP"], dens$Q10[dens$trt=="trtpd" & dens$spp=="CP"], dens$mo_jit[dens$trt=="trtpd" & dens$spp=="CP"], dens$Q90[dens$trt=="trtpd" & dens$spp=="CP"],col=alpha("firebrick2",0.5),lwd=3,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="trtpd" & dens$spp=="CP"], dens$Q20[dens$trt=="trtpd" & dens$spp=="CP"], dens$mo_jit[dens$trt=="trtpd" & dens$spp=="CP"], dens$Q80[dens$trt=="trtpd" & dens$spp=="CP"],col=alpha("firebrick2",0.5),lwd=5,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="untrtpd" & dens$spp=="CP"], dens$Q5[dens$trt=="untrtpd" & dens$spp=="CP"], dens$mo_jit[dens$trt=="untrtpd" & dens$spp=="CP"], dens$Q95[dens$trt=="untrtpd" & dens$spp=="CP"],col=alpha("goldenrod2",0.5),lwd=1,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="untrtpd" & dens$spp=="CP"], dens$Q10[dens$trt=="untrtpd" & dens$spp=="CP"], dens$mo_jit[dens$trt=="untrtpd" & dens$spp=="CP"], dens$Q90[dens$trt=="untrtpd" & dens$spp=="CP"],col=alpha("goldenrod2",0.5),lwd=3,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="untrtpd" & dens$spp=="CP"], dens$Q20[dens$trt=="untrtpd" & dens$spp=="CP"], dens$mo_jit[dens$trt=="untrtpd" & dens$spp=="CP"], dens$Q80[dens$trt=="untrtpd" & dens$spp=="CP"],col=alpha("goldenrod2",0.5),lwd=5,length=0, angle=90, code=3)
##points
points(dens$AcDe[dens$trt=="bison" & dens$spp=="CP"] ~ dens$mo_jit[dens$trt=="bison" & dens$spp=="CP"],pch=21,col="sienna",bg="sienna",cex=2.5)
points(dens$AcDe[dens$trt=="cattle" & dens$spp=="CP"] ~ dens$mo_jit[dens$trt=="cattle" & dens$spp=="CP"],pch=22,col="gray20",bg="gray20",cex=2.5)
points(dens$AcDe[dens$trt=="ungrazed" & dens$spp=="CP"] ~ dens$mo_jit[dens$trt=="ungrazed" & dens$spp=="CP"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2.5)
points(dens$AcDe[dens$trt=="trtpd" & dens$spp=="CP"] ~ dens$mo_jit[dens$trt=="trtpd" & dens$spp=="CP"],pch=24,col="firebrick2",bg="firebrick2",cex=2.5)
points(dens$AcDe[dens$trt=="untrtpd" & dens$spp=="CP"] ~ dens$mo_jit[dens$trt=="untrtpd" & dens$spp=="CP"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2.5)
legend("topright", legend="A", bty="n", cex=2)
legend("topleft",legend=c("Cattle","Bison","Trt PD","Untrt PD","Ungrazed"), bty="n", pt.cex=2,cex=1.3, pch=c(22,21,24,25,23), pt.bg=c("gray20","sienna","firebrick2","goldenrod2","dodgerblue"),col=c("gray20","sienna","firebrick2","goldenrod2","dodgerblue"))
##

##
##Onthophagus nuchicornis
plot(1, type="n", xlim=c(5.8,8.2), ylim=c(0,82),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8),cex.axis=1.1,labels=c("June","July","August"))
box(lwd=2)
title(ylab=substitute(paste(italic('O. nuchicornis'), ' beetles/trap')), line=3, cex.lab=1.6)
##conecting lines
points(dens$AcDe[dens$trt=="bison" & dens$spp=="ON"] ~ dens$mo_jit[dens$trt=="bison" & dens$spp=="ON"],type="l",col="sienna",lwd=1)
points(dens$AcDe[dens$trt=="cattle" & dens$spp=="ON"] ~ dens$mo_jit[dens$trt=="cattle" & dens$spp=="ON"],type="l",col="gray20",lwd=1)
points(dens$AcDe[dens$trt=="ungrazed" & dens$spp=="ON"] ~ dens$mo_jit[dens$trt=="ungrazed" & dens$spp=="ON"],type="l",col="dodgerblue",lwd=1)
points(dens$AcDe[dens$trt=="trtpd" & dens$spp=="ON"] ~ dens$mo_jit[dens$trt=="trtpd" & dens$spp=="ON"],type="l",col="firebrick2",lwd=1)
points(dens$AcDe[dens$trt=="untrtpd" & dens$spp=="ON"] ~ dens$mo_jit[dens$trt=="untrtpd" & dens$spp=="ON"],type="l",col="goldenrod2",lwd=1)
##CIs
arrows(dens$mo_jit[dens$trt=="bison" & dens$spp=="ON"], dens$Q5[dens$trt=="bison" & dens$spp=="ON"], dens$mo_jit[dens$trt=="bison" & dens$spp=="ON"], dens$Q95[dens$trt=="bison" & dens$spp=="ON"],col=alpha("sienna",0.5),lwd=1,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="bison" & dens$spp=="ON"], dens$Q10[dens$trt=="bison" & dens$spp=="ON"], dens$mo_jit[dens$trt=="bison" & dens$spp=="ON"], dens$Q90[dens$trt=="bison" & dens$spp=="ON"],col=alpha("sienna",0.5),lwd=3,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="bison" & dens$spp=="ON"], dens$Q20[dens$trt=="bison" & dens$spp=="ON"], dens$mo_jit[dens$trt=="bison" & dens$spp=="ON"], dens$Q80[dens$trt=="bison" & dens$spp=="ON"],col=alpha("sienna",0.5),lwd=5,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="cattle" & dens$spp=="ON"], dens$Q5[dens$trt=="cattle" & dens$spp=="ON"], dens$mo_jit[dens$trt=="cattle" & dens$spp=="ON"], dens$Q95[dens$trt=="cattle" & dens$spp=="ON"],col=alpha("gray20",0.5),lwd=1,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="cattle" & dens$spp=="ON"], dens$Q10[dens$trt=="cattle" & dens$spp=="ON"], dens$mo_jit[dens$trt=="cattle" & dens$spp=="ON"], dens$Q90[dens$trt=="cattle" & dens$spp=="ON"],col=alpha("gray20",0.5),lwd=3,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="cattle" & dens$spp=="ON"], dens$Q20[dens$trt=="cattle" & dens$spp=="ON"], dens$mo_jit[dens$trt=="cattle" & dens$spp=="ON"], dens$Q80[dens$trt=="cattle" & dens$spp=="ON"],col=alpha("gray20",0.5),lwd=5,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="ungrazed" & dens$spp=="ON"], dens$Q5[dens$trt=="ungrazed" & dens$spp=="ON"], dens$mo_jit[dens$trt=="ungrazed" & dens$spp=="ON"], dens$Q95[dens$trt=="ungrazed" & dens$spp=="ON"],col=alpha("dodgerblue",0.5),lwd=1,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="ungrazed" & dens$spp=="ON"], dens$Q10[dens$trt=="ungrazed" & dens$spp=="ON"], dens$mo_jit[dens$trt=="ungrazed" & dens$spp=="ON"], dens$Q90[dens$trt=="ungrazed" & dens$spp=="ON"],col=alpha("dodgerblue",0.5),lwd=3,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="ungrazed" & dens$spp=="ON"], dens$Q20[dens$trt=="ungrazed" & dens$spp=="ON"], dens$mo_jit[dens$trt=="ungrazed" & dens$spp=="ON"], dens$Q80[dens$trt=="ungrazed" & dens$spp=="ON"],col=alpha("dodgerblue",0.5),lwd=5,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="trtpd" & dens$spp=="ON"], dens$Q5[dens$trt=="trtpd" & dens$spp=="ON"], dens$mo_jit[dens$trt=="trtpd" & dens$spp=="ON"], dens$Q95[dens$trt=="trtpd" & dens$spp=="ON"],col=alpha("firebrick2",0.5),lwd=1,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="trtpd" & dens$spp=="ON"], dens$Q10[dens$trt=="trtpd" & dens$spp=="ON"], dens$mo_jit[dens$trt=="trtpd" & dens$spp=="ON"], dens$Q90[dens$trt=="trtpd" & dens$spp=="ON"],col=alpha("firebrick2",0.5),lwd=3,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="trtpd" & dens$spp=="ON"], dens$Q20[dens$trt=="trtpd" & dens$spp=="ON"], dens$mo_jit[dens$trt=="trtpd" & dens$spp=="ON"], dens$Q80[dens$trt=="trtpd" & dens$spp=="ON"],col=alpha("firebrick2",0.5),lwd=5,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="untrtpd" & dens$spp=="ON"], dens$Q5[dens$trt=="untrtpd" & dens$spp=="ON"], dens$mo_jit[dens$trt=="untrtpd" & dens$spp=="ON"], dens$Q95[dens$trt=="untrtpd" & dens$spp=="ON"],col=alpha("goldenrod2",0.5),lwd=1,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="untrtpd" & dens$spp=="ON"], dens$Q10[dens$trt=="untrtpd" & dens$spp=="ON"], dens$mo_jit[dens$trt=="untrtpd" & dens$spp=="ON"], dens$Q90[dens$trt=="untrtpd" & dens$spp=="ON"],col=alpha("goldenrod2",0.5),lwd=3,length=0, angle=90, code=3)
arrows(dens$mo_jit[dens$trt=="untrtpd" & dens$spp=="ON"], dens$Q20[dens$trt=="untrtpd" & dens$spp=="ON"], dens$mo_jit[dens$trt=="untrtpd" & dens$spp=="ON"], dens$Q80[dens$trt=="untrtpd" & dens$spp=="ON"],col=alpha("goldenrod2",0.5),lwd=5,length=0, angle=90, code=3)
##points
points(dens$AcDe[dens$trt=="bison" & dens$spp=="ON"] ~ dens$mo_jit[dens$trt=="bison" & dens$spp=="ON"],pch=21,col="sienna",bg="sienna",cex=2.5)
points(dens$AcDe[dens$trt=="cattle" & dens$spp=="ON"] ~ dens$mo_jit[dens$trt=="cattle" & dens$spp=="ON"],pch=22,col="gray20",bg="gray20",cex=2.5)
points(dens$AcDe[dens$trt=="ungrazed" & dens$spp=="ON"] ~ dens$mo_jit[dens$trt=="ungrazed" & dens$spp=="ON"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2.5)
points(dens$AcDe[dens$trt=="trtpd" & dens$spp=="ON"] ~ dens$mo_jit[dens$trt=="trtpd" & dens$spp=="ON"],pch=24,col="firebrick2",bg="firebrick2",cex=2.5)
points(dens$AcDe[dens$trt=="untrtpd" & dens$spp=="ON"] ~ dens$mo_jit[dens$trt=="untrtpd" & dens$spp=="ON"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2.5)
legend("topright", legend="B", bty="n", cex=2)
##

dev.off()

##