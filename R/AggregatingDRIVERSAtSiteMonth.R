##Set working directory
#Ellen's working directory:
setwd("C:/Users/elwel/OneDrive/Desktop/AmongTheDung")
#Ben's working directory:

##load libraries
library(stringr)
library(plotrix)
library(vegan)
library(BiodiversityR)
library(tidyverse)
library(modelr)

############# attach temp data
temp <- read.csv("outputs/Temp_daily_SiteMo.csv")

####### attach stocking densities
st <- read.csv("rawdata/stocking.csv")
est <- merge(temp,st,by=c("site"),all.x=T, all.y=T)
head(est)
dim(est)

############attach poo data
poo <- read.csv("rawdata/PooCount.csv")
head(poo)

#means by trt
poosum <- aggregate(cbind(browser_100m2,patty_100m2,PD_1m2,browser_minusLastMo,patty_minusLastMo,PD_minusLastMo,browser_lastMo,patty_lastMo,PD_lastMo,browser_2MoPrior,patty_2MoPrior,PD_2MoPrior) ~ code, data = poo, FUN = mean, na.action=NULL)
poosum[c('site', 'month')] <- str_split_fixed(poosum$code, '-', 2)
poosum$month <-as.numeric(poosum$month)
head(poosum)

estp <- merge(poosum,est,by=c("site","month"),all.y=T)
head(estp)
dim(estp)

######################################## Plant chem
chem <- read.csv("rawData/PlantChem.csv")
head(chem)
colnames(chem)
chem = subset(chem, select = -c(Number,label,yr,month,day,V_ppm,Al_ppm,B_ppm,Cu_ppm,Fe_ppm,Li_ppm,Mn_ppm,Ni_ppm,Pb_ppm,S_ppm,Sr_ppm,Ti_ppm,Zn_ppm))
sub_g <- chem[chem$type=="grass",]
sub_g$site_mo <- paste(sub_g$site,sub_g$mo)
sub_g = subset(sub_g, select = -c(trt,site,mo,type))
row.names(sub_g) <- sub_g[,9]
gc <- sub_g[,1:8]

##grass PCA
prin_comp<-prcomp(gc, scale=T)
summary(prin_comp)
prin_comp
biplot(prin_comp)
g_sc<- (scores(prin_comp)[,1])*-1 #axis scores for first axis ####NOTE: *-1 to make axis represent nutrient rich
# correlation between original variables and principal components
corr <- cor(gc, (prin_comp$x*-1))
round(corr, 3)
write.csv(as.matrix(corr),file="outputs/GrassChemPCA_correlations.csv")
############

#Broken-stick model#####
(ev <- prin_comp$sdev^2)
n <- length (ev)
bsm <- data.frame(j=seq(1:n), p=0)
bsm$p[1] <- 1/n
for (i in 2:n) {
  bsm$p[i] = bsm$p[i-1] + (1/(n+1-i))
}
bsm$p <- 100*bsm$p/n
bsm
barplot(t(cbind(100*ev/sum(ev),bsm$p[n:1])), beside=TRUE, main="Broken stick model", col=c("blue",2), las=2)
legend("topright", c("% eigenvalue", "Broken stick model"), pch=15, col=c("blue",2), bty="n")

##grass
gg <- as.data.frame(g_sc)
gsc <- cbind(rownames(gg), data.frame(gg, row.names=NULL))
colnames(gsc)[1] ="site_mo"
colnames(gsc)[2] ="grass_PC1"
gsc[c('site', 'month')] <- str_split_fixed(gsc$site_mo,' ', 2)
gsc = subset(gsc, select = -c(site_mo))
gsc$month <-as.numeric(gsc$month)

##add t-1 column
gsc <- gsc[order(gsc$site, gsc$mo),]

tm <- as.data.frame(c(NA,gsc[1:3,1],NA,gsc[5:7,1],NA,gsc[9:11,1], #bison
NA,gsc[13:15,1],NA,gsc[17:19,1],NA,gsc[21:22,1], #cattle
NA, #trtpd
NA,gsc[25:27,1],NA,gsc[29:31,1],NA,gsc[33:35,1], #ungrazed
NA,gsc[37:38,1],NA,gsc[40:41,1],NA,gsc[43,1])) #untrtpd
colnames(tm)[1] ="grass_PC_tm1"
gsc<- cbind(gsc,tm)

ests <- merge(gsc,estp,by=c("site","month"),all.y=T)
dim(ests)
ests = subset(ests, select = -c(code,code_spp))
head(ests)

#########densities
dens <- read.csv("rawData/DB_densities.csv")
dens <- dens[which(dens$DriverData=="y"),]
head(dens)
agg_cp <- aggregate(dens$CanPil, by=list(dens$site,dens$month), FUN=sum)
colnames(agg_cp) <- c('site','month','CP_dens')
ests <- merge(agg_cp,ests,by=c("site","month"),all.y=T)
agg_on <- aggregate(dens$OntNuc, by=list(dens$site,dens$month), FUN=sum)
colnames(agg_on) <- c('site','month','ON_dens')
ests <- merge(agg_on,ests,by=c("site","month"),all.y=T)

##
write.csv(ests, "outputs/SiteMo_drivers.csv")
##
