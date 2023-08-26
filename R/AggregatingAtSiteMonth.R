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

#################################################

#means by trt##################### OLD SECTION JUST USING MEANS
dbsum <- aggregate(cbind(wing,thorax,head,forearm,body) ~ code_spp, data = dungb, FUN = function(x)c(mean = mean(x),se = std.error(x)))
dbsum[c('trt','site', 'month', 'spp')] <- str_split_fixed(dbsum$code_spp, ' ', 4)
dbsum$month <-as.numeric(dbsum$month)
head(dbsum)
dim(dbsum)

#dcode <- data.frame((dbsum %>% drop_na(trt) %>% model_matrix(~ -1 + trt)))
#dbsum <- cbind(dbsum,dcode)
#head(dbsum)

############# attach temp data
temp <- read.csv("outputs/Temp_daily_SiteMo.csv")
est <- merge(dbsum,temp,by=c("site","month"),all.x=T, all.y=T)
head(est)
dim(est)

####### attach stocking densities
st <- read.csv("rawdata/stocking.csv")
est <- merge(est,st,by=c("site"),all.x=T, all.y=T)
head(est)
dim(est)

############attach poo data
poo <- read.csv("rawdata/PooCount.csv")
head(poo)

#means by trt
poosum <- aggregate(cbind(browser_100m2,patty_100m2,PD_1m2,browser_minusLastMo,patty_minusLastMo,PD_minusLastMo,browser_lastMo,patty_lastMo,PD_lastMo) ~ code, data = poo, FUN = mean, na.action=NULL)
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

write.csv(ests, "outputs/SiteMo_dungers.csv")

