##Set working directory
#Ellen's working directory:
setwd("C:/Users/elwel/OneDrive/Desktop/AmongTheDung")
#Ben's working directory:

# load libraries
##install.packages("htmltools")
library("brms")
library(dplyr)

##read data
dr <- read.csv("outputs/SiteMo_drivers.csv")
head(dr)
dim(dr)
ds <- read.csv("outputs/BodyEsts_SiteMonth.csv")
head(ds)
dim(ds)
db <- merge(dr,ds,by=c("site","month"),all.y=T)
head(db)
dim(db)

#month as an index starting from 1
db$iMo <- db$month - min(db$month)+1

#function to add a new column onto the data with scaled vars (with s before their name)
scaleVars <- function(df){
  newd <- plyr::numcolwise(scale)(df)
  names(newd) <- sapply(names(newd),function(x)paste0("s",x))
  cbind(df, newd)
}

#apply function
db <- scaleVars(db)

## subset by group in means sheet
cp_m <- db[which(db$spp=='Canthon_pilularius'),]
onF_m <- db[which(db$spp=='Onthophagus_nuchicornis_female'),]
onM_m <- db[which(db$spp=='Onthophagus_nuchicornis_male'),]
##

#define prior
prior1 = c(set_prior("normal(0,3)", class = "b"))

####################insecticide models
##cp
cp_t <- cp_m[cp_m$trt=='trtpd',]
cp_u <- cp_m[cp_m$trt=='untrtpd',]
cp_i <- rbind(cp_t,cp_u)
fit <- brm(body_mm_est|se(body_mm_SE) ~ sinsecticide + siMo, data = cp_i, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))
#pull out fixed effects
cp_95 <- fixef(fit, probs = c(0.05, 0.95))
cp_90 <- fixef(fit, probs = c(0.10, 0.90))
cp_80 <- fixef(fit, probs = c(0.20, 0.80))
cp_in <- cbind(spp= rep("CP",3), response= rep("BodySize",3),cp_95[,1:4], cp_90[,3:4],cp_80[,3:4])
cp_in

##onF
onF_t <- onF_m[onF_m$trt=='trtpd',]
onF_u <- onF_m[onF_m$trt=='untrtpd',]
onF_i <- rbind(onF_t,onF_u)
fit <- brm(body_mm_est|se(body_mm_SE) ~ sinsecticide + siMo, data = onF_i, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))
#pull out fixed effects
onF_95 <- fixef(fit, probs = c(0.05, 0.95))
onF_90 <- fixef(fit, probs = c(0.10, 0.90))
onF_80 <- fixef(fit, probs = c(0.20, 0.80))
onF_in <- cbind(spp= rep("onF",3), response= rep("BodySize",3),onF_95[,1:4], onF_90[,3:4],onF_80[,3:4])
onF_in

##onM
onM_t <- onM_m[onM_m$trt=='trtpd',]
onM_u <- onM_m[onM_m$trt=='untrtpd',]
onM_i <- rbind(onM_t,onM_u)
fit <- brm(body_mm_est|se(body_mm_SE) ~ sinsecticide + siMo, data = onM_i, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))
#pull out fixed effects
onM_95 <- fixef(fit, probs = c(0.05, 0.95))
onM_90 <- fixef(fit, probs = c(0.10, 0.90))
onM_80 <- fixef(fit, probs = c(0.20, 0.80))
onM_in <- cbind(spp= rep("onM",3), response= rep("BodySize",3),onM_95[,1:4], onM_90[,3:4],onM_80[,3:4])
onM_in

in_models <- rbind(cp_in,onF_in,onM_in)
write.csv(in_models, "outputs/DriverModelOutputs/BodySize_Insectide.csv")

#check models
plot(fit)
sr_loo <- loo(fit, cores = getOption("mc.cores", 1))
sr_loo
#####################################################################

####trt models

##cp
cp_n <- cp_m[!cp_m$trt=='trtpd',]
fit <- brm(body_mm_est|se(body_mm_SE) ~ sbison_dens + scattle_dens + sPD_pres + sCP_dens + (1|site), data = cp_n, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))
#pull out fixed effects
cp_95 <- fixef(fit, probs = c(0.05, 0.95))
cp_90 <- fixef(fit, probs = c(0.10, 0.90))
cp_80 <- fixef(fit, probs = c(0.20, 0.80))
cp_trt <- cbind(spp= rep("CP",5), response= rep("BodySize",5),cp_95[,1:4], cp_90[,3:4],cp_80[,3:4])
cp_trt

##onF
onF_n <- onF_m[!onF_m$trt=='trtpd',]
fit <- brm(body_mm_est|se(body_mm_SE) ~ sbison_dens + scattle_dens + sPD_pres + sON_dens + (1|site), data = onF_n, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))
#pull out fixed effects
onF_95 <- fixef(fit, probs = c(0.05, 0.95))
onF_90 <- fixef(fit, probs = c(0.10, 0.90))
onF_80 <- fixef(fit, probs = c(0.20, 0.80))
onF_trt <- cbind(spp= rep("onF",5), response= rep("BodySize",5),onF_95[,1:4], onF_90[,3:4],onF_80[,3:4])
onF_trt

##onM
onM_n <- onM_m[!onM_m$trt=='trtpd',]
fit <- brm(body_mm_est|se(body_mm_SE) ~ sbison_dens + scattle_dens + sPD_pres + sON_dens + (1|site), data = onM_n, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))
#pull out fixed effects
onM_95 <- fixef(fit, probs = c(0.05, 0.95))
onM_90 <- fixef(fit, probs = c(0.10, 0.90))
onM_80 <- fixef(fit, probs = c(0.20, 0.80))
onM_trt <- cbind(spp= rep("onM",5), response= rep("BodySize",5),onM_95[,1:4], onM_90[,3:4],onM_80[,3:4])
onM_trt

trt_models <- rbind(cp_trt,onF_trt,onM_trt)
write.csv(trt_models, "outputs/DriverModelOutputs/BodySize_Trt.csv")

#check models
plot(fit)
sr_loo <- loo(fit, cores = getOption("mc.cores", 1))
sr_loo
############################################################################

##current conditions models

##cp
fit <- brm(body_mm_est|se(body_mm_SE) ~ sTemp48hr + grass_PC1 + sbrowser_100m2 + spatty_100m2 + sPD_1m2 + sCP_dens + (1|site), data = cp_n, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))
#pull out fixed effects
cp_95 <- fixef(fit, probs = c(0.05, 0.95))
cp_90 <- fixef(fit, probs = c(0.10, 0.90))
cp_80 <- fixef(fit, probs = c(0.20, 0.80))
cp_co <- cbind(spp= rep("CP",7), response= rep("BodySize",7),cp_95[,1:4], cp_90[,3:4],cp_80[,3:4])
cp_co

##onF
fit <- brm(body_mm_est|se(body_mm_SE) ~ sTemp48hr + grass_PC1 + sbrowser_100m2 + spatty_100m2 + sPD_1m2 + sON_dens + (1|site), data = onF_n, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))
#pull out fixed effects
onF_95 <- fixef(fit, probs = c(0.05, 0.95))
onF_90 <- fixef(fit, probs = c(0.10, 0.90))
onF_80 <- fixef(fit, probs = c(0.20, 0.80))
onF_co <- cbind(spp= rep("onF",7), response= rep("BodySize",7),onF_95[,1:4], onF_90[,3:4],onF_80[,3:4])
onF_co

##onM
fit <- brm(body_mm_est|se(body_mm_SE) ~ sTemp48hr + grass_PC1 + sbrowser_100m2 + spatty_100m2 + sPD_1m2 + sON_dens + (1|site), data = onM_n, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))
#pull out fixed effects
onM_95 <- fixef(fit, probs = c(0.05, 0.95))
onM_90 <- fixef(fit, probs = c(0.10, 0.90))
onM_80 <- fixef(fit, probs = c(0.20, 0.80))
onM_co <- cbind(spp= rep("onM",7), response= rep("BodySize",7),onM_95[,1:4], onM_90[,3:4],onM_80[,3:4])
onM_co

co_models <- rbind(cp_co,onF_co,onM_co)
write.csv(co_models, "outputs/DriverModelOutputs/BodySize_CurrentConditions.csv")

#check models
plot(fit)
sr_loo <- loo(fit, cores = getOption("mc.cores", 1))
sr_loo

################################################################################

#########one month prior models
##cp
fit <- brm(body_mm_est|se(body_mm_SE) ~ sTemp20day + sgrass_PC_tm1 + sbrowser_lastMo + patty_lastMo + PD_lastMo + CP_dens_tm1 + (1|site), data = cp_n, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))
#pull out fixed effects
cp_95 <- fixef(fit, probs = c(0.05, 0.95))
cp_90 <- fixef(fit, probs = c(0.10, 0.90))
cp_80 <- fixef(fit, probs = c(0.20, 0.80))
cp_1mp <- cbind(spp= rep("CP",7), response= rep("BodySize",7),cp_95[,1:4], cp_90[,3:4],cp_80[,3:4])
cp_1mp

##onF
fit <- brm(body_mm_est|se(body_mm_SE) ~ sTemp20day + sgrass_PC_tm1 + sbrowser_lastMo + patty_lastMo + PD_lastMo + ON_dens_tm1 + (1|site), data = onF_n, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))
#pull out fixed effects
onF_95 <- fixef(fit, probs = c(0.05, 0.95))
onF_90 <- fixef(fit, probs = c(0.10, 0.90))
onF_80 <- fixef(fit, probs = c(0.20, 0.80))
onF_1mp <- cbind(spp= rep("onF",7), response= rep("BodySize",7),onF_95[,1:4], onF_90[,3:4],onF_80[,3:4])
onF_1mp

##onM
fit <- brm(body_mm_est|se(body_mm_SE) ~ sTemp20day + sgrass_PC_tm1 + sbrowser_lastMo + patty_lastMo + PD_lastMo + ON_dens_tm1 + (1|site), data = onM_n, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))
#pull out fixed effects
onM_95 <- fixef(fit, probs = c(0.05, 0.95))
onM_90 <- fixef(fit, probs = c(0.10, 0.90))
onM_80 <- fixef(fit, probs = c(0.20, 0.80))
onM_1mp <- cbind(spp= rep("onM",7), response= rep("BodySize",7),onM_95[,1:4], onM_90[,3:4],onM_80[,3:4])
onM_1mp

omp_models <- rbind(cp_1mp,onF_1mp,onM_1mp)
write.csv(omp_models, "outputs/DriverModelOutputs/BodySize_1mpConditions.csv")

#check models
plot(fit)
sr_loo <- loo(fit, cores = getOption("mc.cores", 1))
sr_loo

################################################################################

#########two month prior models
##cp
fit <- brm(body_mm_est|se(body_mm_SE) ~ sTemp60day + sgrass_PC_tm1 + sbrowser_2MoPrior + spatty_2MoPrior + sPD_2MoPrior + CP_dens_tm2, data = cp_n, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))
#pull out fixed effects
cp_95 <- fixef(fit, probs = c(0.05, 0.95))
cp_90 <- fixef(fit, probs = c(0.10, 0.90))
cp_80 <- fixef(fit, probs = c(0.20, 0.80))
cp_2mp <- cbind(spp= rep("CP",7), response= rep("BodySize",7),cp_95[,1:4], cp_90[,3:4],cp_80[,3:4])
cp_2mp

##onF
fit <- brm(body_mm_est|se(body_mm_SE) ~ sTemp60day + sgrass_PC_tm1 + sbrowser_2MoPrior + spatty_2MoPrior + sPD_2MoPrior + ON_dens_tm2, data = onF_n, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))
#pull out fixed effects
onF_95 <- fixef(fit, probs = c(0.05, 0.95))
onF_90 <- fixef(fit, probs = c(0.10, 0.90))
onF_80 <- fixef(fit, probs = c(0.20, 0.80))
onF_2mp <- cbind(spp= rep("onF",7), response= rep("BodySize",7),onF_95[,1:4], onF_90[,3:4],onF_80[,3:4])
onF_2mp

##onM
fit <- brm(body_mm_est|se(body_mm_SE) ~ sTemp60day + sgrass_PC_tm1 + sbrowser_2MoPrior + spatty_2MoPrior + sPD_2MoPrior + ON_dens_tm2, data = onM_n, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))
#pull out fixed effects
onM_95 <- fixef(fit, probs = c(0.05, 0.95))
onM_90 <- fixef(fit, probs = c(0.10, 0.90))
onM_80 <- fixef(fit, probs = c(0.20, 0.80))
onM_2mp <- cbind(spp= rep("onM",7), response= rep("BodySize",7),onM_95[,1:4], onM_90[,3:4],onM_80[,3:4])
onM_2mp

tmp_models <- rbind(cp_2mp,onF_2mp,onM_2mp)
write.csv(tmp_models, "outputs/DriverModelOutputs/BodySize_2mpConditions.csv")

#check models
plot(fit)
sr_loo <- loo(fit, cores = getOption("mc.cores", 1))
sr_loo

####
##################
#############################
####
