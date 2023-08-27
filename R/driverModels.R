##Set working directory
#Ellen's working directory:
setwd("C:/Users/elwel/OneDrive/Desktop/AmongTheDung")
#Ben's working directory:

# load libraries
##install.packages("htmltools")
library("brms")

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
prior1 = c(set_prior("normal(0,3)", class = "Intercept"))
#prior1 = c(set_prior("horseshoe(1)", class = "b"))
##################cp
head(cp_m)

fit_in <- brm(body_mm_est|se(body_mm_SE) ~ sinsecticide + sCP_dens + (1|site), data = cp_m, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))

cp_n <- cp_m[!cp_m$trt=='trtpd',]

fit_trt <- brm(body_mm_est|se(body_mm_SE) ~ sbison_dens + scattle_dens + sPD_pres + sCP_dens + (1|site), data = cp_n, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))

fit_co <- brm(body_mm_est|se(body_mm_SE) ~ sTemp48hr + grass_PC1 + sbrowser_100m2 + spatty_100m2 + sPD_1m2 + sCP_dens + (1|site), data = cp_m, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))

fit_1mp <- brm(body_mm_est|se(body_mm_SE) ~ sTemp20day + (1|site), data = cp_m, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))

fit_2mp <- brm(body_mm_est|se(body_mm_SE) ~ sTemp60day + (1|site), data = cp_m, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))

#check model
plot(fit_trt)
sr_loo <- loo(fit_trt, cores = getOption("mc.cores", 1))
sr_loo

#pull out fixed effects
cp_fixed_95 <- fixef(fit_trt, probs = c(0.05, 0.95))
cp_fixed_90 <- fixef(fit, probs = c(0.10, 0.90))
cp_fixed_80 <- fixef(fit, probs = c(0.20, 0.80))
####
##################onF

fit <- brm(body.mean|se(body.se) ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + iMo + sTemp48hr + (1|site), data = onF, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))

#check model
plot(fit)
sr_loo <- loo(fit, cores = getOption("mc.cores", 1))
sr_loo

#pull out fixed effects
onF_fixed_95 <- fixef(fit, probs = c(0.05, 0.95))
onF_fixed_90 <- fixef(fit, probs = c(0.10, 0.90))
onF_fixed_80 <- fixef(fit, probs = c(0.20, 0.80))
####
##################onM

fit <- brm(body.mean|se(body.se) ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + iMo + sTemp48hr + (1|site), data = onM, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))

#check model
plot(fit)
sr_loo <- loo(fit, cores = getOption("mc.cores", 1))
sr_loo

#pull out fixed effects
onM_fixed_95 <- fixef(fit, probs = c(0.05, 0.95))
onM_fixed_90 <- fixef(fit, probs = c(0.10, 0.90))
onM_fixed_80 <- fixef(fit, probs = c(0.20, 0.80))
####
