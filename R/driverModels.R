##Set working directory
#Ellen's working directory:
setwd("C:/Users/elwel/OneDrive/Desktop/AmongTheDung")
#Ben's working directory:

# load libraries
##install.packages("htmltools")
library("brms")

##read data
db <- read.csv("outputs/SiteMo_dungers.csv")
head(db)
db$site_broadly <- db$site

#month as an index starting from 1
db$iMo <- db$month - min(db$month)+1

all <- read.csv("rawdata/DungersAll.csv")
head(all)

all_t <- merge(all,db,by=c("site_broadly","month"),all.x=T, all.y=F)
dim(all_t)

#function to add a new column onto the data with scaled vars (with s before their name)
scaleVars <- function(df){
  newd <- plyr::numcolwise(scale)(df)
  names(newd) <- sapply(names(newd),function(x)paste0("s",x))
  cbind(df, newd)
}

#apply function
all_t <- scaleVars(all_t)

## subset by group
cp <- all_t[which(all_t$spp=='Canthon_pilularius'),]
onF <- all_t[which(all_t$spp=='Onthophagus_nuchicornis_female'),]
onM <- all_t[which(all_t$spp=='Onthophagus_nuchicornis_male'),]
##

#define prior
prior1 = c(set_prior("normal(0,3)", class = "Intercept"))
#prior1 = c(set_prior("horseshoe(1)", class = "b"))
##################cp
head(cp)

#fit <- brm(body.mean|se(body.se) ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + iMo + sTemp48hr + (1|site), data = cp, iter=5000, init = 0,
#            chains = 4, prior = prior1,
#            control = list(adapt_delta = 0.90,
#                           max_treedepth = 12))

fit <- brm(body.length..mm. ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + iMo + sTemp48hr + (1|site_broadly), data = cp, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))

#check model
plot(fit)
sr_loo <- loo(fit, cores = getOption("mc.cores", 1))
sr_loo

#pull out fixed effects
cp_fixed_95 <- fixef(fit, probs = c(0.05, 0.95))
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
