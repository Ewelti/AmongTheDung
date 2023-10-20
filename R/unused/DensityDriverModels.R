##Set working directory
#Ellen's working directory:
setwd("C:/Users/elwel/OneDrive/Desktop/AmongTheDung")
#Ben's working directory:

##load libraries
library(brms)
library(stringr)

# attach data
dens <- read.csv("rawdata/DB_densities.csv")
head(dens)
dim(dens)
dr <- read.csv("outputs/SiteMo_drivers.csv")
head(dr)
dim(dr)
dens <- merge(dr,dens,by=c("site","month"),all.y=T)

#month as an index starting from 1
dens$iMo <- dens$month - min(dens$month)+1

#function to add a new column onto the data with scaled vars (with s before their name)
scaleVars <- function(df){
  newd <- plyr::numcolwise(scale)(df)
  names(newd) <- sapply(names(newd),function(x)paste0("s",x))
  cbind(df, newd)
}

#apply function
dens <- scaleVars(dens)

#define prior
prior1 = c(set_prior("normal(0,3)", class = "b"))

boxplot(dens$CanPil~dens$trt)
##cp
cp_t <- dens[dens$trt=='trtpd',]
cp_u <- dens[dens$trt=='untrtpd',]
cp_i <- rbind(cp_t,cp_u)
t.test(cp_i$CanPil ~ cp_i$trt)
fit <- brm(CanPil ~ sinsecticide + siMo, data = cp_i, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))
#pull out fixed effects
cp_95 <- fixef(fit, probs = c(0.05, 0.95))
cp_90 <- fixef(fit, probs = c(0.10, 0.90))
cp_80 <- fixef(fit, probs = c(0.20, 0.80))
cp_in <- cbind(spp= rep("CP",3), response= rep("ActivityDensity",3),cp_95[,1:4], cp_90[,3:4],cp_80[,3:4])
cp_in