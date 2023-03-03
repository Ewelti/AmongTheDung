##Set working directory
#Ellen's working directory:
setwd("C:/Users/elwel/OneDrive/Desktop/AmongTheDung")
#Ben's working directory:

# load libraries
library("brms")

##read data
db <- read.csv("outputs/SiteMo_dungers.csv")
head(db)

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

## subset by group
cp <- db[which(db$spp=='Canthon_pilularius'),]
onF <- db[which(db$spp=='Onthophagus_nuchicornis_female'),]
onM <- db[which(db$spp=='Onthophagus_nuchicornis_male'),]
##

#define prior
prior1 = c(set_prior("normal(0,3)", class = "Intercept"))

##################cp

fit <- brm(Estimate|se(Est.Error) ~ yr_c, data = cp, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))

#check model
plot(fit)
sr_loo <- loo(fit, cores = getOption("mc.cores", 1))
sr_loo

#pull out fixed effects
sr_fixed_995 <- fixef(fit, probs = c(0.005, 0.995))
sr_fixed_975 <- fixef(fit, probs = c(0.025, 0.975))
sr_fixed_95 <- fixef(fit, probs = c(0.05, 0.95))
sr_fixed_90 <- fixef(fit, probs = c(0.1, 0.9))
sr_fixed <- list(Response="abundance", sr_fixed_995[2,1:4], sr_fixed_975[2,3:4],
                 sr_fixed_95[2,3:4],sr_fixed_90[2,3:4])
ab_fixed <-data.frame(lapply(sr_fixed, function(x) t(data.frame(x))))

####





hist(cp$sbody.mean)
head(cp)
plot(cp$body.mean ~ cp$grass_PC1)

cp.m <- lmer(sbody.mean ~ trt + sTemp48hr + sbrowser_100m2 + spatty_100m2 + (1|site),na.action=na.omit, data = cp)
cp.m <- lm(sbody.mean ~ trt + sTemp48hr + sbrowser_100m2 + spatty_100m2 + iMo ,na.action=na.omit, data = cp)
cp.m <- lm(sbody.mean ~ trt + sTemp48hr + sbrowser_100m2 + spatty_100m2 + iMo ,na.action=na.omit, data = cp)
summary(cp.m)

#run this if the above does not provide p-values
# extract coefficients
coefs <- data.frame(coef(summary(cp.m)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
