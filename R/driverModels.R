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
#prior1 = c(set_prior("horseshoe(1)", class = "b"))
##################cp
head(cp)

fit <- brm(body.mean|se(body.se) ~ trt + iMo + sTemp48hr, data = cp, iter=5000, init = 0,
            chains = 4, prior = prior1,
            control = list(adapt_delta = 0.90,
                           max_treedepth = 12))

#check model
plot(fit)
sr_loo <- loo(fit, cores = getOption("mc.cores", 1))
sr_loo

#pull out fixed effects
cp_fixed_95 <- fixef(fit, probs = c(0.05, 0.95))

####
