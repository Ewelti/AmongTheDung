##Set working directory
#Ellen's working directory:
setwd("C:/Users/elwel/OneDrive/Desktop/AmongTheDung")

# load libraries
install.packages("corrplot")
library(corrplot)

##read data
dr <- read.csv("outputs/Site_Mo/SiteMo_drivers.csv")
head(dr)
dim(dr)

#subset data
dr_sub <- cbind(dr[,3], dr[,10:12], dr[,14:16], dr[,20:32])
head(dr_sub)

#rename columns
colnames(dr_sub) <- c("Month","Plant quality, contemporary", "Plant quality, 1 mo prior", "Plant quality, 2 mo prior",
"Browser dung, contemporary", "Grazer dung, contemporary", "Prairie dog dung, contemporary", "Browser dung, 1 mo prior", 
"Grazer dung, 1 mo prior", "Prairie dog dung, 1 mo prior", "Browser dung, 2 mo prior","Grazer dung, 2 mo prior",
"Prairie dog dung, 2 mo prior", "Temp, 48hr", "Temp, 20 days", "Temp, 60 days", "Bison density", "Cattle density", 
"Prairie dog presence", "Insecticide presence")

#test correlations
cor.test(dr_sub[,1], dr_sub[,2], na.action=na.omit)

####function for correlations and allowing NAs
mycor<- function(x,...){
  r<- apply(x, 2, function(j){
    apply(x, 2, function(i){
      as.numeric(cor.test(i,j)$estimate)
    })
  })
  P<- apply(x, 2, function(j){
    apply(x, 2, function(i){
      as.numeric(cor.test(i,j)$p.value)
    })
  })
  out<-c()
  out$P<- P
  out$r<- r
  return(out) 
}

##run function
CorDat<- mycor(dr_sub, method="pearson", na.action=na.omit)
head(CorDat)

#check corrplot
corrplot(corr=CorDat$r, method="number")

##save plot
tiff(filename = "plots/Drivers_Correlations.tiff", width = 7, height = 6, units = 'in', res = 600, compression = 'lzw')

corrplot(corr=CorDat$r, p.mat = CorDat$P,
         type="lower", insig="pch", sig.level =.05, pch.cex = .9)

dev.off()


