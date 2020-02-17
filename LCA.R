
###################################
#### Loading Packages and Data ####
###################################

library(poLCA)

pubData<-read.csv('~/MEGA/icensa_projects/ssrc/data_public.csv',sep=',',header=TRUE)
pubData <- unique(pubData[,c(4:7)]) ### Simplifying data b/c individuals show up multiple times and demographics are time stable
pubData$race <- as.factor(pubData$race) ### Converting race to factor
pubData$gender <- as.factor(pubData$gender) ### Converting gender to factor

###################################
#### Running LCA ####
###################################

f <- cbind(race,gender,yourelig)~1 ### specifying formula

lca.dat<- poLCA(f, pubData, nclass=2, maxiter=1000,verbose=TRUE, calc.se=TRUE,graphs=TRUE,na.rm=F) ### for 2 classes

lca.dat<- poLCA(f, pubData, nclass=3, maxiter=1000,verbose=TRUE, calc.se=TRUE,graphs=TRUE,na.rm=F) ### for 3 classes

lca.dat<- poLCA(f, pubData, nclass=4, maxiter=1000,verbose=TRUE, calc.se=TRUE,graphs=TRUE,na.rm=F) ### for 4 classes
##	- AIC/BIC gets larger after 2 classes
