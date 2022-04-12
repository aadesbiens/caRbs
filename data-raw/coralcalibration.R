#load required packages
library("latticeExtra")
library("dplyr")
library("tidyr")
library("forcats")
library("tidylog", warn.conflicts = FALSE)

#geometric equations relating coral morphometrics to 3D growth
massive.eq <- function(samples) {(2*pi*((sqrt(((samples))/pi))^2))*density*extension}
plating.eq <- function(samples){((((pi*(((sqrt((4*(samples))/pi)) + (extension*2))/2)^2)-samples)*thickness)*density) + (((pi*((((sqrt((4*(samples))/pi)))+(extension*2))/2)^2)*0.1)*extension*density)}
submassive.eq <- function(samples) {(samples*density*extension)}
digitate.eq <- function(samples) {(samples*d.conversionfactor*extension*density) + (samples-(d.conversionfactor*samples)*0.1*extension*density)}
branching.eq <- function(samples) {((((((radial*branchthick*density*extension)*0.1)*(axial*branchthick*density*extension))*100)/25000)*samples)}

#key growth rates for each taxa
genusgrowthforms <- read.csv("ameliadesbiens/Desktop/NESP caRbs/Data/genusgrowthforms.csv")

######################################################
###PRODUCTION SIMULATION

set.seed(101)

coralequations<-vector("list",length = nrow(genusgrowthforms))

for (i in 1:nrow(genusgrowthforms)) {
  colony.equation <- eval(parse(text = paste(genusgrowthforms[i,2])))   # geometric formula
  d.extension <-  genusgrowthforms[i,4]                                 # extension
  d.extension.sd <-  genusgrowthforms[i,5]                              # extension SD
  d.density <-  genusgrowthforms[i,6]                                   # density
  d.density.sd <-  genusgrowthforms[i,7]                                # density SD
  d.conversionfactor <- genusgrowthforms[i,8]                           # conversion factor
  maxcolonysize <- genusgrowthforms[i,9]                                # maximum colony size

  nloops <- 1000 # number of loops

  d.thickness <- 0.5 # colony thickness for plating.eq

  # Parameters for branching corals

  d.radial <- 7  # Number of radial branches per m2
  d.radial.sd <- 0.75 # SD
  d.axial <- 3 # Number of axial branches per m2
  d.axial.sd <- 0.34 #SD
  d.branchthick <- 1.9 # branch thickness for branching.eq
  d.branchthick.sd <- 0.2 # branch thickness SD for branching.eq

  coral.cover<-array(NA,dim=c(nloops,1))
  CP.sum<-array(NA,dim=c(nloops,1))
  CP.density<-array(NA,dim=c(nloops,1))
  CP.maxwidth<-array(NA,dim=c(nloops,1))

  # start loop
  for (j in 1:nloops){

    reefarea <- 400000
    cc <- as.numeric(sample(1:100,1)) # generate a string between 0 and 1 for n numbers of colonies
    colonyarea <- reefarea*(cc/100) # area of tables per given CC

    min <- 5 # % minimum colony size in width
    max <- maxcolonysize # % maximum colony size in width

    min <- pi*(min/2)^2
    max <- pi*(max/2)^2

    total <- colonyarea
    samples <- NULL
    successive_sum <- 0
    counter <- 1
    while (counter>0){
      thissample <- (max-min)*runif(1, 0, 1) + min
      if ((successive_sum+thissample)<(total-max)) {
        successive_sum <- successive_sum + thissample
        samples <- unname(cbind(samples,thissample)) }
      else {
        if ((total-(successive_sum+thissample)<min)){
          #    continue
        } else {
          samples <- cbind(samples,abs(thissample-(total-successive_sum)),total-(successive_sum+abs(thissample-(total-successive_sum))))
          counter <- 0
        }
      }
    }

    #define paramterisation of each colony at random
    n <- length(samples) # number of colonies
    diameter<- sqrt((4*(samples))/pi) # diameter of colonies
    radius <- sqrt(((samples))/pi)  # radius of colonies
    circumference <- 2*pi*radius # circumference of colonies
    extension <- abs(rnorm(n,d.extension,d.extension.sd))
    density <- abs(rnorm(n,d.density,d.density.sd))
    axial <- abs(rnorm(n,d.axial,d.axial.sd))
    radial <- abs(rnorm(n,d.radial,d.radial.sd))
    branchthick <- abs(rnorm(n,d.branchthick,d.branchthick.sd))
    thickness <- d.thickness


    CP.colony <- colony.equation(samples) # use equation to calculate carbonate production per colony
    CP.colony.sum <- sum(CP.colony)


    coral.cover[j,] <- cc # coral cover
    CP.sum[j,] <-sum(CP.colony.sum/reefarea) * 10 # carbonate production (convert g per cm2 to kg per m2 [x10])
    CP.density[j,] <-n # density
    CP.maxwidth[j,] <- max(diameter) # maximum colony diameter

  }

  df <- data.frame(CP = CP.sum[,1], coral.cover = coral.cover[,1])
  coralequations[[i]] <- lm(CP.sum ~ 0 + coral.cover, data = df)

}


### Extract Predictions--------------------

#extract predicted values across 0-100% cover for each species (nearest 0.1%)
datalist = list()

for (i in 1:length(coralequations)){
  df <- as.data.frame(c(seq(0.1, 100, by = 0.1)))
  colnames(df) <- "coral.cover"

  fit <- predict(coralequations[[i]], newdata = df, interval = "prediction", se.fit = TRUE)

  df$species <- genusgrowthforms[i,1]
  df <- cbind(df, fit$fit, fit$se.fit)

  datalist[[i]] <- df
}

df_main <- do.call(rbind, datalist)
colnames(df_main) <- c("X","taxa", "Y", "Y.lower", "Y.upper", "Y.se")
df_main <- as.data.frame(df_main)
df_main$Y <- as.numeric(as.character(df_main$Y))
df_main$X <- as.numeric(as.character(df_main$X))
df_main$Y.upper <- as.numeric(as.character(df_main$Y.upper))
df_main$Y.lower <- as.numeric(as.character(df_main$Y.lower))
df_main$Y.se <- as.numeric(as.character(df_main$Y.se))

write.csv(df_main, "pa_coefs_FINAL.csv") #save as pa_coefs in sysdata
