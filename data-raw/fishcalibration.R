#load required packages
library("latticeExtra")
library("dplyr")
library("tidyr")
library("forcats")
library("tidylog", warn.conflicts = FALSE)

################################################
##BITE RATES

#import data from local drive
biterates <- read.csv("ameliadesbiens/Desktop/caRbs/Data/fish erosion/bite_rates.csv") %>%
  rename(TL = TL_cm) %>%
  filter(Species != "S. quoyi") #remove - not enough data

#rename species to match AIMS naming conventions
biterates <- read.csv("Data/fish erosion/bite_rates.csv") %>%
  rename(TL = TL_cm) %>%
  filter(Species != "S. quoyi") #remove - not enough data

#rename species to match AIMS naming conventions
biterates$taxa <-  recode(biterates$Species, "B. muricatum" = "BOL.MURI", "C. bicolor" = "CET.BIC", "C. bleekeri" = "CHS.BLEE",
                          "C. carolinus" = "CAL.CARO", "C. microrhinos" = "CHS.MICR", "C. sordidus" = "CHS.SORD",
                          "H. longiceps" = "HIP.LONG", "S. altipinnis" = "SCA.ALTI", "S. chameleon" = "SCA.CHAM",
                          "S. dimidiatus" = "SCA.DIMI", "S. flavipectoralis" = "SCA.FLAV", "S. forsteni" = "SCA.FORS",
                          "S. frenatus" = "SCA.FREN", "S. ghobban" = "SCA.GHOB", "S. globiceps" = "SCA.GLOB",
                          "S. niger" = "SCA.NIGR", "S. oviceps" = "SCA.OVIC", "S. psittacus" = "SCA.PSIT",
                          "S. rivulatus" = "SCA.RIVU", "S. rubroviolaceus" = "SCA.RUBR",
                          "S. schlegeli" = "SCA.SCHL", "S. spinus" = "SCA.SPIN")

# Fit exponential function
spbr <- unique(biterates$taxa) #extract species names
modbrexp <- vector("list",length(spbr)) #create empty list to fill in for loop

for (i in 1:length(spbr)) {

  #fit model to each species
  modbrexp[[i]] <- lm(log(bite_rate)~TL, data = biterates %>% filter(taxa == spbr[i]))

}

names(modbrexp) <- spbr

#########################################################
##BITE DIMENSIONS

#import data from local drive
bitedimensions <- read.csv("ameliadesbiens/Desktop/caRbs/Data/fish erosion/bite_dimensions.csv") %>%
  filter(Bite_length_mm > 0) %>%
  mutate(ellipse = pi* (Bite_length_mm/2) * (Bite_width_mm/2))  %>% #convert length x width to ellipse for area calculation
  rename(TL = TL_cm) %>%
  filter(!Species %in% c("bleekeri", "dimidiadus", "flavipectoralis", "ghobban", "globiceps", "rivulatus")) #not enough data for these species

#rename species codes as above
bitedimensions$taxa <-  recode(bitedimensions$Species, "bicolor" = "CET.BIC","microrhinos" = "CHS.MICR", "sordidus" = "CHS.SORD",
                               "altipinnus" = "SCA.ALTI", "forsteni" = "SCA.FORS", "frenatus" = "SCA.FREN",
                               "niger" = "SCA.NIGR", "oviceps" = "SCA.OVIC", "rubroviolaceus" = "SCA.RUBR",
                               "schlegeli" = "SCA.SCHL", "spinus" = "SCA.SPIN")

#Fit linear function
spba <- unique(bitedimensions$taxa)
modbalm <- vector("list", length(spba)) #initialise vectors as above

for (i in 1:length(spba)) {

  #fit model for each species
  modbalm[[i]] <- lm(ellipse~0+TL, data = bitedimensions %>% filter(taxa == spba[i])) #force through 0

}

names(modbalm) <- spba

#################################################
##BITE PROPORTIONS

#import data from local drive
biteprop <- read.csv("ameliadesbiens/Desktop/caRbs/Data/fish erosion/bite_dimensions.csv") %>%
  mutate(scar = ifelse(Bite_length_mm > 0, 1, 0)) %>%
  rename(TL = TL_cm) %>%
  filter(!Species %in% c("bleekeri", "carolinus","chameleon", "dimidiadus",
                         "flavipectoralis", "ghobban", "globiceps", "psittacus", "rivulatus")) #not enough data for these species

#rename species codes
biteprop$taxa <-  recode(biteprop$Species, "bicolor" = "CET.BIC","microrhinos" = "CHS.MICR",
                         "sordidus" = "CHS.SORD", "altipinnus" = "SCA.ALTI", "forsteni" = "SCA.FORS",
                         "frenatus" = "SCA.FREN", "globiceps" = "SCA.GLOB", "niger" = "SCA.NIGR",
                         "oviceps" = "SCA.OVIC", "rubroviolaceus" = "SCA.RUBR", "schlegeli" = "SCA.SCHL",
                         "spinus" = "SCA.SPIN")

spbp <- unique(biteprop$taxa)
modbp <- vector("list", length(spbp)) #initialise vector

for (i in 1:length(spbp)) {

  #fit model per species
  modbp[[i]] <- glm(scar ~ TL, data = biteprop %>%  filter(taxa == spbp[[i]]),
                    family = binomial(link = "logit"))
}

names(modbp) <- spbp

#####################################################
#FORMAT & SAVE MODEL FITS

#import maximum body length from FishBase for each species
#dataframe also has bite depth values (extracted from literature)
maxTL <- read.csv("Data/fish erosion/maxTL.csv")

#create master list of model fits & other info for each species
#C.frontalis, C. japanensis, S. longiceps and S.quoyi have no raw data but add to species list
speciesfits <- data.frame(taxa = c(spbr, "CHS.FRON", "CHS.JAPA", "SCA.LONG", "SCA.QUO")) %>%
  inner_join(maxTL)

####bite rate species substitutions
#C. frontalis and C.japanensis inherits C.bleekeri
#S. longipinnis inherits S. altipinnis
#S. quoyi inherits S.oviceps
speciesfits$modbr <- ifelse(speciesfits$taxa %in% c("CHS.FRON", "CHS.JAPA"), "CHS.BLEE",
                            ifelse(speciesfits$taxa == "SCA.LONG", "SCA.ALTI",
                                   ifelse(speciesfits$taxa == "SCA.QUO", "SCA.OVIC", speciesfits$taxa)))

####bite area species substitutions
# S. psittacus inherits S. schlegeli,
# S. longipinnis inherits S. altipinnis,
# C. bleekeri, C. frontalis, C. japanensis and Calotomus carolinus inherits C. sordidus
# S. rivulatus and S. ghobban inherits S. frenatus
# H.longiceps inherits S. forsteni
# B. muricatum inherits C. microrhinos
# S. quoyi, S. flavipectoralis, S. dimidiatus, S. chameleon and S. globiceps inherits S. oviceps
speciesfits$modba <- ifelse(speciesfits$taxa =="SCA.PSIT", "SCA.SCHL",
                            ifelse(speciesfits$taxa == "SCA.LONG", "SCA.ALTI",
                                   ifelse(speciesfits$taxa %in% c("CHS.BLEE", "CHS.FRON", "CHS.JAPA", "CAL.CARO"), "CHS.SORD",
                                          ifelse(speciesfits$taxa %in% c("SCA.RIVU", "SCA.GHOB"), "SCA.FREN",
                                                 ifelse(speciesfits$taxa == "HIP.LONG", "SCA.FORS",
                                                        ifelse(speciesfits$taxa == "BOL.MURI", "CHS.MICR",
                                                               ifelse(speciesfits$taxa %in% c("SCA.QUO", "SCA.FLAV", "SCA.DIMI", "SCA.CHAM", "SCA.GLOB"), "SCA.OVIC", speciesfits$taxa)))))))

#bite proportion species substitutions
#C. carolinus, S.chameleon & S. quoyi get 0 (no bites leave scars)
#all other substitutions as for bite area
speciesfits$modbp <- ifelse(speciesfits$taxa %in% c("CAL.CARO", "SCA.CHAM", "SCA.QUO"), NA,
                            speciesfits$modba)


##########################################################
####BIOEROSION SIMULATION

set.seed(101)

#initialise output vector of fitted models
fishequations <- vector("list", length = nrow(speciesfits))
fishdat <- NULL

for(i in 1:nrow(speciesfits)){

  if (is.na(speciesfits$modbp[i])) {

    fishequations[i] <- NA

  } else {

    nloops <- 1000 #number of simulations

    maxtl <- speciesfits[i,2]
    tl <- data.frame(TL = sample(1:maxtl, nloops, replace = TRUE)) #random sample of TLs

    #predict values w/ error based on random body lengths
    biterate <- predict(modbrexp[[speciesfits[i,6]]], newdata = tl, se.fit = TRUE)
    bitearea <- predict(modbalm[[speciesfits[i,7]]], newdata = tl, se.fit = TRUE)
    biteprop <- predict(modbp[[speciesfits[i,8]]], newdata = tl, se.fit = TRUE, type = "response") #extract probabilities

    if (is.na(speciesfits[i,3])) {
      bitedepth <- (speciesfits[i,5] * (tl ^ speciesfits[i,4]))[[1]]
    } else {
      bitedepth <- rep(speciesfits[i,3], nloops)
    }

    sub.density <- 0.000000001 * 1750 #fixed substrate density

    #initialise output data
    PE.total <- array(NA, dim=c(nloops,1))

    for(j in 1:nloops){

      #daily bite rate
      br <- exp(rnorm(365, biterate$fit[j], biterate$se.fit[j])) #cinvert from log-scale
      a <- -br / 230400
      bn <- (a*-447114806 + (a*608400+br)*1000.8) - (a*-85536000 + (a*608400+br)*360)

      bp <- biteprop$fit[j] #no variance for bite proportion
      totalbn <- sum(bn) * bp

      ba <- abs(rnorm(1, bitearea$fit[j], bitearea$se.fit[j]))
      bv <- (4/3) * ba * bitedepth[j] / 2

      PE.total[j,] <- totalbn * bv * sub.density
    }

    df <- data.frame(PE = PE.total[, 1], TL = tl$TL)
    fishequations[[i]] <- lm(PE ~ 0 + TL + I(TL^2) + I(TL^3), data = df) #fit polynomial function

  }
}


datalist = list()

for (i in 1:length(fishequations)){
  df <- as.data.frame(c(seq(0.1, speciesfits[i,2], by = 0.1)))
  colnames(df) <- "TL"

  if (is.na(fishequations[i])) {
    datalist[i] <- NA
  } else {
    fit <- predict(fishequations[[i]], newdata = df, interval = "prediction", se.fit = TRUE)

    df$species <- speciesfits[i,1]
    df <- cbind(df, fit$fit, fit$se.fit)

    datalist[[i]] <- df
  }
}


df_main <- do.call(rbind, datalist) %>%
  filter(!is.na(TL)) #3 species have no relationship to erosion because bite proportion is 0 - remove
colnames(df_main) <- c("X","taxa", "Y", "Y.lower", "Y.upper", "Y.se")
df_main <- as.data.frame(df_main)
df_main$Y <- as.numeric(as.character(df_main$Y))
df_main$X <- as.numeric(as.character(df_main$X))
df_main$Y.upper <- as.numeric(as.character(df_main$Y.upper))
df_main$Y.lower <- as.numeric(as.character(df_main$Y.lower))
df_main$Y.se <- as.numeric(as.character(df_main$Y.se))

write.csv(df_main, "pe_coefs.csv") #save as pe_coefs in sysdata
