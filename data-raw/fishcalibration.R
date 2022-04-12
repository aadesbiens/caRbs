#load required packages
library("latticeExtra")
library("dplyr")
library("tidyr")
library("forcats")
library("tidylog", warn.conflicts = FALSE)

################################################
##BITE RATES

#import data from local drive
biterates <- read.csv("ameliadesbiens/Desktop/NESP caRbs/Data/fish erosion/bite_rates.csv") %>%
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

# Fit equations and compare AICs
spbr <- unique(biterates$taxa) #extract species names
modbrexp <- vector("list",length(spbr)) #create empty list to fill in for loop
modbrlm <- vector("list",length(spbr)) #as above
AICbr <- NULL #as above

#iterate across each species
for (i in 1:length(spbr)) {

  #fit models
  modbrexp[[i]] <- lm(log(bite_rate)~TL, data = biterates %>% filter(taxa == spbr[i])) #exponential fit
  modbrlm[[i]] <- lm(bite_rate~TL, data = biterates %>% filter(taxa == spbr[i])) #linear fit

  #extract AICs
  AICbr <- rbind(AICbr, data.frame(taxa = spbr[i],
                                   exp = extractAIC(modbrexp[[i]])[2],
                                   lm = extractAIC(modbrlm[[i]])[2]) %>%
                   mutate(best = names(.)[which.min(.)]))

}

AICbr ###inspect - exponential fit for all species has lower AIC
names(modbrexp) <- spbr

#########################################################
##BITE DIMENSIONS

#redo all fit procedures with bite dimension data
bitedimensions <- read.csv("ameliadesbiens/Desktop/NESP caRbs/Data/fish erosion/bite_dimensions.csv") %>%
  filter(Bite_length_mm > 0) %>%
  mutate(ellipse = pi* (Bite_length_mm/2) * (Bite_width_mm/2))  %>% #convert length x width to ellipse for area calculation
  rename(TL = TL_cm) %>%
  filter(!Species %in% c("bleekeri", "dimidiadus", "flavipectoralis", "ghobban", "globiceps", "rivulatus")) #not enough data for these species

#rename species codes as above
bitedimensions$taxa <-  recode(bitedimensions$Species, "bicolor" = "CET.BIC","microrhinos" = "CHS.MICR", "sordidus" = "CHS.SORD",
                               "altipinnus" = "SCA.ALTI", "forsteni" = "SCA.FORS", "frenatus" = "SCA.FREN",
                               "niger" = "SCA.NIGR", "oviceps" = "SCA.OVIC", "rubroviolaceus" = "SCA.RUBR",
                               "schlegeli" = "SCA.SCHL", "spinus" = "SCA.SPIN")

#initialise vectors as above
spba <- unique(bitedimensions$taxa)
modbaexp <- vector("list", length(spba))
modbalm <- vector("list", length(spba))
AICba <- NULL

for (i in 1:length(spba)) {

  #fit models
  modbaexp[[i]] <- lm(log(ellipse)~TL, data = bitedimensions %>% filter(taxa == spba[i]))
  modbalm[[i]] <- lm(ellipse~TL, data = bitedimensions %>% filter(taxa == spba[i]))

  #compare AICs
  AICba <- rbind(AICba, data.frame(taxa = spba[i],
                                   exp = extractAIC(modbaexp[[i]])[2],
                                   lm = extractAIC(modbalm[[i]])[2]) %>%
                   mutate(best = names(.)[which.min(.)]))
}

AICba ###inspect - exponential fit for all species has lower AIC

names(modbaexp) <- spba

####################################
##BITE PROPORTIONS
biteprop <- read.csv("ameliadesbiens/Desktop/NESP caRbs/Data/fish erosion/bite_dimensions.csv") %>%
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

#iterate over each species
for (i in 1:length(spbp)) {
  modbp[[i]] <- glm(scar ~ TL, data = biteprop %>%  filter(taxa == spbp[[i]]),
                    family = binomial(link = "logit")) #binomial regression
}

names(modbp) <- spbp

#################################
#FORMAT & SAVE MODEL FITS

#import maximum body length from FishBase for each species
#dataframe also has bite depth values (extracted from literature)
maxTL <- read.csv("ameliadesbiens/Desktop/NESP caRbs/Data/fish erosion/maxTL.csv")

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


##################################################
####BIOEROSION SIMULATION

set.seed(101)

#initialise output vector of fitted models
fishequations <- vector("list", length = nrow(speciesfits))

for(i in 1:nrow(speciesfits)){

  if (is.na(speciesfits$modbp[i])) {

    fishequations[i] <- NA

  } else {

    nloops <- 1000 #number of simulations

    maxtl <- speciesfits[i,2]
    tl <- data.frame(TL = sample(1:maxtl, nloops, replace = TRUE)) #random sample of TLs

    #predict values w/ error based on random body lengths
    biterate <- predict(modbrexp[[speciesfits[i,6]]], newdata = tl, se.fit = TRUE)
    bitearea <- predict(modbaexp[[speciesfits[i,7]]], newdata = tl, se.fit = TRUE)
    biteprop <- predict(modbp[[speciesfits[i,8]]], newdata = tl, se.fit = TRUE)

    if (is.na(speciesfits[i,3])) {
      bitedepth <- (speciesfits[i,5] * (tl ^ speciesfits[i,4]))[[1]]
    } else {
      bitedepth <- rep(speciesfits[i,3], nloops)
    }

    sub.density <- 0.000000001 * 1750 #fixed substrate density

    #initialise output data
    PE.total <- array(NA, dim=c(nloops,1))

    for(j in 1:nloops){

      br <- exp(rnorm(365, biterate$fit[j], biterate$se.fit[j])) #daily bite rate
      a <- -br / 230400
      bn <- (a*-447114806 + (a*608400+br)*1000.8) - (a*-85536000 + (a*608400+br)*360)

      bp <- rnorm(1, biteprop$fit[j], biteprop$se.fit[j])
      totalbn <- sum(bn) * (exp(bp) / (1 + exp(bp)))

      ba <- exp(rnorm(1, bitearea$fit[j], bitearea$se.fit[j]))
      bv <- (4/3) * ba * bitedepth[j] / 2

      PE.total[j,] <- totalbn * bv * sub.density
    }

    df <- data.frame(PE = PE.total[, 1], TL = tl$TL)
    fishequations[[i]] <- lm(log(PE) ~ 0 + TL, data = df)
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
df_main$Y <- -exp(as.numeric(as.character(df_main$Y))) #make negative because erosion not production
df_main$X <- as.numeric(as.character(df_main$X))
df_main$Y.upper <- -exp(as.numeric(as.character(df_main$Y.upper)))
df_main$Y.lower <- -exp(as.numeric(as.character(df_main$Y.lower)))
df_main$Y.se <- -exp(as.numeric(as.character(df_main$Y.se)))

write.csv(df_main, "pe_coefs.csv") #save as pe_coefs in sysdata
