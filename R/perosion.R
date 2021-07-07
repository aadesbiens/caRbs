#' Primary Erosion
#'
#' This function uses fish survey data to quantify bioerosion (kg/m2yr)
#'
#' This function currently supports parameterisation for 23 fish species as below:
#'
#' | **Species**                   | **Naming Convention** |
#' | ----------------------------- | --------------------- |
#' | *Bolbometopon muricatum*      | "BOL.MURI"     |
#' | *Calotomus carolinus*         | "CAL.CARO"     |
#' | *Cetoscarus bicolor*          | "CET.BIC"      |
#' | *Chlorurus bleekeri*          | "CHS.BLEE"     |
#' | *Chlorurus frontalis*         | "CHS.FRON"     |
#' | *Chlorurus japanensis*        | "CHS.JAPA"     |
#' | *Chlorurus mircrorhinos*      | "CHS.MICR"     |
#' | *Chlorurus sordidus*          | "CHS.SORD"     |
#' | *Hipposcarus longiceps*       | "HIP.LONG"     |
#' | *Scarus altipinnis*           | "SCA.ALTI"     |
#' | *Scarus chameleon*            | "SCA.CHAM"     |
#' | *Scarus dimidatus*            | "SCA.DIMI"     |
#' | *Scarus flavipectoralis*      | "SCA.FLAV"     |
#' | *Scarus forsteni*             | "SCA.FORS"     |
#' | *Scarus frenatus*             | "SCA.FREN"     |
#' | *Scarus ghobban*              | "SCA.GHOB"     |
#' | *Scarus globiceps*            | "SCA.GLOB"     |
#' | *Scarus longipinnis*          | "SCA.LONG"     |
#' | *Scarus niger*                | "SCA.NIGR"     |
#' | *Scaru oviceps*               | "SCA.OVIC"     |
#' | *Scarus psittacus*            | "SCA.PSIT"     |
#' | *Scarus rivulatus*            | "SCA.RIVU"     |
#' | *Scarus rubroviolaceus*       | "SCA.RUBR"     |
#' | *Scarus schlegeli*            | "SCA.SCHL"     |
#' | *Scarus spinus*               | "SCA.SPIN"     |
#' | *Scarus quoyi*                | "SCA.QUO"      |
#' @md
#'
#' @param TL numeric vector of individual body length (cm)
#' @param species character vector of species codes as per the below naming convention
#' @param ta transect area (m2)
#' @param n numeric vector of count for each \code{species} of length \code{TL} per transect area (\code{ta}). Default of n = 1 for each species.
#'
#' @return total primary erosion (kg/m2yr)
#' @export

perosion <- function (TL, species, ta, n = 1) {

  thiscoef <- merge(data.frame(FISH_CODE = species), pe_coefs, by = c("FISH_CODE"))

  TL <- ifelse(TL > thiscoef$maxTL, thiscoef$maxTL, TL)

  iters <- 10000
  runs <- vector("numeric", iters)

  for (i in 1:iters) {

    dailybites <- function(TL, exp, exp_sd, off, off_sd) {

      br <- exp(-abs(stats::rnorm(365, exp, exp_sd)) * TL) * abs(stats::rnorm(365, off, off_sd))

      a <- -br / 230400
      bn <- (a*-447114806 + (a*608400+br)*1000.8) - (a*-85536000 + (a*608400+br)*360)

      totalbn <- sum(bn)
      totalbn

    }

    prop <- ifelse(TL < 15 , thiscoef$P14,
                   ifelse(TL >= 15 & TL < 20, thiscoef$P15_19,
                          ifelse(TL >= 20 & TL < 25, thiscoef$P20_24,
                                 ifelse(TL >= 25 & TL < 30, thiscoef$P25_29,
                                        ifelse(TL >= 30 & TL < 35, thiscoef$P30_34, thiscoef$P35)))))


    bn <- mapply(dailybites, TL, thiscoef$BR_Coef_exp, thiscoef$BR_Coef_exp_sd,
                 thiscoef$BR_Offset_exp, thiscoef$BR_Offset_exp_sd) * prop

    bn <- ifelse(thiscoef$FISH_CODE == "CHS.MICR", bn/7, bn)

    ba <- thiscoef$BS_Coef1 * TL + thiscoef$BS_Offset

    bv <- ifelse(is.na(thiscoef$BD_Other),
                 ((4/3) * ba * (thiscoef$BD_Offset_SCA * (TL ^ thiscoef$BD_Exp_SCA))) / 2,
                 ((4/3) * ba * (thiscoef$BD_Other)) / 2)

    total <- bn * bv * 0.000000001 * 1750

    total_per_m <- total / ta * n

    runs[i] <- -sum(total_per_m)

  }

  values <- list(est = "primary erosion", median = stats::median(runs),
                 sd = stats::sd(runs),  iters = runs)
  class(values) <- "carb"
  values

}
