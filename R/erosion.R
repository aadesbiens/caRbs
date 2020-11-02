#' Primary Erosion
#'
#' This function uses fish survey data to quantify erosion (kg/m2yr)
#'
#' @param TL numeric vector of individual lengths
#' @param species character vector of species codes as per AIMS convention for each individual
#' @param ta transect area (m2)
#'
#' @return total primary erosion (kg/m2yr)
#' @export

perosion <- function (TL, species, ta) {

  thiscoef <- dplyr::inner_join(data.frame(FISH_CODE = species), pe_coefs, by = c("FISH_CODE"))

  TL <- ifelse(TL > thiscoef$maxTL, thiscoef$maxTL, TL)

  iters <- 10000
  runs <- vector("numeric", iters)

  for (i in 1:iters) {

    dailybites <- function(TL, exp, exp_sd, off, off_sd) {

      br <- exp(-abs(stats::rnorm(365, exp, exp_sd)) * TL) * abs(stats::rnorm(365, off, off_sd))

      a <- -br / 230400
      bn <- (a*-377353291 + (a*608400+br)*880.8) - (a*-85536000 + (a*608400+br)*360)

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

    total_per_m <- total / ta

    runs[i] <- -sum(total_per_m)

  }

  values <- list(est = "primary erosion", median = stats::median(runs),
                 sd = stats::sd(runs),  iters = runs)
  class(values) <- "carb"
  values

}

###########################################################################

#' Secondary Erosion
#'
#' This function calculates total erosion (kg/m2yr) of all secondary (non-fish)
#' reef components, namely, micro- and macro-borers
#'
#' @param cover numeric vector of percentage cover of non-coral substrate
#' @param rug numeric vector of chain rugosity
#' @param shelf character vector of shelf position (I = inner, M = mid-shelf, O = outer)
#'
#' @return total secondary erosion (kg/m2yr)
#' @export

serosion <- function (cover, rug, shelf) {

  area <- cover * rug / 100

  iters <- 10000
  runs <- vector("numeric", iters)

  for (i in 1:iters) {

    micro <- ifelse(shelf == "I", abs(stats::rnorm(length(area), 0.43, 0.02)) * area,
                    ifelse(shelf == "M", abs(stats::rnorm(length(area), 0.89, 0.02)) * area,
                           abs(stats::rnorm(length(area), 1.19, 0.08)) * area))

    macro <- ifelse(shelf == "I", abs(stats::rnorm(length(area), 1.53, 0.23)) * area ,
                    ifelse(shelf == "M", abs(stats::rnorm(length(area), 0.37, 0.16)) * area,
                           abs(stats::rnorm(length(area), 0.34, 0.05)) * area))

    runs[i] <- -(micro + macro)

  }

  values <- list(est = "secondary erosion", median = stats::median(runs),
                 sd = stats::sd(runs), iters = runs)
  class(values) <- "carb"
  values

}
