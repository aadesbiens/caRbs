#' Secondary Production
#'
#' This function calculates carbonate production (kg/m2yr) of all secondary
#' (non-coral) reef components, namely, calcareous algae
#'
#' This function currently supports parameterisation of 4 calcifying algal taxa as listed below:
#'
#' | **Taxa**                                     | **Naming Convention**   |
#' | -------------------------------------------- | ----------------------- |
#' | Crustose corraline algae (*Corallinaceae*)   | "CCA" |
#' | *Halimeda* spp                               | "Halimeda' |
#' | Calcareous red macroalgae (*Amphiroa* spp)   | "Calcareous.Red.Macroalgae" |
#' | *Peyssonnelia* spp                           | "Peyssonnelia" |
#' @md
#'
#'@param species character vector of algal types as per the below naming convention
#'@param cover numeric vector of percentage cover (0-100) of each algal type
#'@param rug habitat rugosity score (0-5)
#'@param region character vector of GBR region ("north", "central" or "south")
#'@param shelf character vector of shelf position ("I" = inner, "M" = mid-shelf, "O" = outer)
#'
#'@return total secondary production (kg/m2yr)
#'@export

sproduction <- function(species, cover, rug, region, shelf) {

  thiscoef <- dplyr::inner_join(data.frame(species = species, region = region, shelf = shelf),
                                sa_coefs, by = c("species", "region", "shelf"))

  rug_conv <- 0.8657 + 0.1474*rug

  iters <- 10000
  runs <- vector("numeric", iters)

  for (i in 1:iters) {
    acc <- cover/100 * abs(stats::rnorm(nrow(thiscoef), thiscoef$est, thiscoef$sd)) * rug_conv
    runs[i] <- sum(acc)
  }

  values <- list(est = "secondary production", median = stats::median(runs),
                 sd = stats::sd(runs),  iters = runs)
  class(values) <- "carb"
  values

}
