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
#'@param rug habitat rugosity (either chain-link measurement or visual 0-5 scale)
#'@param chain logical if rugosity is chain-link, default is TRUE
#'@param region character vector of GBR region ("north", "central" or "south")
#'@param shelf character vector of shelf position ("I" = inner, "M" = mid-shelf, "O" = outer)
#'
#'@return total secondary production (kg/m2yr)
#'@export

sproduction <- function(species, cover, rug, chain = TRUE, region, shelf) {

  thiscoef <- merge(data.frame(species = species, region = region, shelf = shelf),
                                sa_coefs, by = c("species", "region", "shelf"))

  if (chain == TRUE) rug_conv <- rug else rug_conv <- 0.8657 + 0.1474*rug

  iters <- 10000
  runs <- vector("numeric", iters)

  for (i in 1:iters) {
    acc <-  abs(stats::rnorm(nrow(thiscoef), thiscoef$est*cover/100, thiscoef$sd)) * rug_conv
    runs[i] <- sum(acc)
  }

  values <- list(est = "secondary production", median = stats::median(runs),
                 sd = stats::sd(runs),  iters = runs)
  class(values) <- "carb"
  values

}
