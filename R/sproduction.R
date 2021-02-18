#' Secondary Production
#'
#' This function calculates carbonate production (kg/m2yr) of all secondary
#' (non-coral) reef components, namely, calcareous algae
#'
#'@param species character vector of algal types
#'@param cover numeric vector of percentage cover of each algal type
#'@param rug numeric vector of chain rugosity
#'@param region character vector of GBR region ("north", "central" or "south")
#'@param shelf character vector of shelf position ("I" = inner, "M" = mid-shelf, "O" = outer)
#'
#'@return total secondary production (kg/m2yr)
#'@export

sproduction <- function(species, cover, rug, region, shelf) {

  thiscoef <- dplyr::inner_join(data.frame(species = species, region = region, shelf = shelf),
                                sa_coefs, by = c("species", "region", "shelf"))

  iters <- 10000
  runs <- vector("numeric", iters)

  for (i in 1:iters) {
    acc <- cover/100 * abs(stats::rnorm(nrow(thiscoef), thiscoef$est, thiscoef$sd)) * rug
    runs[i] <- sum(acc)
  }

  values <- list(est = "secondary production", median = stats::median(runs),
                 sd = stats::sd(runs),  iters = runs)
  class(values) <- "carb"
  values

}
