#' Primary Production
#'
#' This function uses coral cover data to calculate carbonate production (kg/m2yr)
#'
#' @param species character vector of species names as per AIMS convention
#' @param cover numeric vector of percent cover of each species
#' @param lat numeric vector of latitude
#'
#' @return total primary production (kg/m2yr)
#' @export

pproduction <- function(species, cover, lat) {

  cover <- round(cover, 0)
  lat <- round(abs(lat), 0)

  thiscoef <- dplyr::inner_join(data.frame(taxa = species, X = cover),
                                pa_coefs, by = c("taxa", "X"))
  thatcoef <- dplyr::inner_join(data.frame(latitude = lat),
                                lat_coefs, by = "latitude")

  iters <- 10000
  runs <- vector("numeric", iters)

  for (i in 1:iters) {
    acc <- abs(stats::rnorm(nrow(thiscoef), thiscoef$Y, thiscoef$Y.sd)) * thatcoef$conv/100
    runs[i] <- sum(acc)
  }

  values <- list(est = "primary production", median = stats::median(runs),
                 sd = stats::sd(runs),  iters = runs)
  class(values) <- "carb"
  values

}

################################################################################

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
