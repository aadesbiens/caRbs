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
#'
#' @examples
#'
#'
#'
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
    acc <- ifelse(cover <= 0, 0, rnorm(1, thiscoef$Y, thiscoef$Y.sd) * thatcoef$conv/100)
    runs[i] <- sum(acc)
  }

  values <- list(est = "primary production", median = median(runs), sd = sd(runs),  iters = runs)
  class(values) <- "carb"
  values

}

################################################################################

#' Secondary Production
#'
#' This function calculates carbonate production (kg/m2yr) of all secondary
#' (non-coral) reef components, namely, calcareous algae
#'
#'@param substrate character vector of algal substrate types
#'@param cover numeric vector of percentage cover of each substrate type
#'@param rug numeric vector of chain rugosity
#'@param shelf character vector of shelf position (I = inner, M = mid-shelf, O = outer)
#'
#'@return total secondary production (kg/m2yr)
#'@export
#'
#'@examples
#'
#'
#'
sproduction <- function(species, cover, region, shelf) {

  thiscoef <- dplyr::inner_join(data.frame(species = species, region = region, shelf = shelf),
                                sa_coefs, by = c("species", "region", "shelf"))

  iters <- 10000
  runs <- vector("numeric", iters)

  for (i in 1:iters) {
    acc <- cover * rnorm(1, thiscoef$est, thiscoef$sd)
    runs[i] <- sum(acc)
  }

  values <- list(est = "secondary production", median = median(runs), sd = sd(runs),  iters = runs)
  class(values) <- "carb"
  values

}
