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

