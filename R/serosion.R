#' Secondary Erosion
#'
#' This function calculates total erosion (kg/m2yr) of all secondary (non-fish)
#' reef components, namely, micro- and macro-borers
#'
#' @param cover numeric vector of percentage cover (0-100) of non-coral substrate
#' @param rug habitat rugosity score (0-5)
#' @param shelf character vector of shelf position ("I" = inner, "M" = mid-shelf, "O" = outer)
#'
#' @return total secondary erosion (kg/m2yr)
#' @export

serosion <- function (cover, rug, shelf) {

  rug_conv <- 0.8657 + 0.1474*rug

  area <- cover * rug_conv / 100

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
