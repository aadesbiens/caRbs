#' Carbonate Budget
#'
#' This function combines outputs of \code{pproduction}, \code{sproduction},
#' \code{perosion} & \code{serosion} to calculate overall carbonate budget
#'
#' @param pp object of class \code{carb} pertaining to primary production
#' @param sp object of class \code{carb} pertaining to secondary production
#' @param pe object of class \code{carb} pertaining to primary erosion
#' @param se object of class \code{carb} pertaining to secondary erosion
#'
#' @return carbonate budget (net kg/m2yr)
#' @export
#'
#' @examples
#'
#'
#'
cbudget <- function(pp, sp, pe, se) {

  qss <- function(X, n, ...) {
    dens_X <- density(X, ...)
    sample(dens_X$x, size = n, prob = dens_X$y, replace = TRUE)
  }

  sample_pp <- qss(pp$data, n = 10000)
  sample_sp <- qss(sp$data, n = 10000)
  sample_pe <- qss(pe$data, n = 10000)
  sample_se <- qss(se$data, n = 10000)

  conv <- sample_pp + sample_sp + sample_pe + sample_se

  values <- list(est = "carbonate budget", median = median(conv), sd = sd(conv),  data = conv)
  class(values) <- "carb"
  values

}

##########################################################################

is.carb <- function(obj) inherits(obj, "carb")

summary.carb <- function(obj) {
  tab <- cbind(obj$median, obj$sd)
  colnames(tab) <- c("median", "sd")
  rownames(tab) <- c(obj$est)
  print(tab)
}

plot.carb <- function(obj) {
  d <- density(obj$data)
  plot(d, main = paste(obj$est))
}
