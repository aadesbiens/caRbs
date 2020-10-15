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

  values <- list(est = "carbonate budget", median = median(conv), sd = sd(conv),  iters = conv)
  class(values) <- "carb"
  values

}

##########################################################################

is.carb <- function(obj) inherits(obj, "carb")

summary.carb <- function(obj) {
  x <- density(obj$iters)$x
  px <- density(obj$iters)$y
  cpx <- cumsum(px) / sum(px)

  lower_ci <- x[which(cpxx >= 0.025)[1]]
  upper_ci <- x[which(cpxx >= 0.975)[1]-1]

  tab <- c(median = obj$median, sd = obj$sd,
           lower_CI = lower_ci, upper_CI = upper_ci)

  class(tab) <- c("summaryCarb", "table")
  tab
}

plot.carb <- function(obj, ...) {
  d <- density(obj$iters)
  plot(d, main = paste(obj$est), ...)
}

