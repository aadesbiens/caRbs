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

cbudget <- function(pp, sp, pe, se) {

  qss <- function(X, n, ...) {
    dens_X <- stats::density(X, ...)
    sample(dens_X$x, size = n, prob = dens_X$y, replace = TRUE)
  }

  sample_pp <- qss(pp$iters, n = 10000)
  sample_sp <- qss(sp$iters, n = 10000)
  sample_pe <- qss(pe$iters, n = 10000)
  sample_se <- qss(se$iters, n = 10000)

  conv <- sample_pp + sample_sp + sample_pe + sample_se

  values <- list(est = "carbonate budget", median = stats::median(conv),
                 sd = stats::sd(conv),  iters = conv)
  class(values) <- "carb"
  values

}

##########################################################################

#' @export
summary.carb <- function(object, ...) {
  x <- stats::density(object$iters)$x
  px <- stats::density(object$iters)$y
  cpx <- cumsum(px) / sum(px)

  lower_ci <- x[which(cpx >= 0.025)[1]]
  upper_ci <- x[which(cpx >= 0.975)[1]-1]

  tab <- c(median = object$median, sd = object$sd,
           lower_CI = lower_ci, upper_CI = upper_ci)

  class(tab) <- c("summaryCarb", "table")
  tab
}

#' @export
plot.carb <- function(x, ...) {
  d <- stats::density(x$iters)
  plot(d, main = paste(x$est), ...)
  graphics::abline(v = x$median, col = "red")
  graphics::abline(v = summary(x)[[4]], col = "blue", lty = 2)
  graphics::abline(v = summary(x)[[3]], col = "blue", lty = 2)
}

