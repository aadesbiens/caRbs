cbudget <- function(A, B, C, D) {

  qss <- function(X, n, ...) {
    dens_X <- density(X, ...)
    sample(dens_X$x, size = n, prob = dens_X$y, replace = TRUE)
  }

  sample_A <- qss(A$data, n = 10000)
  sample_B <- qss(B$data, n = 10000)
  sample_C <- qss(C$data, n = 10000)
  sample_D <- qss(D$data, n = 10000)

  conv <- sample_A + sample_B + sample_C + sample_D

  values <- list(est = "carbonate budget", mean = mean(conv), sd = sd(conv),  data = conv)
  class(values) <- "carb"
  values

}


is.carb <- function(obj) inherits(obj, "carb")

summary.carb <- function(obj) {
  tab <- cbind(obj$mean, obj$sd)
  colnames(tab) <- c("median", "sd")
  rownames(tab) <- c(obj$est)
  print(tab)
}

plot.carb <- function(obj) {
  d <- density(obj$data)
  plot(d, main = paste(obj$est))
}
