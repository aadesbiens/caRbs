paccretion <- function(species, cover, lat) {

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

  values <- list(est = "primary accretion", mean = mean(runs), sd = sd(runs),  data = runs)
  class(values) <- "carb"
  values

}


saccretion <- function(species, cover, region, shelf) {

  thiscoef <- dplyr::inner_join(data.frame(species = species, region = region, shelf = shelf),
                                sa_coefs, by = c("species", "region", "shelf"))

  iters <- 10000
  runs <- vector("numeric", iters)

  for (i in 1:iters) {
    acc <- cover * rnorm(1, thiscoef$est, thiscoef$sd)
    runs[i] <- sum(acc)
  }

  values <- list(est = "secondary accretion", mean = mean(runs), sd = sd(runs),  data = runs)
  class(values) <- "carb"
  values

}
