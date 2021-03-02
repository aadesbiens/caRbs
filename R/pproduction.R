#' Primary Production
#'
#' This function uses coral cover data to calculate carbonate production (kg/m2yr)
#'
#' This function currently supports parameterisation for 56 coral taxa as below:
#'
#' | **Taxa**                         | **Naming Convention** |
#' | -------------------------------- | --------------------- |
#' | *Acanthastrea* spp               | "ACANTHASTREA" |
#' | *Acropora* spp (arborescent)     | "ACB" |
#' | *Acropora* spp (digitate)        | "ACD" |
#' | *Acropora* spp (encrusting)      | "ACE" |
#' | *Acropora* spp (corymbose)       | "ACO" |
#' | *Acropora* spp (submassive)      | "ACS" |
#' | *Acropora* spp (table)           | "ACT" |
#' | *Acropora* spp (bottlebrush)     | "ACX" |
#' | *Alveopora* spp                  | "ALVEOPORA" |
#' | *Astreopora* spp                 | "ASTREOPORA" |
#' | *Caulastrea* spp                 | "CAULASTREA" |
#' | *Coscinaraea* spp                | "COSCINARAEA" |
#' | *Cyphastrea* spp                 | "CYPHASTREA" |
#' | *Diploastrea* spp                | "DIPLOASTREA" |
#' | *Echinophyllia/Oxypora* (indeterminate) | "ECHINOPHYLLIA.OXYPORA" |
#' | *Echinophyllia* spp              | "ECHINOPHYLLIA" |
#' | *Echinopora* spp                 | "ECHINOPORA" |
#' | *Euphyllia* spp                  | "EUPHYLLIA" |
#' | *Favia* spp                      | "FAVIA" |
#' | *Favid* spp                      | "FAVID.SPP" |
#' | *Favites* spp                    | "FAVITES" |
#' | *Fungiidae* (solitary)           | "SOLITARY.FUNGID" |
#' | *Galaxea* spp                    | "GALAXEA" |
#' | *Gardineroseris* spp             | "GARDINEROSERIS" |
#' | *Goniastrea* spp                 | "GONIASTREA" |
#' | *Goniopora* spp                  | "GONIOPORA" |
#' | *Hydnophora* spp                 | "HYDNOPHORA" |
#' | *Isopora* spp                    | "ISOPORA" |
#' | *Leptastrea* spp                 | "LEPTASTREA" |
#' | *Leptoria* spp                   | "LEPTORIA" |
#' | *Leptoseris* spp                 | "LEPTOSERIS" |
#' | *Lopbophyllia* spp               | "LOBOPHYLLIA" |
#' | *Merulina* spp                   | "MERULINA" |
#' | *Montastrea* spp                 | "MONTASTREA" |
#' | *Montipora* spp                  | "MONTIPORA" |
#' | *Mycedium* spp                   | "MYCEDIUM" |
#' | *Oulophyllia* spp                | "OULOPHYLLIA" |
#' | *Oxypora* spp                    | "OXYPORA" |
#' | *Pachyseris* spp                 | "PACHYSERIS" |
#' | *Palauastrea* spp                | "PALAUASTREA" |
#' | *Paraclavarina* spp              | "PARACLAVARINA" |
#' | *Pavona* spp                     | "PAVONA" |
#' | *Pectinia* spp                   | "PECTINIA" |
#' | *Physogyra* spp                  | "PHYSOGYRA" |
#' | *Platygyra* spp                  | "PLATYGYRA" |
#' | *Plerogyra* spp                  | "PLEROGYRA" |
#' | *Plesiastrea* spp                | "PLESIASTREA" |
#' | *Pocillopora* spp                | "POCILLOPORA" |
#' | *Podabacia* spp                  | "PODABACIA" |
#' | *Porites* spp                    | "PORITES" |
#' | *Psammocora* spp                 | "PSAMMOCORA" |
#' | *Seriatopora* spp                | "SERIATOPORA" |
#' | *Stylocoeniella* spp             | "STYLOCOENIELLA" |
#' | *Stylophora* spp                 | "STYLOPHORA" |
#' | *Symphyllia* spp                 | "SYMPHYLLIA" |
#' | *Turbinaria* spp                 | "TURBINARIA" |
#'@md
#'
#' @param species character vector of coral types as per the below naming convention
#' @param cover numeric vector of percent cover (0-100%) of each coral type
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

