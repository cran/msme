## nbinomial   maximum likelihood NB2 and Heterogeneous NB
## Hilbe & Robinson, Methods of Statistical Model Estimation
## Chapman & Hall/CRC 2013
## msme package

nbinomial <- function(formula1,
                       formula2 = ~1,
                       data,
                       family = "nb2",
                       mean.link = "log",
                       scale.link = "inverse_s",
                       offset = 0,
                       start = NULL,
                       verbose = FALSE) {
  ml_glm2(formula1,
          formula2,
          data,
          family,
          mean.link,
          scale.link,
          offset = offset,
          start = start,
          verbose = verbose)
}

