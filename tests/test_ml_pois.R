
library(msme)
data(medpar)


## IDENTITY POISSON ----------------------------old-----------

ipoi <- ml_glm(los ~ hmo + white,
               family = "poisson", link = "log",
               data = medpar)
ipoi
summary(ipoi)


ipoi.g <- glm(los ~ hmo + white,
              family = poisson,
              data = medpar)
ipoi.g
summary(ipoi.g)








## RATE POISSON

oset <- rep(1:5, each=299, times=1)*100
loset <- log(oset)

rpoi <- ml_glm(los ~ hmo + white,
               family = "poisson", link = "log",
               offset = loset,
               data = medpar)
rpoi
summary(rpoi)

rpoi.g <- glm(los ~ hmo + white,
              family = poisson,
              offset = loset,
              data = medpar)
rpoi.g
summary(rpoi.g)

