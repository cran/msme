

#################################################################################

library(msme)

data(ufc)
ufc <- na.omit(ufc)

## Normal

test.1.glm <- glm(height.m ~ dbh.cm,
                family = gaussian,
                data = ufc)

summary(test.1.glm)

test.1.g <- ml_glm2(height.m ~ dbh.cm,
                    formula2 = ~1,
                    data = ufc,
                    family = "normal",
                    mean.link = "identity",
                    scale.link = "identity_s")

summary(test.1.g)

test.1a.g <- ml_glm2(height.m ~ dbh.cm,
                    formula2 = ~1,
                    data = ufc,
                    family = "normal",
                    mean.link = "identity",
                    scale.link = "log_s")

summary(test.1a.g)

test.1b.g <- ml_glm2(height.m ~ dbh.cm,
                    formula2 = ~1,
                    data = ufc,
                    family = "normal",
                    mean.link = "identity",
                    scale.link = "inverse_s")

summary(test.1b.g)

test.2.g <- ml_glm2(height.m ~ dbh.cm,
                    formula2 = ~ dbh.cm,
                    data = ufc,
                    family = "normal",
                    mean.link = "identity",
                    scale.link = "log_s")

summary(test.2.g)

##################################################################

## Negative Binomial 2 

data(medpar)

library(MASS)

test.3.glm <- glm.nb(los ~ hmo + white,
                     data = medpar)

summary(test.3.glm)

test.3.g <- ml_glm2(los ~ hmo + white,
                    formula2 = ~1,
                    data = medpar,
                    family = "negBinomial",
                    mean.link = "log",
                    scale.link = "inverse_s")

summary(test.3.g)

oset <- rep(1:5, each=299, times=1)*100  # offset Poisson, NB, offset w medpar
loset <- log(oset)                       # log of oset

test.4.glm <- glm.nb(los ~ hmo + white + offset(loset),
                     data = medpar)

summary(test.4.glm)

test.4.g <- ml_glm2(los ~ hmo + white,
                    formula2 = ~1, 
                    data = medpar,
                    offset = loset,
                    family = "negBinomial",
                    mean.link = "log",
                    scale.link = "inverse_s")

summary(test.4.g)

##################################################################

## Gamma (inverse link) ## Mostly working 

test.5.glm <- glm(los ~ hmo + white,
                  family = Gamma,
                  data = medpar)

summary(test.5.glm)

test.5.g <- ml_glm2(los ~ hmo + white,
                    formula2 = ~1, 
                    data = medpar,
                    family = "gamma",
                    mean.link = "inverse",
                    scale.link = "inverse_s")

summary(test.5.g)

## Fit seems to be slightly 'better' when scale is estimated at the
## same time when assessed by AIC and SE, but deviance is identical
## ... therefore the (supposed) error in the LL is cancelling out.
