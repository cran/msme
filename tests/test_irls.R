library(msme)

# library(msme, lib.loc="lib")

library(MASS)

data(medpar)

denom <- rep(1:5, each=299, times=1)*100   # m : binomial denominator w medpar
oset <- rep(1:5, each=299, times=1)*100    # offset Poisson, NB, offset w medpar
loset <- log(oset)                         #    log of oset

## POISSON -------------------------------------------------

irls.poi <- irls(los ~ hmo + white,
                 family = "poisson",
                 link = "log",
                 data = medpar)

glm.poi <- glm(los ~ hmo + white,
               family = "poisson",
               data = medpar)

ml.poi <- ml_glm(los ~ hmo + white,
                 family = "poisson",
                 link = "log",
                 data = medpar)

ml.poi  
irls.poi
glm.poi

summary(ml.poi)
summary(irls.poi)
summary(glm.poi)

# Null deviance

irls(los ~ 1,
     family = "poisson",
     link = "log",
     data = medpar)$deviance

# cf
summary(glm.poi)$null.deviance

## RATE POISSON

irls.rpoi <- irls(los ~ hmo + white,
                  family = "poisson",
                  link = "log",
                  offset = loset,
                  data = medpar,
                  verbose = 1)
glm.rpoi <- glm(los ~ hmo + white + offset(loset),
                family = poisson,
                data = medpar)
ml.rpoi <- ml_glm(los ~ hmo + white,
                  family = "poisson", link = "log",
                  offset = loset,
                  data = medpar)

ml.rpoi
irls.rpoi
glm.rpoi

summary(ml.rpoi)
summary(irls.rpoi)
summary(glm.rpoi)

# How to obtain null deviance

irls(los ~ 1,
     family = "poisson",
     link = "log",
     offset = loset,
     data = medpar)$deviance

# cf

summary(glm.rpoi)$null.deviance


## IDENTITY POISSON ----------------------------old-----------

irls.ipoi <- irls(los ~ hmo + white,
                  family = "poisson",
                  link = "identity",
                  data = medpar)

glm.ipoi <- glm(los ~ hmo + white,
                family = poisson(link=identity),
                data = medpar)

ml.ipoi <- ml_glm(los ~ hmo + white,
                  family = "poisson", link = "identity",
                  data = medpar)

irls.ipoi
glm.ipoi
ml.ipoi

summary(irls.ipoi)
summary(glm.ipoi)
summary(ml.ipoi)

## NEGATIVE BINOMIAL (NB2) ------------------------------------------

irls.nb2 <- irls(los ~ hmo + white,
                 family = "negBinomial",
                 link = "log",
                 a = 0.5,
                 data = medpar)

glm.nb2 <- glm.nb(los ~ hmo + white,
                  data = medpar)

irls.nb2
glm.nb2

summary(irls.nb2)
summary(glm.nb2)

## The IRLS deviance is wrong because of the conflict between JLL functions.

## RATE NEGATIVE BINOMIAL (NB2)--------------------------------

irls.rnb2 <- irls(los ~ hmo + white,
                  family = "negBinomial",
                  link = "log",
                  a = 0.5,
                  offset=loset,
                  data = medpar)

glm.rnb2 <- glm.nb(los ~ hmo + white + offset(loset),
                   data = medpar)

irls.rnb2
glm.rnb2

summary(irls.rnb2)
summary(glm.rnb2)

#oset <- rep(1:5, each=299, times=1)*100
#loset <- log(oset)
#glm.rnb2 <- glm(los ~ hmo + white + offset(loset),
#                family = negBinomial(2),
#                data = medpar)
#summary(glm.rnb2)

## The IRLS deviance is wrong because of the conflict between JLL functions.

###  CANONICAL NEGATIVE BINOMIAL (NBC)-------------------------
irls.nbc <- irls(los ~ hmo + white,
                 family = "negBinomial",
                 link = "negbin",
                 a = 0.5,
                 data = medpar)
irls.nbc
summary(irls.nbc)


###  IDENTITY NEGATIVE BINOMIAL --------------------------
irls.nbi <- irls(los ~ hmo + white,
                 family = "negBinomial",
                 link = "identity",
                 a = 0.5,
                 data = medpar)
irls.nbi
summary(irls.nbi)



##  BERNOULLI LOGIT --------------------------------------------

irls.logit <- irls(died ~ hmo + white,
                   family = "binomial",
                   link = "logit",
                   data = medpar)
glm.logit <- glm(died ~ hmo + white,
                 family = "binomial",
                 data = medpar)
ml.logit <- ml_glm(died ~ hmo + white,
                  data = medpar,
                  family = "bernoulli",
                  link = "logit1")

ml.logit
irls.logit
glm.logit

summary(ml.logit)
summary(irls.logit)
summary(glm.logit)

## BINOMIAL LOGIT -------------------------------------------

denom <- rep(1:5, each=299, times=1)*100

irls.blogit <- irls(los ~ hmo + white,
                    family = "binomial",
                    link = "logit",
                    m = denom,
                    data = medpar)

not.los <- denom - medpar$los
glm.blogit <- glm(cbind(los, not.los) ~ hmo + white,
                  family = "binomial",
                  data = medpar)

glm.blogit
irls.blogit

summary(irls.blogit)
summary(glm.blogit)

## BERNOULLI PROBIT --------------------------------------------

irls.probit <- irls(died ~ hmo + white,
                    family = "binomial",
                    link = "probit",
                    data = medpar)
glm.probit <- glm(died ~ hmo + white,
                  family = binomial(link=probit),
                  data = medpar)


irls.probit
glm.probit

summary(irls.probit)
summary(glm.probit)

## BINOMIAL PROBIT------------------------------------------

denom <- rep(1:5, each=299, times=1)*100

irls.bprobit <- irls(los ~ hmo + white,
                     family = "binomial",
                     link = "probit",
                     m = denom,
                     data = medpar,
                     verbose = 1)

not.los <- denom - medpar$los
glm.bprobit <- glm(cbind(los, not.los) ~ hmo + white,
                   family = binomial(link=probit),
                   data = medpar)


irls.bprobit
glm.bprobit

summary(irls.bprobit)
summary(glm.bprobit)

## BERNOULLI CLOGLOG --------------------------------------------

irls.cloglog <- irls(died ~ hmo + white,
                     family = "binomial",
                     link = "cloglog",
                     data = medpar,
                     verbose = 1)

glm.cloglog <- glm(died ~ hmo + white,
                   family = binomial(link=cloglog),
                   data = medpar)

irls.cloglog
glm.cloglog

summary(irls.cloglog)
summary(glm.cloglog)


## BINOMIAL CLOGLOG

denom <- rep(1:5, each=299, times=1)*100

irls.bcloglog <- irls(los ~ hmo + white,
                      family = "binomial",
                      link = "cloglog",
                      m = denom,
                      data = medpar,
                      verbose = 1)

not.los <- denom - medpar$los
glm.bcloglog <- glm(cbind(los, not.los) ~ hmo + white,
                    family = binomial(link=cloglog),
                    data = medpar)

irls.bcloglog
glm.bcloglog

summary(irls.bcloglog)
summary(glm.bcloglog)


## GAMMA CANONICAL--------------------------------------
#irls.gam <- irls(los ~ hmo + white,
#                 family = "gamma",
#                 link = "inverse",
#                 data = medpar)

#glm.gam <- glm(los ~ hmo + white,
#               family = Gamma,
#               data = medpar)

#irls.gam
#glm.gam

#summary(irls.gam)
#summary(glm.gam)


## INVERSE GAUSSIAN CANONICAL--------------------------------------
#irls.ivg <- irls(los ~ hmo + white,
#                 family = "inv_gauss",
#                 link = "inverse2",
#                 data = medpar)
#glm.ivg <- glm(los ~ hmo + white,
#               family = inverse.gaussian,
#               data = medpar)

#irls.ivg
#glm.ivg

#summary(irls.ivg)
#summary(glm.ivg)


## POST-ESTIMATION STATISTICS --------------------------------------

## MODEL COEFFICIENTS
irls.logit$coefficients

## MODEL STANDARD ERRORS
irls.logit$se.beta.hat

## NULL DEGREES OF FREEDOM
irls.logit$df.null

## NUMBER OF OBSERVATIONS IN MODEL
irls.logit$nobs

## RESIDUAL DEGREES OF FREEDOM
irls.logit$df.residual

## DEVIANCE
irls.logit$deviance

## NULL DEVIANCE (INTERCEPT ONLY)
# irls.logit$null.deviance

## PEARSON CHI2 DISPERSION
irls.logit$p.dispersion

## PEARSON CHI2 STATISTIC
irls.logit$pearson

## LOG-LIKELIHOOD
irls.logit$loglik

## AIC STATISTIC
irls.logit$aic

## AIC STATISTIC WITH N
irls.logit$aic/irls.logit$nobs

## NUMBER MODEL PREDICTORS, INCLUDING INTERCEPT
irls.logit$i

## MODEL CALL TO IRLS FUNCTION
irls.logit$call

## GLM FAMILY USED FOR MODEL
irls.logit$family

## MU OR FIT VALUES
mu <- irls.logit$mu
summary(mu)

## ETA OR LINEAR PREDICTOR VALUES
eta <- irls.logit$eta
summary(eta)

## DEVIANCE RESIDUALS
dev.res <- irls.logit$residuals
summary(dev.res)

## STANDARDIZED DEVIANCE RESIDUALS
# residuals(irls.logit, type="standard")
stdres <- residuals(irls.logit, type="standard")
str(stdres)

## HAT MATRIX DIAGONAL STATISTICS
# hatvalues(irls.logit)
hat <- hatvalues(irls.logit)
str(hat)

## DISPLAYS DATA MATRIX VALUES
X <- irls.logit$X
str(X)
