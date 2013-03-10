\name{medpar}
\alias{medpar}
\docType{data}
\title{
US national Medicare inpatient hospital database for Arizona patients.
}
\description{
%% ~~ description of the dataset. ~~ The US national Medicare inpatient
hospital database is referred to as the Medpar data, which is prepared
yearly from hospital filing records. Medpar files for each state are
also prepared. The full Medpar data consists of 115 variables. The
national Medpar has some 14 million records, with one record for each
hospilitiztion. The data in the medpar file comes from 1991 Medicare
files for the state of Arizona. The data are limited to only one
diagnostic group (DRG 112). Patient data have been randomly selected
from the original data.  }
\usage{data(medpar)}
\format{
  A data frame with 1495 observations on the following 10 variables.
  \describe{
    \item{\code{los}}{length of hospital stay}
    \item{\code{hmo}}{Patient belongs to a Health Maintenance Organization, binary}
    \item{\code{white}}{Patient identifies themselves as Caucasian, binary}
    \item{\code{died}}{Patient died, binary}
    \item{\code{age80}}{Patient age 80 and over, binary}
    \item{\code{type}}{Type of admission, categorical}
    \item{\code{type1}}{Elective admission, binary}
    \item{\code{type2}}{Urgent admission,binary}
    \item{\code{type3}}{Elective admission, binary}
    \item{\code{provnum}}{Provider ID}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
Medpar is saved as a data frame.
Count models use los as response variable. 0 counts are structurally excluded  
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
1991 National Medpar data, National Health Economics & Research Co.
}
\references{
Hilbe, Joseph M (2007, 2011), Negative Binomial Regression, Cambridge University Press
Hilbe, Joseph M (2009), Logistic Regression Models, Chapman & Hall/CRC
first used in Hardin, JW and JM Hilbe (2001, 2007), Generalized Linear Models and Extensions, Stata Press
}
\examples{
data(medpar)
glmp <- glm(los ~ hmo + white + factor(type),
            family = poisson, data = medpar)
summary(glmp)
exp(coef(glmp))

ml.p <- ml_glm(los ~ hmo + white + factor(type),
               family = "poisson",
               link = "log",
               data = medpar)

summary(ml.p)

library(MASS)
glmnb <- glm.nb(los ~ hmo + white + factor(type),
                data = medpar)
summary(glmnb)
exp(coef(glmnb))

}
\keyword{datasets}
