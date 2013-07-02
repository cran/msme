\name{alrt}
\alias{alrt}

\title{
Function to compute asymptotic likelihood ratio test of two models.
}

\description{
This function computes the asymptotic likelihood ratio test of two
models by comparing twice the different in the log-likelihoods of the
models with the Chi-squared distribution with degrees of freedom equal
to the difference in the degrees of freedom of the models.
}

\usage{
alrt(x1, x2, boundary = FALSE)
}

\arguments{
  \item{x1}{
    A fitted model as an object that logLik will work for.
}
  \item{x2}{
    A fitted model as an object that logLik will work for.
}
  \item{boundary}{
    A flag that reports whether a boundary correction should be made.
}
}

\value{
  \item{out.tab }{A data frame that summarizes the test.}
  \item{jll.diff }{The difference between the log-likelihoods.}
  \item{df.diff }{The difference between the degrees of freedom.}
  \item{p.value }{The p-value of the statistical test of the null
  hypothesis that there is no difference between the fit of the models.}
}

\references{
Hilbe, J.M., and Robinson, A.P. 2013. Methods of Statistical Model
Estimation. Chapman & Hall / CRC.
}

\author{
Andrew Robinson and Joe Hilbe.
}

\note{
The function does not provide any checks for nesting, data equivalence,
etc.
}

\seealso{
\code{\link{ml_glm}}, \code{\link{ml_glm2}}
}

\examples{
data(medpar)

ml.poi.1 <- ml_glm(los ~ hmo + white,
                   family = "poisson",
                   link = "log",
                   data = medpar)

ml.poi.2 <- ml_glm(los ~ hmo,
                   family = "poisson",
                   link = "log",
                   data = medpar)

alrt(ml.poi.1, ml.poi.2)

}

\keyword{ ~models }
