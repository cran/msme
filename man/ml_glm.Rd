\name{ml_glm}
\alias{ml_glm}

\title{
A function to fit generalized linear models using maximum likelihood.
}

\description{
This function fits generalized linear models by maximizing the joint
log-likeliood, which is set in a separate function.  Only single-parameter
members of the exponential family are covered.
The post-estimation
output is designed to work with existing reporting functions.
}

\usage{
ml_glm(formula, data, family, link, offset = 0, start = NULL, verbose =
FALSE, ...)
}

\arguments{
  \item{formula}{
    an object of class '"formula"' (or one that can be coerced to
    that class): a symbolic description of the model to be
    fitted. (See the help for 'glm' for more details). 
}
  \item{data}{
    a data frame containing the variables in the model. 
}
  \item{family}{
    a description of the error distribution be used in the model.
    This must be a character string naming a family.
}
  \item{link}{
    a description of the link function be used in the model.
    This must be a character string naming a link function.
}
  \item{offset}{
    this can be used to specify an _a priori_ known component to
    be included in the linear predictor during fitting.  This
    should be 0 or a numeric vector of length equal to the
    number of cases. 
}
  \item{start}{
    optional starting points for the parameter estimation. 
}
  \item{verbose}{
    logical flag affecting the detail of printing.  Defaults to FALSE.
}
  \item{\dots}{
    optional arguments to pass within the function.
}

}

\details{
  The containing package, msme, provides the needed functions to
  use the ml_glm function to fit the Poisson and
  Bernoulli families, and supports the use of the
  identity, log, logit, probit, and complementary log-log link functions. 
  The object returned by the function is designed to be reported by the
  print.glm function. 
}

\value{
  \item{fit }{the output of optim.}
  \item{X }{the design matrix.}
  \item{y }{the response variable.}
  \item{call }{the call used for the function.}
  \item{obs }{the number of observations.}
  \item{df.null }{the degrees of freedom for the null model.}
  \item{df.residual }{the residual degrees of freedom.}
  \item{deviance }{the residual deviance.}
  \item{null.deviance }{the residual deviance for the null model.}
  \item{residuals }{the deviance residuals.}
  \item{coefficients }{parameter estimates.}
  \item{se.beta.hat }{standard errors of parameter estimates.}
  \item{aic }{Akaike's Information Criterion.}
  \item{i }{the number of iterations required for convergence.}
}

\references{
Hilbe, J.M., and Robinson, A.P. 2013. Methods of Statistical Model
Estimation. Chapman & Hall / CRC.
}

\author{
Andrew Robinson and Joe Hilbe.
}

\note{
This function is neither as comprehensive nor as stable as the inbuilt
glm function.  It is a lot easier to read, however.
}

\seealso{
\code{\link{irls}}, \code{\link{glm}}, \code{\link{ml_glm2}}
}

\examples{
data(medpar)

ml.poi <- ml_glm(los ~ hmo + white,
                 family = "poisson",
                 link = "log",
                 data = medpar)
ml.poi
summary(ml.poi)

}

\keyword{ ~models }

