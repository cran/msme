\name{ml_glm3}
\alias{ml_glm3}

\title{
A reduced maximum likelihood fitting function that omits null models.
}

\description{
This function fits generalized linear models by maximizing the joint
log-likeliood, which is set in a separate function. Null models 
are omitted from the fit.  The post-estimation
output is designed to work with existing reporting functions.

}
\usage{
ml_glm3(formula, data, family, link, offset = 0, start = NULL, verbose = FALSE, ...)
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
    other arguments to pass to the likelihood function, e.g. group stucture.
}
}

\details{
This function is essentially the same as ml_glm, but includes the dots
argument to allow a richer set of model likelihoods to be fit, and
omits computation of the null deviance.
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
  \item{null.deviance }{the residual deviance for the null model, set to
  NA.}
  \item{residuals }{the deviance residuals.}
  \item{coefficients }{parameter estimates.}
  \item{se.beta.hat }{standard errors of parameter estimates.}
  \item{aic }{Akaike's Information Criterion.}
  \item{i }{the number of iterations required for convergence.}
}

\references{
Hilbe, J.M., and Robinson, A.P. 2013. Methods of Statistical Model
Estimation. Chapman \& Hall / CRC.
}

\author{
Andrew Robinson and Joe Hilbe.
}

%\note{
%%  ~~further notes~~  %% FIXME - did we really need this function?
%}

\seealso{
\code{\link{irls}}, \code{\link{glm}}, \code{\link{ml_glm}}
}

\examples{

data(medpar)
med.nb.g <- ml_glm3(los ~ hmo + white,
                   family = "gNegBinomial", 
                   link = "log",
                   group = medpar$provnum, 
                   data = medpar)
summary(med.nb.g)

}

\keyword{ ~models }
