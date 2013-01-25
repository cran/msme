\name{irls}
\alias{irls}

\title{
  Function to fit generalized linear models using IRLS.
}

\description{
  This function fits a wide range of generalized linear models using the
  iteratively reweighted least squares algorithm.  The intended benefit
  of this function is for teaching.  Its scope is similar to that of R's
  glm function, which should be preferred for operational use.
}

\usage{
irls(formula, data, family, link, tol = 1e-06, offset = 0, m = 1, a = 1, verbose = 0)
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
  \item{tol}{
    an optional quantity to use as the convergence criterion for the
    change in deviance.
}
  \item{offset}{
    this can be used to specify an _a priori_ known component to
    be included in the linear predictor during fitting.  This
    should be 0 or a numeric vector of length equal to the
    number of cases. 
}
  \item{m}{
    the number of cases per observation for binomial regression.
}
  \item{a}{
    the scale for negative binomial regression.
}
  \item{verbose}{
    a flag to control the amount of output printed by the function.
}
}
\details{
   All statistics are computed at the final
  iteration of the IRLS algorithm. The convergence criterion is the
  magnitude of the change in deviance. The object returned by the
  function is designed to be reported by the print.glm function. 
}

\value{
\item{coefficients }{parameter estimates.}
\item{se.beta.hat }{standard errors of parameter estimates.}
\item{model }{the final, weighted linear model.}
\item{call }{the function call used to create the object.}
\item{nobs }{the number of observations.}
\item{eta }{the linear predictor at the final iteration.}
\item{mu }{the estimated mean at the final iteration.}
\item{df.residual }{the residual degrees of freedom.}
\item{df.null }{the degrees of freedom for the null model.}
\item{deviance }{the residual deviance.}
\item{null.deviance }{a place-holder for the null deviance - returned as NA}
\item{p.dispersion }{Pearsons's Chi-squared statistic.}
\item{pearson }{Pearson's deviance.}
\item{loglik }{the maximized log-likelihood.}
\item{family }{the chosen family.}
\item{X }{the design matrix.}
\item{i }{the number of iterations required for convergence.}
\item{residuals }{the deviance residuals.}
\item{aic }{Akaike's Information Criterion.}
}

\references{
Hilbe, J.M., and Robinson, A.P. 2013. Methods of Statistical Model
Estimation. Chapman \& Hall / CRC.
}

\author{
Andrew Robinson and Joe Hilbe.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{glm}}, \code{\link{ml_glm}}
}

\examples{
data(medpar)

irls.poi <- irls(los ~ hmo + white,
                 family = "poisson",
                 link = "log",
                 data = medpar)
summary(irls.poi)

irls.probit <- irls(died ~ hmo + white,
                    family = "binomial",
                    link = "probit",
                    data = medpar)
summary(irls.probit)

}

\keyword{ ~models }

