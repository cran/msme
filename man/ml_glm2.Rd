\name{ml_glm2}
\alias{ml_glm2}

\title{
A function to fit generalized linear models using maximum likelihood.
}

\description{
This function fits generalized linear models by maximizing the joint
log-likeliood, which is set in a separate function.  Two-parameter
members of the exponential family are covered. The post-estimation
output is designed to work with existing reporting functions.
}
\usage{
ml_glm2(formula1, formula2 = ~1, data, family, mean.link, scale.link,
        offset = 0, start = NULL, verbose = FALSE)
}

\arguments{
  \item{formula1}{
    an object of class '"formula"' (or one that can be coerced to
    that class): a symbolic description of the mean function for the model to be
    fitted. (See the help for 'glm' for more details). 
}
  \item{formula2}{
    an object of class '"formula"' (or one that can be coerced to
    that class): a symbolic description of the scale function for the model to be
    fitted. (See the help for 'glm' for more details). 
}
  \item{data}{
    a data frame containing the variables in the model. 
}
  \item{family}{
    a description of the error distribution be used in the model.
    This must be a character string naming a family.
}
  \item{mean.link}{
    a description of the link function be used for the mean in the model.
    This must be a character string naming a link function.
}
  \item{scale.link}{
    a description of the link function be used for the scale in the model.
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
}

\details{
  The containing package, msme, provides the needed functions to
  use the ml_glm2 function to fit the normal and negative binomial (2),
  families, and supports the use of the
  identity and log link functions. 

  The object returned by the function is designed to be reported by the
  print.glm function. 
}

\value{
  \item{fit }{the output of optim.}
  \item{loglike }{the maximized log-likelihood.}
  \item{X }{the design matrix.}
  \item{y }{the response variable.}
  \item{p }{the number of parameters estimated.}
  \item{rank }{the rank of the design matrix for the mean function.}
  \item{call }{the call used for the function.}
  \item{obs }{the number of observations.}
  \item{fitted.values }{estimated response variable.}
  \item{linear.predictor}{linear predictor.}
  \item{df.null }{the degrees of freedom for the null model.}
  \item{df.residual }{the residual degrees of freedom.}
  \item{pearson }{the Pearson Chi2.}
  \item{null.pearson }{the Pearson Chi2 for the null model.}
  \item{dispersion }{the dispersion.}
  \item{deviance }{the residual deviance.}
  \item{null.deviance }{the residual deviance for the null model.}
  \item{residuals }{the deviance residuals.}
  \item{presiduals }{the Pearson residuals.}
  \item{coefficients }{parameter estimates.}
  \item{se.beta.hat }{standard errors of parameter estimates.}
  \item{aic }{Akaike's Information Criterion.}
  \item{offset }{the offset used.}
  \item{i }{the number of iterations required for convergence.}
}

\references{
Hilbe, J.M., and Robinson, A.P. 2013. Methods of Statistical Model
Estimation. Chapman & Hall / CRC.
}

\author{
Andrew Robinson and Joe Hilbe.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{glm}}, \code{\link{irls}}, \code{\link{ml_glm}}, 
}

\examples{
data(medpar)
ml.nb2 <- ml_glm2(los ~ hmo + white,
                    formula2 = ~1,
                    data = medpar,
                    family = "negBinomial",
                    mean.link = "log",
                    scale.link = "inverse_s")

data(ufc)

ufc <- na.omit(ufc)
ml.g <- ml_glm2(height.m ~ dbh.cm,
                formula2 = ~ dbh.cm,
                data = ufc,
                family = "normal",
                mean.link = "identity",
                scale.link = "log_s")

summary(ml.g)

}

\keyword{ ~models }
