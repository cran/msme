\name{ml_g}
\alias{ml_g}

\title{
Function to fit linear regression using maximum likelihood.
}

\description{
This function demonstrates the use of maximum likelihood to fit ordinary
least-squares regression models, by maximizing the likelihood as a
function of the parameters.  
}

\usage{
ml_g(formula, data)
}

\arguments{
  \item{formula}{
    an object of class '"formula"' (or one that can be coerced to
    that class): a symbolic description of the model to be
    fitted. (See the help for 'lm' for more details). 
}
  \item{data}{
    a data frame containing the variables in the model. 
}
}

\details{
  This function has limited functionality compared with
  R's internal lm function, which should be preferred in general.  
}

\value{
  \item{fit }{the output of optim.}
  \item{X }{the design matrix.}
  \item{y }{the response variable.}
  \item{call }{the call used for the function.}
  \item{beta.hat }{the parameter estimates.}
  \item{se.beta.hat }{estimated standard errors of the parameter estimates.}
  \item{sigma.hat }{the estimated conditional standard deviation of the
  response variable.}
}

\references{
Hilbe, J.M., and Robinson, A.P. 2013. Methods of Statistical Model
Estimation. Chapman \& Hall / CRC.
}

\author{
Andrew Robinson and Joe Hilbe.
}

\note{
  We use least squares to get initial estimates, which is a
  pretty barbaric hack.  But the purpose of this function is as a
  starting point, not to replace existing functions.
}

\seealso{
\code{\link{lm}}
}

\examples{

data(ufc)
ufc <- na.omit(ufc)

ufc.g.reg <- ml_g(height.m ~ dbh.cm, data = ufc)

summary(ufc.g.reg)

}

\keyword{ ~models }
