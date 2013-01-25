\name{summary.ml_g_fit}
\alias{summary.ml_g_fit}

\title{
A summary method for objects of class ml_g_fit.
}

\description{
This function provides a compact summary for fitted models.
}

\usage{
\method{summary}{ml_g_fit}(object, dig = 3, ...)
}

\arguments{
  \item{object}{
    the fitted model.
}
  \item{dig}{
    an optional integer detailing the number of significant digits for
  printing. 
}
  \item{\dots}{
    other arguments, retained for compatibility with generic method.
}
}

\details{
The function prints out a summary and returns an invisible list with
useful objects.  The output is structured to match the print.summary.lm
function.
}

\value{
\item{call }{the call used to fit the model.}
\item{coefficients }{a dataframe of estimates, standard errors, etc.}
\item{residuals }{deviance residuals from the model.}
\item{aliased }{included to match the print.summary.lm function.  Lazily
  set to FALSE for all parameters.}
\item{sigma }{the estimate of the conditional standard deviation of the
  response variable.}
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
\code{\link{ml_g}}
}

\examples{

data(ufc)
ufc <- na.omit(ufc)

ufc.g.reg <- ml_g(height.m ~ dbh.cm, data = ufc)

summary(ufc.g.reg)

}

\keyword{ ~models }
