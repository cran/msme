\name{residuals.msme}
\alias{residuals.msme}

\title{
Function to produce residuals from a model of class msme.
}

\description{
  Function to produce deviance and standardized deviance residuals
  from a model of class msme.
}

\usage{
\method{residuals}{msme}(object, type = c("deviance", "standard"), ...)
}

\arguments{
  \item{object}{
a model of class msme.
}
  \item{type}{
the type of residual requested.  Defaults to deviance.
}
  \item{\dots}{
arguments to pass on.  Retained for compatibility with generic method.
}
}

\details{
Presently only deviance or standardized deviance residuals are computed.
}

\value{
A vector of residuals.
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

%\seealso{
%}

\examples{
data(medpar)

ml.poi <- ml_glm(los ~ hmo + white,
                 family = "poisson",
                 link = "log",
                 data = medpar)

str(residuals(ml.poi))

}

\keyword{ ~models }
