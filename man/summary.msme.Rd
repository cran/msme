\name{summary.msme}
\alias{summary.msme}

\title{
A summary method for objects of class msme.
}

\description{
This function provides a compact summary for fitted models.
}

\usage{
\method{summary}{msme}(object, ...)
}

\arguments{
  \item{object}{
    the fitted model.
}
  \item{\dots}{
    optional arguments to be passed through.
}
}

\details{
The function prints out a summary and returns an invisible list with
useful objects.  
}

\value{
\item{call }{the call used to fit the model.}
\item{coefficients }{a dataframe of estimates, standard errors, etc.}
\item{deviance }{deviance from the model fit.}
\item{null.deviance }{deviance from the null model fit.}
\item{df.residual }{residual degrees of freedom from the model fit.}
\item{df.null }{residual degrees of freedom from the null model fit.}
}

\references{
Hilbe, J.M., and Robinson, A.P. 2013. Methods of Statistical Model
Estimation. Chapman & Hall / CRC.
}

\author{
Andrew Robinson and Joe Hilbe.
}

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{

data(medpar)

ml.poi <- ml_glm(los ~ hmo + white,
                 family = "poisson",
                 link = "log",
                 data = medpar)

summary(ml.poi)


}

\keyword{ ~models }
