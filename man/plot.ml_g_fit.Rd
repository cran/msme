\name{plot.ml_g_fit}
\alias{plot.ml_g_fit}

\title{A plot method for objects of class ml_g_fit.}

\description{
This function provides a four-way plot for fitted models.
}

\usage{
\method{plot}{ml_g_fit}(x, ...)
}

\arguments{
  \item{x}{
    the fitted model.
  }
  \item{\dots}{
    other arguments, retained for compatibility with generic method.
  }
}

\details{
The function plots a summary.  The output is structured to broadly
match the default options of the plot.lm function.
}

\value{
Run for its side effect of producing a plot object.
}

\references{
Hilbe, J.M., and Robinson, A.P. 2013. Methods of Statistical Model
Estimation. Chapman & Hall / CRC.
}

\author{
Andrew Robinson and Joe Hilbe.
}

\seealso{
\code{\link{ml_g}}
}

\examples{

data(ufc)
ufc <- na.omit(ufc)

ufc.g.reg <- ml_g(height.m ~ dbh.cm, data = ufc)

plot(ufc.g.reg)

}


\keyword{ models }
\keyword{ htest }
