\name{P__disp}
\alias{P__disp}

\title{A function to calculate Pearson Chi2 and its dispersion statistic following glm and glm.nb.}

\description{ 
This function calculates Pearson Chi2 statistic and the
Pearson-based dipersion statistic. Values of the dispersion greater
than 1 indicate model overdispersion. Values under 1 indicate
under-dispersion.
}

\usage{
P__disp(x)
}

\arguments{
  \item{x}{
    the fitted model.
}
}


\value{
\item{pearson.chi2 }{Pearson Chi2 value.}
\item{dispersion }{Pearson-basde dispersion.}
}


\details{
To be used following glm and glm.nb functions. 
}


\author{
Joseph Hilbe and Andrew Robinson
}

\references{
Hilbe, J.M., and Robinson, A.P. 2013. Methods of Statistical Model Estimation. Chapman & Hall / CRC. 
}

\seealso{
\code{\link{glm}}, \code{\link[MASS]{glm.nb}}
}

\examples{
data(medpar)
mymod <- glm(los ~ hmo + white + factor(type), 
             family = poisson, 
             data = medpar)
P__disp(mymod)
}
