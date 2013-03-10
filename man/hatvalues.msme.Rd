\name{hatvalues.msme}
\alias{hatvalues.msme}

\title{
Function to return the hat matrix of a msme-class model.
}

\description{
This function uses QR decomposition to determine the hat matrix of a
model given its design matrix X.  It is specific to objects of class msme.
}

\usage{
\method{hatvalues}{msme}(model, ...)
}

\arguments{
  \item{model}{
    A fitted model of class msme.
}
  \item{\dots}{
    other arguments, retained for compatibility with generic method.
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
An n*n matrix of hat values, where n is the number of observations used
to fit the model.  Needed to standardize the residuals.
}

\references{
Hilbe, J.M., and Robinson, A.P. 2013. Methods of Statistical Model
Estimation. Chapman & Hall / CRC.
}

\author{
Andrew Robinson and Joe Hilbe.
}

\note{
Leverages can be obtained as the diagonal of the output.  See the examples.
}

\seealso{
\code{\link{hatvalues}}
}

\examples{
data(medpar)

ml.poi <- ml_glm(los ~ hmo + white,
                 family = "poisson",
                 link = "log",
                 data = medpar)

str(diag(hatvalues(ml.poi)))

}

\keyword{ ~models }
