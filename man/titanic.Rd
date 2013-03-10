\name{titanic}
\alias{titanic}
\docType{data}
\title{
Titanic passenger survival data
}
\description{
%%  ~~ description of the dataset. ~~
Passenger survival data from 1912 Titanic shipping accident. 
}
\usage{data(titanic)}
\format{
  A data frame with 1316 observations on the following 4 variables.
  \describe{
    \item{\code{survived}}{1=survived; 0=died}
    \item{\code{age}}{1=adult; 0=child}
    \item{\code{sex}}{1=Male; 0=female}
    \item{\code{class}}{ticket class 1= 1st class; 2= second class; 3= third class}
   }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
Titanic is saved as a data frame.
Used to assess risk ratio; not stardard count model; good binary response model.  
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
Found in many other texts
}
\references{
%%  ~~ found in ~~
Hilbe, Joseph M (2007, 2011), Negative Binomial Regression, Cambridge University Press
Hilbe, Joseph M (2009), Logistic Regression Models, Chapman & Hall/CRC
}
\examples{
data(titanic)

glm.lr <- glm(survived ~ age + sex + factor(class),
             family=binomial, data=titanic)
summary(glm.lr)
exp(coef(glm.lr))

glm.irls <- irls(survived ~ age + sex + factor(class),
                 family = "binomial",
                 link = "cloglog",
                 data = titanic)
summary(glm.irls)
exp(coef(glm.irls))

glm.ml <- ml_glm(survived ~ age + sex + factor(class),
                 family = "bernoulli",
                 link = "cloglog1",
                 data = titanic)
summary(glm.ml)
exp(coef(glm.ml))

}
\keyword{datasets}
