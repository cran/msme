\name{rwm5yr}
\alias{rwm5yr}
\docType{data}
\title{
German health registry for the years 1984-1988
}
\description{
%%  ~~ Description of the dataset. ~~
German health registry for the years 1984-1988. Health 
information for years immediately prior to health reform. 
}
\usage{data(rwm5yr)}
\format{
  A data frame with 19,609 observations on the following 17 variables.
  \describe{
   \item{\code{id}}{patient ID  (1=7028)}
   \item{\code{docvis}}{number of visits to doctor during year (0-121)}
   \item{\code{hospvis}}{number of days in hospital during year (0-51)}
   \item{\code{year}}{year; (categorical: 1984, 1985, 1986, 1987, 1988)}
   \item{\code{edlevel}}{educational level (categorical: 1-4)}
   \item{\code{age}}{age: 25-64}
   \item{\code{outwork}}{out of work=1; 0=working}
   \item{\code{female}}{female=1; 0=male}
   \item{\code{married}}{married=1; 0=not married}
   \item{\code{kids}}{have children=1; no children=0}
   \item{\code{hhninc}}{household yearly income in marks (in Marks)}
   \item{\code{educ}}{years of formal education (7-18)}
   \item{\code{self}}{self-employed=1; not self employed=0}
   \item{\code{edlevel1}}{(1/0) not high school graduate}
   \item{\code{edlevel2}}{(1/0) high school graduate}
   \item{\code{edlevel3}}{(1/0) university/college} 
   \item{\code{edlevel4}}{(1/0) graduate school}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
rwm5yr is saved as a data frame.
Count models typically use docvis as response variable. 0 counts are included
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
German Health Reform Registry, years pre-reform 1984-1988, 
}
\references{
%%  ~~ found in following texts ~~
Hilbe, Joseph M (2007, 2011), Negative Binomial Regression, Cambridge University Press
}
\examples{
data(rwm5yr)
glmrp <- glm(docvis ~ outwork + female + age + factor(edlevel),
             family = poisson, data = rwm5yr)
summary(glmrp)
exp(coef(glmrp))
library(MASS)
glmrnb <- glm.nb(docvis ~ outwork + female + age + factor(edlevel),
                 data = rwm5yr)
summary(glmrnb)
exp(coef(glmrnb))
\dontrun{
library(gee)
mygee <- gee(docvis ~ outwork + age + factor(edlevel), id=id, 
  corstr = "exchangeable", family=poisson, data=rwm5yr)
summary(mygee)
exp(coef(mygee))
}
}
\keyword{datasets}
