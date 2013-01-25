\name{doll}
\alias{doll}
\docType{data}
\title{ Physician smoking and mortality count data }
\description{
The data are a record of physician smoking habits and the frequency of
death by myocardial infarction, or heart attack.
}
\usage{data(doll)}
\format{
  A data frame with 10 observations on the following variables.
  \describe{
    \item{age}{Ordinal age group}
    \item{smokes}{smoking status}
    \item{deaths}{count of deaths in category}
    \item{pyears}{number of physisian years in scope of data}
    \item{a1}{Dummy variable for age level 1}
    \item{a2}{Dummy variable for age level 2}
    \item{a3}{Dummy variable for age level 3}
    \item{a4}{Dummy variable for age level 4}
    \item{a5}{Dummy variable for age level 5}
  }
}
\details{
The physicians were divided into five age divisions, with deaths as the
response, person years (pyears) as the binomial denominator, and both
smoking behavior (smokes) and agegroup (a1--a5) as predictors. 
}
\source{
Doll, R and A.B.Hill (1966). Mortality of British doctors in relation to
smoking; observations on coronary thrombosis. In Epidemiological
Approaches to the Study of Cancer and Other Chronic Diseases,
W. Haenszel (ed), 19: 204--268. National Cancer Institute Monograph.
}
\references{
HIlbe, J., and A.P. Robinson. 2012.  Methods of Statistical Model
Estimation.  Chapman & Hall / CRC.
}
\examples{
data(doll)
}

\keyword{datasets}
