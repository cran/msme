\name{heart}

\alias{heart}

\docType{data}

\title{ Heart surgery outcomes for Canadian patients }

\description{ The data consists of Canadian patients who have
either a Coronary Artery Bypass Graft surgery (CABG) or Percutaneous
Transluminal Coronary Angioplasty (PTCA) heart procedure.  }

\usage{data(heart)}

\format{
  A grouped binomial data frame with 15 observations.
  \describe{
    \item{death}{number of patients that died within 48 hours of
  hospital admission}
    \item{cases}{number of patients monitored}
    \item{anterior}{1: anterior site damage heart attack; 0: other site damage}
    \item{hcabg}{1: previous CABG procedure; 0: previous PTCA procedure;}
    \item{killip}{1: normal heart; 2: angina; 3: minor heart blockage;
  4: heart attack or myocardial infarction;}
  }
}

\details{
The data are presented as a grouped binomial dataset, with each row
representing a different combination of the predictor variables.
}
\source{
National Canadian Registry of Cardiovascular Disease
}

\references{
Hilbe, Joseph M (2009), Logistic Regression Models, Chapman & Hall/CRC
first used in Hardin, JW and JM Hilbe (2001, 2007), Generalized Linear Models and Extensions, Stata Press
}

\examples{
data(heart)
}

\keyword{datasets}
