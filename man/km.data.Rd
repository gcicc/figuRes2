\name{km.data}
\alias{km.data}
\docType{data}
\title{
km.data
}
\description{
A data set for creating Kaplan-Meier curves.
}
\usage{data("km.data")}
\format{
  A data frame with 15828 observations on the following 9 variables.
  \describe{
    \item{\code{CENTREID}}{a numeric vector}
    \item{\code{SUBJID}}{a numeric vector}
    \item{\code{AGE}}{a numeric vector}
    \item{\code{SEX}}{a factor with levels \code{F} \code{M}}
    \item{\code{TRTGRP}}{a factor with levels \code{Placebo} \code{Treatment}}
    \item{\code{COUNTRY}}{a factor with levels \code{Argentina} \code{Australia} \code{Belgium} \code{Brazil} \code{Bulgaria} \code{Canada} \code{Chile} \code{China} \code{Czech Republic} \code{Denmark} \code{Estonia} \code{France} \code{Germany} \code{Greece} \code{Hong Kong} \code{Hungary} \code{India} \code{Italy} \code{Japan} \code{Korea, Republic of} \code{Mexico} \code{Netherlands} \code{New Zealand} \code{Norway} \code{Pakistan} \code{Peru} \code{Philippines} \code{Poland} \code{Romania} \code{Russian Federation} \code{Slovakia} \code{South Africa} \code{Spain} \code{Sweden} \code{Taiwan} \code{Thailand} \code{Ukraine} \code{United Kingdom} \code{United States}}
    \item{\code{REGRAP}}{a factor with levels \code{Asia/Pacific} \code{Eastern Europe} \code{North America} \code{South America} \code{Western Europe}}
    \item{\code{censor}}{a numeric vector}
    \item{\code{centime}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(km.data)
## maybe str(km.data) ; plot(km.data) ...
}
\keyword{datasets}
