\name{demog.data}
\alias{demog.data}
\docType{data}
\title{
demog.data
}
\description{
Demography data}
\usage{data("demog.data")}
\format{
  A data frame with 5000 observations on the following 15 variables.
  \describe{
    \item{\code{CENTREID}}{a numeric vector}
    \item{\code{SUBJID}}{a numeric vector}
    \item{\code{AGE}}{a numeric vector}
    \item{\code{SEX}}{a factor with levels \code{F} \code{M}}
    \item{\code{TRTGRP}}{a factor with levels \code{Placebo} \code{Treatment}}
    \item{\code{REGRAP}}{a factor with levels \code{Asia/Pacific} \code{Eastern Europe} \code{North America} \code{South America} \code{Western Europe}}
    \item{\code{HEIGHT}}{a numeric vector}
    \item{\code{WEIGHT}}{a numeric vector}
    \item{\code{DIABET}}{a numeric vector}
    \item{\code{SYSBP}}{a numeric vector}
    \item{\code{DIABP}}{a numeric vector}
    \item{\code{VSBMI}}{a numeric vector}
    \item{\code{VSBMIG}}{a factor with levels \code{<25 kg/m2} \code{>=30 kg/m2} \code{25-<30 kg/m2} \code{Missing}}
    \item{\code{GFRFG}}{a factor with levels \code{Mild} \code{Missing} \code{Moderate} \code{Normal} \code{Severe}}
    \item{\code{HDLC}}{a numeric vector}
  }
}
\details{
No details.
}

\examples{
data(demog.data)
## maybe str(demog.data) ; plot(demog.data) ...
}
\keyword{datasets}
