\name{raw.lineplot.data}
\alias{raw.lineplot.data}
\docType{data}
\title{
raw.lineplot.data}
\description{
A data set for creating line plots.
}
\usage{data("raw.lineplot.data")}
\format{
  A data frame with 62602 observations on the following 10 variables.
  \describe{
    \item{\code{CENTREID}}{a numeric vector}
    \item{\code{SUBJID}}{a numeric vector}
    \item{\code{TRTGRP}}{a factor with levels \code{Placebo} \code{Treatment}}
    \item{\code{VISITNUM}}{a numeric vector}
    \item{\code{VISIT}}{a factor with levels \code{BASELINE} \code{BASELINE (RAW)} \code{END-OF-TREATMENT (Derived)} \code{MONTH 1} \code{MONTH 18} \code{MONTH 3} \code{MONTH 6} \code{SCREENING} \code{TRT END/EWD IP (Combined - per protocol)} \code{TRT END/EWD IP (Combined)}}
    \item{\code{PTM}}{a factor with levels \code{} \code{TROUGH - MAIN STUDY} \code{TROUGH - SERIAL SUBSTUDY} \code{TROUGH - TROUGH SUBSTUDY}}
    \item{\code{PTMNUM}}{a numeric vector}
    \item{\code{LBTEST}}{a factor with levels \code{Lab Test XYZ}}
    \item{\code{LBSTRES2}}{a numeric vector}
    \item{\code{LBSTUNIT}}{a factor with levels \code{MCMOL/MIN/L}}
  }
}
\details{
No details.
}

\examples{
data(raw.lineplot.data)
## maybe str(raw.lineplot.data) ; plot(raw.lineplot.data) ...
}
\keyword{datasets}
