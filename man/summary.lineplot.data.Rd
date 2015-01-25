\name{summary.lineplot.data}
\alias{summary.lineplot.data}
\docType{data}
\title{
summary.lineplot.data
}
\description{
A data set for creating line plots.
}
\usage{data("summary.lineplot.data")}
\format{
  A data frame with 238 observations on the following 14 variables.
  \describe{
    \item{\code{TRTGRP}}{a factor with levels \code{Placebo} \code{Treatment}}
    \item{\code{VISIT}}{a factor with levels \code{BASELINE (RAW)} \code{BASELINE [1]} \code{END-OF-STUDY (Derived) [2]} \code{END-OF-TREATMENT (Derived) [2]} \code{FOLLOW-UP} \code{MONTH 1} \code{MONTH 12} \code{MONTH 18} \code{MONTH 24} \code{MONTH 3} \code{MONTH 30} \code{MONTH 36} \code{MONTH 42} \code{MONTH 48} \code{MONTH 6} \code{SCREENING} \code{TRT END/EWD IP (Combined)}}
    \item{\code{lbtest1}}{a factor with levels \code{Alanine Amino Transferase (IU/L)} \code{Albumin (G/L)} \code{Alkaline Phosphatase (IU/L)} \code{Aspartate Amino Transferase (IU/L)} \code{Total Bilirubin (MG/DL)} \code{Total Bilirubin (UMOL/L)} \code{Total Protein (G/L)}}
    \item{\code{TRTCD}}{a numeric vector}
    \item{\code{VISITNUM}}{a numeric vector}
    \item{\code{bign}}{a numeric vector}
    \item{\code{P25}}{a numeric vector}
    \item{\code{P75}}{a numeric vector}
    \item{\code{max}}{a numeric vector}
    \item{\code{mean}}{a numeric vector}
    \item{\code{median}}{a numeric vector}
    \item{\code{min}}{a numeric vector}
    \item{\code{n}}{a numeric vector}
    \item{\code{std}}{a numeric vector}
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
data(summary.lineplot.data)
## maybe str(summary.lineplot.data) ; plot(summary.lineplot.data) ...
}
\keyword{datasets}
