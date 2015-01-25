\name{benrisk.data}
\alias{benrisk.data}
\docType{data}
\title{
Benefit risk data.set
}
\description{
A data set for building forest plots.
}
\usage{data("benrisk.data")}
\format{
  A data frame with 9 observations on the following 12 variables.
  \describe{
    \item{\code{class}}{a character vector}
    \item{\code{order}}{a numeric vector}
    \item{\code{Group}}{a factor with levels \code{Part C Randomized Phase}}
    \item{\code{Category}}{a factor with levels \code{Alanine aminotransferase increased} \code{Aspartate aminotransferase increased} \code{Chorioretinopathy} \code{Dermatitis acneiform} \code{Ejection fraction decreased} \code{Hypertension} \code{Response Rate} \code{PFS} \code{Cutaneuous SCC (Including Keratoacanthoma)}}
    \item{\code{Comparison}}{a factor with levels \code{150/2 vs. Mono}}
    \item{\code{Measure}}{a factor with levels \code{Risk Difference} \code{Difference in RR} \code{Hazard Ratio}}
    \item{\code{Point.Estimate}}{a numeric vector}
    \item{\code{LCL.95.CI}}{a numeric vector}
    \item{\code{UCL.95.CI}}{a numeric vector}
    \item{\code{Reverse.Point.Estimate}}{a numeric vector}
    \item{\code{Reverse.LCL.95.CI}}{a numeric vector}
    \item{\code{Reverse.UCL.95.CI}}{a numeric vector}
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
data(benrisk.data)
## maybe str(benrisk.data) ; plot(benrisk.data) ...
}
\keyword{datasets}
