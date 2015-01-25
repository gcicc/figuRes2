\name{benrisk2.data}
\alias{benrisk2.data}
\docType{data}
\title{
benrisk2.data
}
\description{
A data set for producting forest plots}
\usage{data("benrisk2.data")}
\format{
  A data frame with 18 observations on the following 19 variables.
  \describe{
    \item{\code{Benefit_Risk}}{a factor with levels \code{Benefit} \code{Risk}}
    \item{\code{Category}}{a factor with levels \code{Grp1} \code{Grp2} \code{Grp3} \code{Grp4}}
    \item{\code{Subcategory}}{a factor with levels \code{Alanine aminotransferase increased} \code{Aspartate aminotransferase increased} \code{Chorioretinopathy} \code{Cutaneuous SCC (Including Keratoacanthoma)} \code{Dermatitis acneiform} \code{Ejection fraction decreased} \code{Hypertension} \code{PFS} \code{Response Rate}}
    \item{\code{Type}}{a factor with levels \code{Difference in RR} \code{Hazard Ratio} \code{Risk Difference}}
    \item{\code{Type_short}}{a factor with levels \code{HR} \code{RiskDiff} \code{RR Diff}}
    \item{\code{Treatment}}{a factor with levels \code{150/1 vs. Mono} \code{150/2 vs. Mono}}
    \item{\code{Big_N1}}{a numeric vector}
    \item{\code{Small_N1}}{a numeric vector}
    \item{\code{Big_N2}}{a numeric vector}
    \item{\code{Small_N2}}{a factor with levels \code{} \code{                           xcv} \code{11} \code{12} \code{13} \code{16} \code{20} \code{21} \code{5} \code{7}}
    \item{\code{Point_Est}}{a numeric vector}
    \item{\code{LCI}}{a numeric vector}
    \item{\code{UCI}}{a numeric vector}
    \item{\code{Scale}}{a factor with levels \code{linear} \code{log}}
    \item{\code{Reverse}}{a factor with levels \code{} \code{Y}}
    \item{\code{Anno1}}{a factor with levels \code{} \code{Test Annotation 1}}
    \item{\code{Anno1pos}}{a numeric vector}
    \item{\code{Anno2}}{a factor with levels \code{} \code{Test Annotation 2}}
    \item{\code{Anno2pos}}{a numeric vector}
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
data(benrisk2.data)
## maybe str(benrisk2.data) ; plot(benrisk2.data) ...
}
\keyword{datasets}
