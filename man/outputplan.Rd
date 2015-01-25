\name{outputplan}
\alias{outputplan}
\docType{data}
\title{
outputplan}
\description{
A dataset to assist with mass production of figures.}
\usage{data("outputplan")}
\format{
  A data frame with 7 observations on the following 24 variables.
  \describe{
    \item{\code{Study}}{a factor with levels \code{} \code{StudyXYZ}}
    \item{\code{Section}}{a numeric vector}
    \item{\code{Number}}{a numeric vector}
    \item{\code{FigureNumber}}{a numeric vector}
    \item{\code{TableID}}{a factor with levels \code{} \code{Table 1.1} \code{Table 1.2} \code{Table 1.3} \code{Table 1.4} \code{Table 1.5} \code{Table 1.6} \code{Table 1.7}}
    \item{\code{HarpTitle}}{a factor with levels \code{} \code{Boxplot of Age} \code{Boxplot of Body Mass Index} \code{Boxplot of Diastolic Blood Pressure} \code{Boxplot of HDL Cholesterol} \code{Boxplot of Height} \code{Boxplot of Systolic Blood Pressure} \code{Boxplot of Weight} \code{Density Curves}}
    \item{\code{PlotStyle}}{a factor with levels \code{} \code{Boxplot}}
    \item{\code{ulh1}}{a factor with levels \code{} \code{Protocol: Comp1234}}
    \item{\code{ulh2}}{a factor with levels \code{} \code{Population: ITT}}
    \item{\code{ulh3}}{a logical vector}
    \item{\code{urh1}}{a factor with levels \code{} \code{Page 1 of 1}}
    \item{\code{urh2}}{a logical vector}
    \item{\code{urh3}}{a logical vector}
    \item{\code{fnote1}}{a factor with levels \code{} \code{4th footnote}}
    \item{\code{fnote2}}{a factor with levels \code{} \code{2nd Footnote} \code{3rd footnote}}
    \item{\code{fnote3}}{a factor with levels \code{} \code{2nd Footnote}}
    \item{\code{fnote4}}{a factor with levels \code{} \code{1st footnote} \code{Only 1 footnote needed.}}
    \item{\code{FigureStatus}}{a logical vector}
    \item{\code{outputfile}}{a factor with levels \code{} \code{boxplot_age.PDF} \code{boxplot_bmi.PDF} \code{boxplot_diabp.PDF} \code{boxplot_hdl.PDF} \code{boxplot_height.PDF} \code{boxplot_sysbp.PDF} \code{boxplot_weight.PDF}}
    \item{\code{rcode}}{a factor with levels \code{} \code{boxplot_age.r} \code{boxplot_bmi.r} \code{boxplot_diabp.r} \code{boxplot_hdl.r} \code{boxplot_height.r} \code{boxplot_sysbp.r} \code{boxplot_weight.r}}
    \item{\code{csv}}{a factor with levels \code{} \code{g_bslchar.csv} \code{g_CDFipexp} \code{g_kmfmace} \code{g_kminvres} \code{g_lineplot_lppla2} \code{g_LP_bilt_stats} \code{g_subgrp_intg_page_all}}
    \item{\code{FigureTitle}}{a factor with levels \code{} \code{Boxplot of Age} \code{Boxplot of Body Mass Index} \code{Boxplot of Diastolic Blood Pressure} \code{Boxplot of HDL Cholesterol} \code{Boxplot of Height} \code{Boxplot of Systolic Blood Pressure} \code{Boxplot of Weight}}
    \item{\code{Author}}{a factor with levels \code{} \code{GC}}
    \item{\code{Response}}{a factor with levels \code{} \code{AGE} \code{DIABP} \code{HDLC} \code{HEIGHT} \code{SYSBP} \code{VSBMI} \code{WEIGHT}}
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
data(outputplan)
## maybe str(outputplan) ; plot(outputplan) ...
}
\keyword{datasets}
