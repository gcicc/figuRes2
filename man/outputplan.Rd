\name{outputplan}
\alias{outputplan}
\docType{data}
\title{
outputplan
}
\description{
outputplan
}
\usage{data("outputplan")}
\format{
  A data frame with 16 observations on the following 24 variables.
  \describe{
    \item{\code{Study}}{a factor with levels \code{StudyXYZ}}
    \item{\code{Section}}{a numeric vector}
    \item{\code{Number}}{a numeric vector}
    \item{\code{FigureNumber}}{a numeric vector}
    \item{\code{TableID}}{a factor with levels \code{} \code{Table 1.1} \code{Table 1.1 and 1.2} \code{Table 1.1COMMA 1.2COMMA and 1.3} \code{Table 1.3} \code{Table 1.4} \code{Table 1.5} \code{Table 1.6} \code{Table 1.7}}
    \item{\code{HarpTitle}}{a factor with levels \code{BMI by Visit and Treatment Group} \code{BMI Group by Visit} \code{Boxplot of Age} \code{Boxplot of Body Mass Index} \code{Boxplot of Diastolic Blood Pressure} \code{Boxplot of HDL Cholesterol} \code{Boxplot of Height} \code{Boxplot of Systolic Blood Pressure} \code{Boxplot of Weight} \code{Cumulative Distribution Function Plot of Weight} \code{Density Plot E0 (Normal Distribution) for Primary and Sensitivity Priors } \code{Lineplot Example} \code{Plot of Individual Values of Weight with 95 Confidence Intervals} \code{Plot of Individual Values of Weight with 95 Confidence Intervals by Region} \code{Scatterplot of Height vs Weight by Region with Smoothers} \code{Scatterplot of Height vs Weight by Region with Smoothers by Region}}
    \item{\code{PlotStyle}}{a factor with levels \code{} \code{BarChart} \code{Boxplot} \code{CDF} \code{Density} \code{Scatter}}
    \item{\code{ulh1}}{a factor with levels \code{Protocol: Comp1234}}
    \item{\code{ulh2}}{a factor with levels \code{Population: ITT}}
    \item{\code{ulh3}}{a logical vector}
    \item{\code{urh1}}{a factor with levels \code{Page 1 of 1}}
    \item{\code{urh2}}{a logical vector}
    \item{\code{urh3}}{a logical vector}
    \item{\code{fnote1}}{a factor with levels \code{} \code{In more than 4 lines of footnotes are neededCOMMA one can adapt the refresh.outputplan function.}}
    \item{\code{fnote2}}{a factor with levels \code{} \code{The refresh.outputplan function identifies the number of footnotes and adapts height for graphic region accordingly.}}
    \item{\code{fnote3}}{a factor with levels \code{} \code{2nd Footnote} \code{The refresh.outputplan function identifies the number of footnotes and adapts height for graphic region accordingly.}}
    \item{\code{fnote4}}{a factor with levels \code{} \code{Note: In this example four footnotes are needed.} \code{Note: In this example only one footnote line is needed.  The driver file expands the height of the graph.region accordingly.} \code{Note: In this example three footnotes are needed.} \code{Note: In this example two footnotes are needed.} \code{Only 1 footnote needed.}}
    \item{\code{outputfile}}{a factor with levels \code{boxplot_age.PDF} \code{boxplot_bmi.PDF} \code{boxplot_diabp.PDF} \code{boxplot_hdl.PDF} \code{boxplot_height.PDF} \code{boxplot_sysbp.PDF} \code{boxplot_weight.PDF} \code{category_by_visit.PDF} \code{cdf_weight.PDF} \code{continuous_by_visit_and_treatment.PDF} \code{jitter_weight.PDF} \code{jitter_weight_faceted.PDF} \code{lineplot_example.PDF} \code{priordens.PDF} \code{scatter_smooth.PDF} \code{scatter_smooth_facet.PDF}}
    \item{\code{rcode}}{a factor with levels \code{boxplot_age.r} \code{boxplot_bmi.r} \code{boxplot_diabp.r} \code{boxplot_hdl.r} \code{boxplot_height.r} \code{boxplot_sysbp.r} \code{boxplot_weight.r} \code{category_by_visit.r} \code{cdf_weight.r} \code{continuous_by_visit_and_treatment.r} \code{jitter_weight.r} \code{jitter_weight_faceted.r} \code{lineplot_example.r} \code{priordens.r} \code{scatter_smooth.r} \code{scatter_smooth_facet.r}}
    \item{\code{csv}}{a factor with levels \code{} \code{g_bslchar.csv} \code{lineplot_data.csv}}
    \item{\code{FigureTitle}}{a factor with levels \code{BMI by Visit and Treatment Group} \code{BMI Group by Visit} \code{Boxplot of Age - Title fits on one line - only pathCOMMA tableCOMMA time footnote} \code{Boxplot of Body Mass Index} \code{Boxplot of Diastolic Blood Pressure Superscript needed in title} \code{Boxplot of HDL Cholesterol} \code{Boxplot of Height Title requires two lines} \code{Boxplot of Systolic Blood Pressure Title requires a second and a third and a fourth line} \code{Boxplot of Weight Title requires a second and a third line} \code{Cumulative Distribution Function Plot of Weight} \code{Density Plot E0 [subscript needed]  Normal Distribution for Primary and Sensitivity Priors} \code{Lineplot Example} \code{Plot of Individual Values of Weight with 95 Confidence Intervals} \code{Scatterplot of Height vs Weight by Region with Smoothers}}
    \item{\code{Author}}{a factor with levels \code{Boxplot of Age} \code{GC}}
    \item{\code{Response}}{a factor with levels \code{} \code{AGE} \code{DIABP} \code{GC} \code{HDLC} \code{HEIGHT} \code{SYSBP} \code{VSBMI} \code{WEIGHT}}
    \item{\code{RunThis}}{a factor with levels \code{AGE} \code{Y}}
  }
}
\details{
No details.
}

\examples{
data(outputplan)
## maybe str(outputplan) ; plot(outputplan) ...
}
\keyword{datasets}
