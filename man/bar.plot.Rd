% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_functions.r
\name{bar.plot}
\alias{bar.plot}
\title{bar.plot}
\usage{
bar.plot(
  parent.df,
  category.col = "TRTGRP",
  category.label = "Treatment Group",
  x.label = "",
  y.col = "GWHRT",
  y.label = "Percentage of Subjects",
  y.limits = c(0, 0.7),
  y.ticks = seq(0, 0.3, 0.05),
  bar.position = "dodge",
  category.palette = c("red", "blue"),
  text.size = 3,
  text.buffer = 0.05,
  killMissing = TRUE
)
}
\arguments{
\item{parent.df}{data.frame used by ggplot}

\item{category.col}{data.frame column associated with categorical variable (bar.plot, box.plot, cdf.plot, dot.plot, km.plot)}

\item{category.label}{passed to x-axis label}

\item{x.label}{value gets passed to labs}

\item{y.col}{parent.df column associated with response vairable}

\item{y.label}{value gets passed to labs}

\item{y.limits}{passed to scale_y_continuous}

\item{y.ticks}{passed to scale_y_continuous}

\item{bar.position}{passed to geom_bar (bar.plot)}

\item{category.palette}{colors assoicated with categorical variable}

\item{text.size}{value gets passed to geom_text}

\item{text.buffer}{used by bar.plot to control text placement}

\item{killMissing}{logical used by bar.plot}
}
\value{
A ggplot object is returned.
}
\description{
A function for creating harmonized ggplot2 bar charts
}
\examples{
{
# Access dummy demography dataset
data(demog.data)
levels(demog.data$SEX) <- c("Female", "Male")

# A ggplot object is returned
p1 <- bar.plot(parent.df = demog.data, y.col = "SEX", 
x.label= "Gender", y.label = "Percentage of Subjects", 
category.col = "REGION", category.label = "Region", 
y.limits = c(0, 0.35), y.ticks = seq(0, 0.5, 0.05), 
bar.position= "dodge", 
category.palette = RColorBrewer::brewer.pal(n=5, name = "Dark2"),
text.size =4, text.buffer=.025, killMissing = TRUE) 
print(p1)
} 
}
\author{
Greg Cicconetti
}
