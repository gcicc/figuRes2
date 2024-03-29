% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_functions.r
\name{box.plot}
\alias{box.plot}
\title{box.plot}
\usage{
box.plot(
  parent.df,
  y.col = "AGE",
  y.label = "AGE",
  category.col = "TRTGRP",
  category.label = "Treatment Group",
  y.limits = NULL,
  y.ticks = NULL,
  y.digits = 0,
  shape.palette = c(21, 22),
  category.palette = c(2, 3),
  text.size = 4
)
}
\arguments{
\item{parent.df}{data.frame used by ggplot}

\item{y.col}{parent.df column associated with response vairable}

\item{y.label}{value gets passed to labs}

\item{category.col}{data.frame column associated with categorical variable (bar.plot, box.plot, cdf.plot, dot.plot, km.plot)}

\item{category.label}{passed to x-axis label}

\item{y.limits}{passed to scale_y_continuous}

\item{y.ticks}{passed to scale_y_continuous}

\item{y.digits}{passed to scale_y_continuous label's, fmt (box.plot, line.plot)}

\item{shape.palette}{values passed to scale_shape_manual}

\item{category.palette}{colors assoicated with categorical variable}

\item{text.size}{value gets passed to geom_text}
}
\value{
A ggplot object is returned.
}
\description{
A function for creating harmonized ggplot2 boxplots
}
\examples{
{
data(demog.data)
# pre-processing

levels(demog.data$SEX) <- c("Female", "Male")

 p1 <- box.plot(parent.df = demog.data, 
     y.col = "BMI", 
     y.label = expression(paste("BMI (m/kg",phantom()^2,")")), 
     category.col = "SEX",
     category.label = "Gender", 
      y.limits = c(0, 70), 
      y.ticks = seq(0, 100, 10), 
      y.digits = 0,
      shape.palette = c(20, 20),
      category.palette = rainbow(6),
      text.size = 4)
 print(p1)
}
}
\author{
Greg Cicconetti
}
