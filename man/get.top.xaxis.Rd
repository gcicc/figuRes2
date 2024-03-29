% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_functions.r
\name{get.top.xaxis}
\alias{get.top.xaxis}
\title{get.top.xaxis}
\usage{
get.top.xaxis(bottom.axis.version, top.axis.version)
}
\arguments{
\item{bottom.axis.version}{ggplot object with bottom x-axis (get.top.xaxis)}

\item{top.axis.version}{ggplot object with intended top x-axis in bottom position (get.top.xaxis)}
}
\description{
This takes two ggplot objects, steals the bottom x-axis from 2nd object and returns a gtable object with that bottom x-axis per object 1 and top x-axis per object 2
}
\author{
Greg Cicconetti
}
