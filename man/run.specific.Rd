% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_functions.r
\name{run.specific}
\alias{run.specific}
\title{run.specific}
\usage{
run.specific(
  source.code = "g_AErr2.r",
  outfile = "",
  toPDF = FALSE,
  toWMF = FALSE,
  toJPEG = FALSE,
  toPNG = FALSE,
  toBMP = FALSE,
  toEPS = FALSE,
  dpires = 600,
  use.log = FALSE
)
}
\arguments{
\item{source.code}{This is intended to be a darapladib graphics driver file returning a graphic possibly with complete headers and footers.}

\item{outfile}{If (toPDF== TRUE & outfile == "") a .pdf file with root name taken from outputplan$outfile[which(outputplan$rcode ==source.code)]. Otherwise a .pdf will be created the value of outfile. The pdf is stored in mypath/od defined in setpaths.r.}

\item{toPDF}{Logical. If TRUE a .pdf file will be created. If FALSE graphic is sent to screen. (run.specific)}

\item{toWMF}{Logical. If TRUE a .wmf file will be created. (run.specific)}

\item{toJPEG}{Logical. If TRUE a .jpeg file will be created. (run.specific)}

\item{toPNG}{Logical. If TRUE a .png file will be created. (run.specific)}

\item{toBMP}{Logical. If TRUE a .bmp file will be created. (run.specific)}

\item{toEPS}{Logical. If TRUE a .eps file will be created. (run.specific)}

\item{dpires}{passed to devices}

\item{use.log}{logical to write a log file}
}
\description{
This function sources a .r driver file and sends its product to a newly opened 8.5in x 11in screen or a pdf file with 8.5in x 11in dimensions.
}
\author{
David wade
}
