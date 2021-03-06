% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geomhurricane.R
\docType{data}
\name{GeomHurricane}
\alias{GeomHurricane}
\title{Subclassing the Geom class to create the mapping for the hurricane.}
\format{An object of class \code{GeomHurricane} (inherits from \code{Geom}, \code{ggproto}) of length 5.}
\usage{
GeomHurricane
}
\arguments{
\item{required_aes}{Required aesthetics for the plot.}

\item{default_aes}{Default aesthetics}

\item{draw_key}{Function to draw the legend.}

\item{draw_group}{custom callback that processes one row of data per function call.}
}
\value{
An instamce of GeomHurricane for drawing radius based plots.
}
\description{
Subclassing the Geom class to create the mapping for the hurricane.
}
\keyword{datasets}
