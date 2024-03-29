% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pictplot.R
\name{DBC}
\alias{DBC}
\title{Calculate the fractal dimension of an image using 3D box counting methods}
\usage{
DBC(im, method = "DBC", n = 20, rawdata = FALSE)
}
\arguments{
\item{im}{an image}

\item{method}{either "DBC", "RDBC", or "IDBC"}

\item{n}{number of samples}

\item{rawdata}{if TRUE, raw data is returned as well as slope value}
}
\value{
a numeric value or a data frame
}
\description{
Calculate the fractal dimension of an image using 3D box counting methods
}
\examples{
DBC(regatta)
}
