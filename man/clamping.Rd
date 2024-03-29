% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pictplot.R
\name{clamping}
\alias{clamping}
\title{Clamp values to a minimum and maximum value}
\usage{
clamping(x, min = 0, max = 1)
}
\arguments{
\item{x}{a numeric vector}

\item{min}{minimum value}

\item{max}{maximum value}
}
\value{
a numeric vector
}
\description{
Clamp values to a minimum and maximum value
}
\examples{
clamping( -5:5, min = -3, max = 3 )
}
