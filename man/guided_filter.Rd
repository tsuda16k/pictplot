% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pictplot.R
\name{guided_filter}
\alias{guided_filter}
\title{Apply the guided filter}
\usage{
guided_filter(p, radius, epsilon = 0.1, I = p)
}
\arguments{
\item{p}{an image}

\item{radius}{filter radius}

\item{epsilon}{epsilon parameter}

\item{I}{guide image}
}
\value{
an image
}
\description{
Apply the guided filter
}
\examples{
plot(guided_filter(regatta,8))
}
