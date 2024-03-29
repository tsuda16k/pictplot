% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pictplot.R
\name{find_circle}
\alias{find_circle}
\title{Find the circle passing through three points}
\usage{
find_circle(x1, y1, x2, y2, x3, y3)
}
\arguments{
\item{x1}{x coordinate of P1}

\item{y1}{y coordinate of P1}

\item{x2}{x coordinate of P2}

\item{y2}{y coordinate of P2}

\item{x3}{x coordinate of P3}

\item{y3}{y coordinate of P3}
}
\value{
a vector of 3 values, the center (x and y) and the radius of the circle
}
\description{
Given 3 points, P1(x1, y1), P2(x2, y2) and P3(x3, y3), find the circle that passes through the points.
For details, see https://www.geeksforgeeks.org/equation-of-circle-when-three-points-on-the-circle-are-given/
}
\examples{
find_circle(0, 0, 2, 0, 0, 2)
}
