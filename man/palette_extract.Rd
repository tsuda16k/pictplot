% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pictplot.R
\name{palette_extract}
\alias{palette_extract}
\title{Extract the color palette of an image
This function performs color palette extraction of an image using the mean shift algorithm.
It uses the meanShift function in the meanShiftR package.}
\usage{
palette_extract(im, bandwidth = 0.03, bound = 40, iterations = 1000)
}
\arguments{
\item{im}{an image}

\item{bandwidth}{This value is used in the kernel density estimate for color classification.
The smaller the value of this parameter, the more colors are extracted.}

\item{bound}{Input image is resized before color classification.
The input image will be reduced in size, with the shorter side equal to this value.}

\item{iterations}{The number of iterations to perform mean shift classification.}
}
\description{
Extract the color palette of an image
This function performs color palette extraction of an image using the mean shift algorithm.
It uses the meanShift function in the meanShiftR package.
}
\examples{
palette = palette_extract(regatta)
palette$df
plot(palette$im)
}
