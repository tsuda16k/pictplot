% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pictplot.R
\name{gabor_kernel}
\alias{gabor_kernel}
\title{Create a gabor filter kernel}
\usage{
gabor_kernel(
  ksize = sigma * 6,
  sigma = min(ksize)/6,
  lambd = min(ksize)/4,
  theta = 0,
  psi = 0,
  gamma = 1,
  normalize = TRUE,
  mask = FALSE
)
}
\arguments{
\item{ksize}{the size of the gabor kernel. should be odd number (if not, incremented by one).}

\item{sigma}{the standard deviation of the Gaussian function}

\item{lambd}{the wavelength of the sinusoidal factor}

\item{theta}{the orientation of the normal to the parallel stripes of the Gabor function}

\item{psi}{the phase offset}

\item{gamma}{the spatial aspect ratio}

\item{normalize}{if TRUE (default), kernel is normalized (the zero-summing normalization)}

\item{mask}{if TRUE, circular mask is applied.}
}
\description{
Create a gabor filter kernel
}
\examples{
gb = gabor_kernel( ksize = 61 )
plot(gb, rescale = TRUE)
gb = gabor_kernel( ksize = 61, theta = pi/6 )
plot(gb, rescale = TRUE)
}
