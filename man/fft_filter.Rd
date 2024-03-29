% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pictplot.R
\name{fft_filter}
\alias{fft_filter}
\title{Apply fft filter}
\usage{
fft_filter(im, xpass, kernel = "ideal", cutoff, cutoff2, order = 4)
}
\arguments{
\item{im}{an image}

\item{xpass}{either "lowpass", "highpass", or "bandpass"}

\item{kernel}{either "ideal", "gauss", "butterworth", or "dc"}

\item{cutoff}{cutoff frequency}

\item{cutoff2}{second cutoff frequency}

\item{order}{order of butterworth filter}
}
\value{
an image
}
\description{
Apply fft filter
}
\examples{
plot(fft_filter(regatta, "lowpass", "ideal", 10)) # ideal lowpass filterig
plot(fft_filter(regatta, "lowpass", "gauss", 10)) # gaussian lowpass filterig
# 5th-order butterworth lowpass filtering
plot(fft_filter(regatta, "lowpass", "butterworth", 10, order = 5))
im = im_gray(regatta)
plot(fft_filter(im, "highpass", "gauss", 60), rescale = TRUE) # gaussian highpass filterig
plot(fft_filter(im, "bandpass", "gauss", 10, 20), rescale = TRUE) # gaussian bandpass filterig
}
