% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pictplot.R
\name{im_moments}
\alias{im_moments}
\title{Get moment statistics of an image}
\usage{
im_moments(
  im,
  channel = 1:3,
  order = 1:4,
  space = "CIELAB",
  max_size = 1024,
  na.rm = FALSE
)
}
\arguments{
\item{im}{an image}

\item{channel}{color channel}

\item{order}{order of the moment to be computed}

\item{space}{color space, either "CIELAB" (default) or "RGB"}

\item{max_size}{resize input image before calculation of moments}

\item{na.rm}{logical. Should missing values be removed?}
}
\value{
a data frame of moment values
}
\description{
Get moment statistics of an image
}
\examples{
im_moments(regatta) # moments in CIELAB color space
im_moments(regatta, space = "RGB") # moments of RGB channels
im_moments(regatta, channel = 1) # L channel of CIELAB color space
im_moments(regatta, channel = "L") # same as above
im_moments(regatta, channel = 1, space = "RGB") # R channel of the input image
im_moments(regatta, channel = 2:3, order = c(2, 3)) # sd and skew in a and b channels
im_moments(regatta, channel = c("a", "b"), order = c(2, 3)) # same as above
}
