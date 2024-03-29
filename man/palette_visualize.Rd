% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pictplot.R
\name{palette_visualize}
\alias{palette_visualize}
\title{Visualize the color palette of an image}
\usage{
palette_visualize(
  im,
  palette,
  type = "bar",
  threshold = 1,
  monospace = FALSE,
  width,
  size,
  outline_width = 0,
  outline_color = c(0.5, 0.5, 0.5),
  margin = 0
)
}
\arguments{
\item{im}{an image}

\item{palette}{color palette}

\item{type}{either "bar" or "circle}

\item{threshold}{minor colors below this threshold is not displayed}

\item{monospace}{either TRUE or FALSE. If FALSE, If FALSE, the length of the color chart is
proportional to the percentage of occurrences of that color in the image.}

\item{width}{width of color chart. A numeric value between 0 and 1.}

\item{size}{output image resolution. Only works for the circle type. The default is 1024.}

\item{outline_width}{outline width of color chart. The default is 0 (no outline).}

\item{outline_color}{outline color.}

\item{margin}{margin between picture and color chart. Only works for the bar type.
A numeric value between 0 and 1. The default is 0.}
}
\description{
Visualize the color palette of an image
}
\examples{
palette = palette_extract(regatta)
im = palette_visualize(regatta, palette$df, "bar")
plot(im)
im2 = palette_visualize(regatta, palette$df, "circle")
plot(im2)
}
