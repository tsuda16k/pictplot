
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pictplot

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/pictplot)](https://cran.r-project.org/package=pictplot)
[![CRAN\_time\_from\_release](https://www.r-pkg.org/badges/ago/pictplot)](https://cran.r-project.org/package=pictplot)
[![CRAN\_latest\_release\_date](https://www.r-pkg.org/badges/last-release/pictplot)](https://cran.r-project.org/package=pictplot)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/grand-total/pictplot)](https://cran.r-project.org/package=pictplot)
[![license](https://img.shields.io/github/license/mashape/apistatus.svg)](https://choosealicense.com/licenses/mit/)
<!-- badges: end -->

This is an R package that contains a lot of basic image processing
functions.

## Dependencies

Mac users need to install XQuartz (<https://www.xquartz.org/>).

## Installation

This package is currently not available on CRAN. The development version
of the package can be installed vie GitHub.

### Installation via GitHub

You can install the development version of the `pictplot` package via
GitHub, by using the `devtools` package.

``` r
# install the devtools package
install.packages("devtools")
```

**NOTE:**  
To install a package from GitHub,  
- On Windows,
<a href="https://cran.r-project.org/bin/windows/Rtools/">Rtools</a>
needs to be installed.  
- On Mac, XCode may be needed to be installed.

After you have installed the required software stated above, install the
package as follows:

``` r
# install the pictplot package
devtools::install_github("tsuda16k/pictplot")
```

Then, load the package.

``` r
library(pictplot)
```

## Example image

The `pictplot` package includes an image data, which is useful when you
want to see how a function works right away. The variable name of the
image is `regatta`.

Internally, this is just a numeric array of size 450 x 600 x 3
\[y-coordinate, x-coordinate, color channel\], meaning that it is 450
pix height and 600 pix width, and has three color channels (Red, Green,
and Blue). Each element of the array represents a pixel value, which can
range between 0 and 1.

``` r
dim(regatta)
#> [1] 450 600   3
```

Image size information can be shown by typing the variable name.

``` r
regatta
#> image: 450 [height] x 600 [width] x 3 [colour channels]
```

To plot an image, use the `plot()` function.

``` r
plot(regatta)
```

<img src="notes/regatta.png" width="60%">

## Load an image

To load an image you like, use the `im_load()` function.

``` r
im = im_load("path/to/your/image.jpg")
```

The jpg, png, and bmp formats are supported.

You can load an image from the web (URL). For example,

``` r
im = im_load("https://raw.githubusercontent.com/tsuda16k/pictplot/master/notes/face.png")
plot(im)
```

will load an image of a face.

<img src="notes/face.png" width="35%">

## Functionality

WIP
