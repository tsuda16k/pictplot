
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
