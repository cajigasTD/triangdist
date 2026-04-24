
# triangdist

This package implements the triangular distribution in R.

## Installation

You can install the development version of triangdist from [GitHub](https://github.com/) with:

``` r
install.packages("remotes")
remotes::install_github("cajigasTD/triangdist")
```

## Example

``` r
library(triangdist)

dtriang(0.5, 0, 1, 0.3)
ptriang(0.5, 0, 1, 0.3)
qtriang(0.5, 0, 1, 0.3)
rtriang(5, 0, 1, 0.3)
```

