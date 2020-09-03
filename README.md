
<!-- README.md is generated from README.Rmd. Please edit that file -->

# distancethreshold

<!-- badges: start -->

[![R build
status](https://github.com/jaredlander/distancethreshold/workflows/R-CMD-check/badge.svg)](https://github.com/jaredlander/distancethreshold/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of distancethreshold is to …

## Installation

<!-- You can install the released version of distancethreshold from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("distancethreshold") -->

<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jaredlander/distancethreshold")
```

## What is this package for?

When computing distance matrices sometimes we only care about
observations that fall within a certain threshold of each other. This
package calculates just that with that added feature that

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(distancethreshold)
```

``` r
set.seed(62)

thedf <- data.frame(
    ID=rep(LETTERS[1:3], length.out=10),
    x=sample(10),
    y=sample(10),
    extra1=sample(letters, size=10),
    extra2=sample(letters, size=10),
    extra3=sample(10)
)

threshold_distance(thedf, threshold=3, as_dataframe=FALSE)
#> $i
#> [1] 1 3 5 6 8
#> 
#> $j
#> [1]  2  4  7  8 10
#> 
#> $distance
#> [1] 1.414214 2.236068 2.828427 2.828427 2.236068
#> 
#> $kept
#> [1] 5
#> 
#> $skipped
#> [1] 40
#> 
#> $ID_1
#> [1] "A" "B" "B" "A" "C"
#> 
#> $ID_2
#> [1] "C" "A" "C" "C" "B"
threshold_distance(thedf, threshold=3, as_dataframe=TRUE)
#>    i  j distance ID_1 ID_2
#> 1: 1  2 1.414214    A    C
#> 2: 6  8 2.828427    A    C
#> 3: 3  4 2.236068    B    A
#> 4: 5  7 2.828427    B    C
#> 5: 8 10 2.236068    C    B
```
