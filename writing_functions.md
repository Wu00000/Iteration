writing functions
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(p8105.datasets)
data(mtcars)

knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Z scores

``` r
x_vec <- rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.01111415 -0.22942836 -1.59964737  1.70819110 -1.16641990 -1.17997823
    ##  [7]  0.17869969 -1.59652502 -0.26694615  0.17445320  0.38661472 -0.76053332
    ## [13]  0.55948988  0.05214029  1.74931148  0.38677743 -0.41869946  0.09940901
    ## [19]  0.80485768 -0.57065776  0.77063662  2.25152907 -0.65386104 -1.14315589
    ## [25]  0.47485645

``` r
z_scores = function(x) {
  z <- (x - mean(x)) / sd(x)
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1] -0.01111415 -0.22942836 -1.59964737  1.70819110 -1.16641990 -1.17997823
    ##  [7]  0.17869969 -1.59652502 -0.26694615  0.17445320  0.38661472 -0.76053332
    ## [13]  0.55948988  0.05214029  1.74931148  0.38677743 -0.41869946  0.09940901
    ## [19]  0.80485768 -0.57065776  0.77063662  2.25152907 -0.65386104 -1.14315589
    ## [25]  0.47485645

``` r
y_vec <- rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1] -1.73021091  0.37367207 -0.64444940  0.02744409 -0.75895261  1.71258801
    ##  [7] -0.68859681 -0.40147771 -0.32849279  2.10645591 -0.04753901  1.46403785
    ## [13]  1.95873675  0.37877274 -0.43818272  1.06568379 -0.81024490  0.43713506
    ## [19] -1.18613878  1.04879106  0.43888053  0.06032831 -1.67883690 -0.45878796
    ## [25]  0.27785177 -1.33979351  0.17278308  0.39502615  0.40881762  0.42510591
    ## [31]  0.45928403  1.47351721  0.36078115 -0.35416711 -1.61807292 -0.85858570
    ## [37] -0.51733425  0.53811915  0.20708857 -1.93103680

How great is this??

``` r
z_scores = function(x) {
  
  if(!is.numeric(x)) {
    stop("x needs to be numeric")
  }  
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  z <- (x - mean(x)) / sd(x)
  return(z)
}
```

``` r
z_scores(3)
```

    ## Error in z_scores(3): x should have at least 3 numbers

``` r
z_scores(c("my", "name", "is", "Daniel"))
```

    ## Error in z_scores(c("my", "name", "is", "Daniel")): x needs to be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): x needs to be numeric
