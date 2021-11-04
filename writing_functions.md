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

    ##  [1]  1.2760343 -0.9527982  1.9846628  2.4438732  0.5538170 -1.3626961
    ##  [7]  1.1881872  0.1083730 -0.9400274 -0.2046733 -1.1206543  0.1160940
    ## [13] -0.7317219 -0.5385993 -0.2717940 -0.1210031 -0.2842463  0.8024273
    ## [19]  0.2493304 -0.9592236  0.7270123  0.3000728 -1.4603803 -0.2054600
    ## [25] -0.5966064

``` r
z_scores = function(x) {
  z <- (x - mean(x)) / sd(x)
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1]  1.2760343 -0.9527982  1.9846628  2.4438732  0.5538170 -1.3626961
    ##  [7]  1.1881872  0.1083730 -0.9400274 -0.2046733 -1.1206543  0.1160940
    ## [13] -0.7317219 -0.5385993 -0.2717940 -0.1210031 -0.2842463  0.8024273
    ## [19]  0.2493304 -0.9592236  0.7270123  0.3000728 -1.4603803 -0.2054600
    ## [25] -0.5966064

``` r
y_vec <- rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1]  1.449998361 -2.462023594 -1.711578697  0.729588845 -0.954966666
    ##  [6] -0.089877842  0.353936327  0.872704829 -0.220738406  1.839172260
    ## [11] -1.037351896  0.513503211  1.587147498 -0.319787373 -1.696971002
    ## [16]  1.429832306  0.343961557 -1.051039401  0.932154460 -0.132650620
    ## [21]  0.865289901 -0.841228203  0.709790292 -0.053899918  1.442537829
    ## [26] -0.614698322  0.226090625  0.416549613  0.412234462 -1.236983649
    ## [31] -1.237367820  0.008485679  0.022843201 -0.230701472  0.059320452
    ## [36]  0.867621105 -0.566973107 -0.387116643  0.870058821 -1.106867006

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

## Mean and sd functions

``` r
mean_and_sd = function(x) {
  
  if(!is.numeric(x)) {
    stop("x needs to be numeric")
  }  
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  mean_x <- mean(x)
  sd_x <- sd(x)
  
  output_df <- 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(output_df)
  
}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.88  3.71

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.333
