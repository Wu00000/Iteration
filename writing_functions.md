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

    ##  [1]  1.5775275 -0.1183170  0.7834300 -0.2279895  1.7109713  0.3831088
    ##  [7] -0.1151540 -0.4141668 -0.2091053  0.2893728 -0.3882604  1.6543994
    ## [13] -0.4958023 -1.7437362 -0.5373870 -1.8081135  1.2976787 -1.3891566
    ## [19]  0.6997688 -0.1896278 -1.5284536  0.1894843  0.1715809 -0.5338778
    ## [25]  0.9418255

``` r
z_scores = function(x) {
  z <- (x - mean(x)) / sd(x)
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1]  1.5775275 -0.1183170  0.7834300 -0.2279895  1.7109713  0.3831088
    ##  [7] -0.1151540 -0.4141668 -0.2091053  0.2893728 -0.3882604  1.6543994
    ## [13] -0.4958023 -1.7437362 -0.5373870 -1.8081135  1.2976787 -1.3891566
    ## [19]  0.6997688 -0.1896278 -1.5284536  0.1894843  0.1715809 -0.5338778
    ## [25]  0.9418255

``` r
y_vec <- rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1] -0.149119237  0.009003732  0.230998897 -0.645237282  1.022799425
    ##  [6]  0.302994113 -0.010188567  1.433278636  1.033616964 -0.335431389
    ## [11]  0.356257752 -0.755828065  1.600768427  1.742757126  0.302873715
    ## [16] -1.590929905  0.291618249 -1.728923973  0.454121545 -0.231989538
    ## [21]  0.894150020 -0.743438811 -1.232157460  0.258441921  0.589810044
    ## [26] -1.356240719  0.622309152 -0.987110554 -2.101374050 -1.403764895
    ## [31] -0.195649861 -0.288615787  2.313410210  0.543405258 -0.027514549
    ## [36]  0.181091444  0.674435490 -0.139730586 -1.538367602  0.603470713

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
    ## 1  3.95  4.13

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.309

## Different sample sizes, means, sds

``` r
sim_data <- 
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.26  2.64

Let’s write a function that simulates data, computes the mean and sd.

``` r
sim_mean_sd <- function(n, mu = 2, sigma = 3) {
  
  # do checks on inputs
  
  sim_data <- 
    tibble(
      x = rnorm(n, mean = mu, sd = sigma)
    )
  
  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
    
}

sim_mean_sd(30, 4, 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.23  3.59

``` r
sim_mean_sd(30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.76  3.39
