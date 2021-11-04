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
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
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

    ##  [1]  0.06589550 -0.54424800  0.12731059 -0.07396901  0.01317567  0.13695808
    ##  [7] -1.21307495 -1.47237791 -1.96853856 -0.04245768 -0.13008940  1.66577666
    ## [13]  1.73533641  1.43254238 -1.40442468  1.34528056 -0.43707798 -0.96261780
    ## [19]  0.34433303  0.62794986 -1.04183101  0.25744957 -0.02572742  1.28137654
    ## [25]  0.28304957

``` r
z_scores = function(x) {
  z <- (x - mean(x)) / sd(x)
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1]  0.06589550 -0.54424800  0.12731059 -0.07396901  0.01317567  0.13695808
    ##  [7] -1.21307495 -1.47237791 -1.96853856 -0.04245768 -0.13008940  1.66577666
    ## [13]  1.73533641  1.43254238 -1.40442468  1.34528056 -0.43707798 -0.96261780
    ## [19]  0.34433303  0.62794986 -1.04183101  0.25744957 -0.02572742  1.28137654
    ## [25]  0.28304957

``` r
y_vec <- rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1]  0.29139230 -2.09401995  1.10869498 -0.28333720 -1.75557209  0.83575184
    ##  [7]  1.62157016  0.58749071  1.11707490 -1.18674731 -0.47719869 -1.30320356
    ## [13]  0.24281563  0.12089385  1.06100106 -1.49913432 -1.44608274  0.21643093
    ## [19]  0.41263753  0.29109123 -0.62353428 -0.15518195 -0.34947164 -1.37341502
    ## [25]  0.76164089 -0.49987567  1.74172771  0.07269948 -0.61197794  0.11694806
    ## [31]  0.78689433 -1.86217890  0.31397300  0.10964655  0.59206732  1.32905236
    ## [37] -0.68462623  0.52877931  1.42562336  0.51965998

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
    ## 1  4.48  4.14

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.278

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
    ## 1  1.50  2.76

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
    ## 1  4.65  2.60

``` r
sim_mean_sd(30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.98  2.47

## Napoleon Dynamite

``` r
url <-  "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html <- read_html(url)

review_titles <- 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

review_stars <-  
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text <-  
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()

reviews <-
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
)
```

Okay but there are a lot of pages of reviews

Write a function that gets reviews based on page url

``` r
get_page_reviews <- function(page_url) {
  
  page_html <- read_html(url)

  review_titles <- 
    page_html %>%
    html_elements(".a-text-bold span") %>%
    html_text()
  
  review_stars <-  
    page_html %>%
    html_elements("#cm_cr-review_list .review-rating") %>%
    html_text()
  
  review_text <-  
    page_html %>%
    html_elements(".review-text-content span") %>%
    html_text()
  
  reviews <-
    tibble(
      title = review_titles,
      stars = review_stars,
      text = review_text
      )
  
  return(reviews)
}

base_url <- "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

urls = str_c(base_url, 1:5)

bind_rows(
  get_page_reviews(urls[1]),
  get_page_reviews(urls[2]),
  get_page_reviews(urls[3]),
  get_page_reviews(urls[4]),
  get_page_reviews(urls[5])
)
```

    ## # A tibble: 50 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  2 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  3 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  4 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  5 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ##  6 Painful                                               1.0 ou… "\n  I think I…
    ##  7 GRAND                                                 5.0 ou… "\n  GRAND\n"  
    ##  8 Hello, 90s                                            5.0 ou… "\n  So nostal…
    ##  9 Cult Classic                                          5.0 ou… "\n  Watched i…
    ## 10 Format was inaccurate                                 4.0 ou… "\n  There was…
    ## # … with 40 more rows

## About scoping

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4
