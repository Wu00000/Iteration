iteration\_listcols
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
library(purrr)
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

## Lists

``` r
l <- 
  list(
    vec_numeric = 5:8,
    vec_logical = c(TRUE, FALSE),
    summary =  summary(rnorm(1000, mean = 5, sd = 3))
  )

l[[3]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.773   2.896   4.940   4.969   7.047  15.961

``` r
l$summary
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.773   2.896   4.940   4.969   7.047  15.961

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
```

## List of normals

``` r
list_norms <- 
  list(
    a = rnorm(50, mean = 2, sd = 1),
    b = rnorm(50, mean = 5, sd = 3),
    c = rnorm(50, mean = 20, sd = 1.2),
    d = rnorm(50, mean = -12, sd = 0.5)
  )

mean_and_sd((list_norms[[1]]))
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.18  1.08

## for loop

Let’s use a for loop to iterate over my list of normals.

``` r
output <- vector("list", length = 4)


for (i in 1:4) {
  
  output[[i]] <- mean_and_sd(list_norms[[i]])
  
}
```

Let’s use map instead

``` r
output <- map(list_norms, mean_and_sd)

output <- map(list_norms, IQR)

output <- map_dbl(list_norms, median)
```

## LIST COLUMNS!!!!

``` r
listcol_df <- 
  tibble(
    name = c("a", "b", "c", "d"),
    norms = list_norms
  )

listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 × 2
    ##   name  norms       
    ##   <chr> <named list>
    ## 1 a     <dbl [50]>

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(norms)
```

    ## $a
    ##  [1]  3.03631735  3.77642873  0.37003841  1.48431876  2.31123719  2.87044520
    ##  [7]  2.38002779  3.04478251  2.36830022  2.03370911 -0.09487466  1.91386617
    ## [13]  4.61196666  3.05681894  1.36201766  1.99582933  2.27834830  2.42810489
    ## [19]  1.51710888  1.97613464  2.47062817  3.40451476  1.87868488  2.35147849
    ## [25]  3.29040799  2.79319765  0.98458580  0.39776745  2.63707729  1.08394737
    ## [31]  0.89241690  1.44285587  3.63083237  2.46836675  2.21487253  1.91333403
    ## [37]  3.22393690  0.57851616  0.94593149  3.03901946  3.24158769  3.46905906
    ## [43]  0.80764778  2.04852669  1.58320270  3.92982883 -0.10824243  2.33306012
    ## [49]  3.83329679  1.67093447
    ## 
    ## $b
    ##  [1]  2.99775709  7.36239241  5.42170466  0.40522349  7.30305118  3.18348612
    ##  [7] 11.61651502 11.56474667  3.07249794  4.52720369  0.21589650  6.93180027
    ## [13]  7.91684842  4.45258522  3.24382826  6.06483503  3.69068846 11.69700900
    ## [19]  7.51095934  2.15156122  5.80237978  1.16808400  7.22832120  3.62988717
    ## [25]  4.73248899  4.37030624  4.75713140  3.76084581  6.99179546  7.62025637
    ## [31]  6.74904338 -2.25295716  3.00418446  0.02195222  5.01951084  6.39500573
    ## [37]  3.41257639  7.39706604  2.20059575  3.96774832  9.22520081  2.21126123
    ## [43]  6.08821813  6.23080538  5.86933275  5.65775150  8.51618043  3.80592522
    ## [49]  1.73779688  5.63353133
    ## 
    ## $c
    ##  [1] 19.44918 21.16648 18.97396 22.12765 18.03562 18.77876 19.94549 20.43804
    ##  [9] 20.99133 20.80964 20.75605 19.85723 19.56427 21.80118 17.71719 20.57843
    ## [17] 19.94033 21.16468 21.55236 20.72634 19.61544 20.80498 20.22731 19.97814
    ## [25] 19.99623 18.33373 22.01984 20.93069 19.67953 20.47691 19.68772 20.41504
    ## [33] 19.30097 20.55966 22.10117 18.34616 20.42822 19.39515 21.94340 19.50384
    ## [41] 20.03380 19.31590 19.71500 19.68944 22.15419 21.30203 19.60209 19.99531
    ## [49] 20.45050 18.97907
    ## 
    ## $d
    ##  [1] -11.79155 -11.95938 -12.57647 -11.32654 -11.15839 -12.79198 -12.02429
    ##  [8] -12.32783 -12.24593 -11.35615 -13.17304 -11.84437 -11.40387 -11.81288
    ## [15] -12.21347 -11.83560 -11.88107 -12.87974 -12.68711 -11.79431 -11.90254
    ## [22] -11.78507 -13.31353 -12.72103 -11.59834 -12.16584 -11.56451 -12.43605
    ## [29] -12.17469 -12.13545 -12.45853 -10.81254 -12.11586 -11.57808 -12.42358
    ## [36] -11.00499 -11.99037 -12.43112 -11.38029 -12.69846 -11.67529 -11.91163
    ## [43] -11.43452 -13.24421 -11.72646 -12.24400 -11.07597 -11.72642 -11.64468
    ## [50] -11.45262

``` r
map(listcol_df$norms, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.18  1.08
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.05  2.94
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  20.2  1.08
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -12.0 0.578

``` r
listcol_df %>% 
  mutate(summaries = map(norms, mean_and_sd))
```

    ## # A tibble: 4 × 3
    ##   name  norms        summaries       
    ##   <chr> <named list> <named list>    
    ## 1 a     <dbl [50]>   <tibble [1 × 2]>
    ## 2 b     <dbl [50]>   <tibble [1 × 2]>
    ## 3 c     <dbl [50]>   <tibble [1 × 2]>
    ## 4 d     <dbl [50]>   <tibble [1 × 2]>

## Nested data

``` r
weather_df <- 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2021-10-05 10:30:04 (7.602)

    ## file min/max dates: 1869-01-01 / 2021-10-31

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2021-10-05 10:30:09 (1.697)

    ## file min/max dates: 1965-01-01 / 2020-02-29

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2021-10-05 10:30:11 (0.912)

    ## file min/max dates: 1999-09-01 / 2021-09-30

Nest data within location

``` r
weather_nested <- nest(weather_df, data = date:tmin)

weather_nested %>% 
  pull(data)
```

    ## [[1]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # … with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # … with 355 more rows

``` r
weather_lm <- function(df){
  
  lm(tmax ~ tmin, data = df)
  
}

weather_lm(weather_nested$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
weather_nested %>% 
  mutate(lm_results = map(data, weather_lm))
```

    ## # A tibble: 3 × 4
    ##   name           id          data               lm_results
    ##   <chr>          <chr>       <list>             <list>    
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>      
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>      
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>
