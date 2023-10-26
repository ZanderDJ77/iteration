writing functions
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(p8105.datasets)
```

Set seed for reproducibility

``` r
set.seed(12345)
```

### Z Score Function

z scores subtract the mean and divide by the standard deviation

``` r
x_vec = rnorm(20, mean = 5, sd = .3)
```

Basic Compute Z scores for `x_vec`

``` r
(x_vec - mean(x_vec))/sd(x_vec)
```

    ##  [1]  0.6103734  0.7589907 -0.2228232 -0.6355576  0.6347861 -2.2717259
    ##  [7]  0.6638185 -0.4229355 -0.4324994 -1.1941438 -0.2311505  2.0874460
    ## [13]  0.3526784  0.5320552 -0.9917420  0.8878182 -1.1546150 -0.4893597
    ## [19]  1.2521303  0.2664557

Write a function to do this

``` r
z_score = function(x) {
  
  if(!is.numeric(x)) {
    stop("Argument x should be numeric")} 
  else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")}
  z = ((x - mean(x))/sd(x))
  
  z
}
```

Check that this works

``` r
z_score(x = x_vec)
```

    ##  [1]  0.6103734  0.7589907 -0.2228232 -0.6355576  0.6347861 -2.2717259
    ##  [7]  0.6638185 -0.4229355 -0.4324994 -1.1941438 -0.2311505  2.0874460
    ## [13]  0.3526784  0.5320552 -0.9917420  0.8878182 -1.1546150 -0.4893597
    ## [19]  1.2521303  0.2664557

``` r
z_score(x = rnorm(10, mean = 5))
```

    ##  [1]  0.5952213  1.1732833 -0.6221352 -1.3990896 -1.4371950  1.4719158
    ##  [7] -0.4830567  0.4590828  0.4520244 -0.2100512

Keep checking

``` r
z_score(x = 3)

z_score(c("hello", "there"))

z_score(c(TRUE, TRUE, FALSE, TRUE))

z_score(iris)
```

## Multiple Outputs

Write a function that returns the mean and sd from a sample of numbers

``` r
mean_and_sd = function(x) {
  
    if(!is.numeric(x)) {
    stop("Argument x should be numeric")} 
  else if (length(x) < 2) {
    stop("Cannot be computed for length 1 vectors")}
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
}
```

Double check

``` r
mean_and_sd(x= x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.02 0.250

### Start getting means and sds

``` r
x_vec = rnorm(n = 30, mean = 5, sd = 0.5)

tibble(
  mean = mean(x_vec),
  sd = sd(x_vec)
)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.16 0.643

Create a function that uses ‘n’ a true mean, and true SD as inputs

``` r
sim_mean_sd = function(n_obs, mu, sigma) {
  
  x_vec = rnorm(n = n_obs, mean = mu, sd = sigma)

tibble(
  mean = mean(x_vec),
  sd = sd(x_vec)
)
  
}

sim_mean_sd(n_obs = 3000, mu = 50, sigma = 5)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  50.0  4.91
