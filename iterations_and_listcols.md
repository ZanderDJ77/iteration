iterations, list columns
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

``` r
vec_numeric = 5:8
vec_char = c("My", "name", "is", "Jeff")
vec_logical = c(TRUE, TRUE, TRUE, FALSE)
```

``` r
l = 
  list(
    vec_numeric = 1:5,
    vec_char = LETTERS,
    matrix = matrix(1:10, nrow = 5, ncol = 2),
    summary = summary(rnorm(100))
  )
```

Accessing lists

``` r
l$vec_char
```

    ##  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
    ## [20] "T" "U" "V" "W" "X" "Y" "Z"

``` r
l[[2]]
```

    ##  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
    ## [20] "T" "U" "V" "W" "X" "Y" "Z"

``` r
l[["summary"]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -2.3804 -0.5901  0.4837  0.2452  0.9004  2.4771

### Loops

``` r
list_norm_samples = 
  list(
    a = rnorm(20, 1, 5),
    b = rnorm(20, 0, 7),
    c = rnorm(20, 20, 1),
    d = rnorm(20, -45, 13)
  )
```

Function from last class

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

Hard to apply a funciton to many inputs ion a list without getting
disorganized

``` r
mean_and_sd(list_norm_samples$a)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.25  4.92

``` r
mean_and_sd(list_norm_samples$b)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.690  9.30

``` r
mean_and_sd(list_norm_samples$c)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  19.8 0.910

``` r
mean_and_sd(list_norm_samples$d)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -44.1  14.0

Creating a For Loop, i is the variable for different interchaning inputs

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm_samples[[i]])
  
}
```

### use `map`

``` r
output2 = map(list_norm_samples, mean_and_sd)

#the map function can do a variety of outputs
output_median = map(list_norm_samples, median)
output_summary = map(list_norm_samples, summary)
```

### Create DF

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm_samples
  )
```

``` r
listcol_df |> pull(samp)
```

    ## $a
    ##  [1]  2.1196270 -4.7811167  3.1120926 -5.6237763  1.7054216 -1.6802400
    ##  [7] -0.5580304  8.7805482 -1.2401665  2.6056177 -5.1508612 -5.6202935
    ## [13]  7.3062114  7.5961586  0.5962312 -1.5254490  0.7392320  4.1443031
    ## [19] 11.9000120  0.6549135
    ## 
    ## $b
    ##  [1]  10.8140452   9.2501641   2.2550610  10.7166858  -2.9486779  -8.1117472
    ##  [7] -12.9175780   8.1012770 -14.8648492  -8.3722206  11.4953439   6.1855838
    ## [13]   3.6741312  -8.2926135  18.5905179  -7.3353960  -7.0778577   4.6824516
    ## [19]   0.9042411  -2.9580381
    ## 
    ## $c
    ##  [1] 18.85974 18.70628 19.40530 18.49919 20.01586 20.54017 18.45271 20.84965
    ##  [9] 20.89601 20.13869 18.38067 20.54840 20.19528 19.19350 19.89138 19.74905
    ## [17] 21.69935 19.65570 20.06777 19.34943
    ## 
    ## $d
    ##  [1] -51.33930 -41.05903 -48.14566 -51.26254 -57.89344 -48.64844 -36.77077
    ##  [8] -61.11764 -22.06392 -45.30784 -42.40103 -27.48649 -44.53104 -34.28045
    ## [15] -67.13473 -38.74765 -12.71385 -39.78226 -42.20270 -68.60426

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.25  4.92
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.690  9.30
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  19.8 0.910
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -44.1  14.0

``` r
listcol_df |> 
  mutate(
    mean_sd = map(samp, mean_and_sd),
    meidan = map(samp, median)) |> 
  select(name, mean_sd)
```

    ## # A tibble: 4 × 2
    ##   name  mean_sd         
    ##   <chr> <named list>    
    ## 1 a     <tibble [1 × 2]>
    ## 2 b     <tibble [1 × 2]>
    ## 3 c     <tibble [1 × 2]>
    ## 4 d     <tibble [1 × 2]>

Function from last class

``` r
 nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

nsduh_import = function(html, table_number, outcome_name) {
  
  html = 
  nsduh_html |> 
  html_table() |> 
  nth(table_number) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent),
    outcome = outcome_name) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
}

nsduh_import(nsduh_html, 1, "marj")
nsduh_import(nsduh_htm, 4, "cocaine")
nsduh_import(nsduh_htm, 5, "heroine")
```

Import Data using a For Loop

``` r
table_input = list(1, 4, 5)
name_input = list("marj", "cocaine", "heroin")

output = vector("list", length = 3)

for(i in c(1:3)) {
  
  output[[i]] = (nsduh_import(msduh_html, table_input[[i]], name_input[[i]]))

  }

nsduh_df = bind_rows(output)
nsduh_df
```

    ## # A tibble: 1,530 × 5
    ##    State   age   year      percent outcome
    ##    <chr>   <chr> <chr>       <dbl> <chr>  
    ##  1 Alabama 12+   2013-2014    9.98 marj   
    ##  2 Alabama 12+   2014-2015    9.6  marj   
    ##  3 Alabama 12-17 2013-2014    9.9  marj   
    ##  4 Alabama 12-17 2014-2015    9.71 marj   
    ##  5 Alabama 18-25 2013-2014   27.0  marj   
    ##  6 Alabama 18-25 2014-2015   26.1  marj   
    ##  7 Alabama 26+   2013-2014    7.1  marj   
    ##  8 Alabama 26+   2014-2015    6.81 marj   
    ##  9 Alabama 18+   2013-2014    9.99 marj   
    ## 10 Alabama 18+   2014-2015    9.59 marj   
    ## # ℹ 1,520 more rows

Lets try again using MAP Statement

``` r
 nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

nsduh_import = function(html, table_number) {
  
  html = 
  nsduh_html |> 
  html_table() |> 
  nth(table_number) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
}

nsduh_df =
  tibble(
    name = c("marj", "cocaine", "heroin"),
    number = c(1, 4, 5)
  ) |> 
  mutate(
    table = map(number, nsduh_import, html = nsduh_html)) |> 
  unnest(table)

#map(nsduh_df$number, nsduh_import, html = nsduh_html)
```

Returning to the Weather Data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USW00022534", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2021-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USW00022534 = "Molokai_HI",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: /Users/zanderdj/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2023-10-31 11:07:04.036221 (8.538)

    ## file min/max dates: 1869-01-01 / 2023-10-31

    ## using cached file: /Users/zanderdj/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00022534.dly

    ## date created (size, mb): 2023-10-31 11:07:09.729959 (3.843)

    ## file min/max dates: 1949-10-01 / 2023-10-31

    ## using cached file: /Users/zanderdj/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2023-10-31 11:07:11.473843 (0.997)

    ## file min/max dates: 1999-09-01 / 2023-10-31

``` r
weather_nest = weather_df |> 
  nest(df = date:tmin)
```

Can I Regress Tmax and Tmin for Each of these

``` r
central_park_df =
  weather_nest |> 
  pull(df) |> 
  nth(1)

central_park_df
```

    ## # A tibble: 730 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2021-01-01   157   4.4   0.6
    ##  2 2021-01-02    13  10.6   2.2
    ##  3 2021-01-03    56   3.3   1.1
    ##  4 2021-01-04     5   6.1   1.7
    ##  5 2021-01-05     0   5.6   2.2
    ##  6 2021-01-06     0   5     1.1
    ##  7 2021-01-07     0   5    -1  
    ##  8 2021-01-08     0   2.8  -2.7
    ##  9 2021-01-09     0   2.8  -4.3
    ## 10 2021-01-10     0   5    -1.6
    ## # ℹ 720 more rows

``` r
weather_lm = function(df) {
  lm(tmax ~ tmin, data = df)
}
weather_lm(central_park_df)
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034

lets try a for loop

``` r
input_list = weather_nest |> pull(df)

output = vector("list", length = 3)

for (i in 1:3) {
  output[[i]] = weather_lm(input_list[[i]])
}

weather_nest |> 
  mutate(models = map(df, weather_lm))
```

    ## # A tibble: 3 × 4
    ##   name           id          df                 models
    ##   <chr>          <chr>       <list>             <list>
    ## 1 CentralPark_NY USW00094728 <tibble [730 × 4]> <lm>  
    ## 2 Molokai_HI     USW00022534 <tibble [730 × 4]> <lm>  
    ## 3 Waterhole_WA   USS0023B17S <tibble [730 × 4]> <lm>
