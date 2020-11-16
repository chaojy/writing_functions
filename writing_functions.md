Writing functinos
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
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
```

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.485647876 -2.630563259 -0.603606225 -0.312251461  0.961694015
    ##  [6] -0.048981788  0.210305740  0.931705272  0.008258203 -0.087647205
    ## [11]  0.514274450  1.123036058  1.461688014 -0.549214077  0.138540836
    ## [16] -0.523342905  1.386806122 -0.798327831 -0.807362828 -1.671573225
    ## [21]  0.046672350 -0.381816011 -1.503442349  2.273668675  0.675827056
    ## [26]  0.035544195  0.324880294  0.957827679 -0.365403113 -0.281548806

I want a function to compute z-scores

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}
## the input is x

z_scores(x_vec)
```

    ##  [1] -0.485647876 -2.630563259 -0.603606225 -0.312251461  0.961694015
    ##  [6] -0.048981788  0.210305740  0.931705272  0.008258203 -0.087647205
    ## [11]  0.514274450  1.123036058  1.461688014 -0.549214077  0.138540836
    ## [16] -0.523342905  1.386806122 -0.798327831 -0.807362828 -1.671573225
    ## [21]  0.046672350 -0.381816011 -1.503442349  2.273668675  0.675827056
    ## [26]  0.035544195  0.324880294  0.957827679 -0.365403113 -0.281548806

Try my function on some other things. These should give errors

``` r
z_scores(3)
```

    ## [1] NA

``` r
z_scores("my name is jeff")
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in x - mean(x): non-numeric argument to binary operator

``` r
## doesn't work for a character vector
z_scores(mtcars)
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in is.data.frame(x): 'list' object cannot be coerced to type 'double'

``` r
## can't take a mean for a dataset
z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## [1]  0.5  0.5 -1.5  0.5

``` r
## actually returns a response - but probably not what you want
```

``` r
z_scores = function(x) {
  
## conditional
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}
## the input is x

z_scores(x_vec)
```

    ##  [1] -0.485647876 -2.630563259 -0.603606225 -0.312251461  0.961694015
    ##  [6] -0.048981788  0.210305740  0.931705272  0.008258203 -0.087647205
    ## [11]  0.514274450  1.123036058  1.461688014 -0.549214077  0.138540836
    ## [16] -0.523342905  1.386806122 -0.798327831 -0.807362828 -1.671573225
    ## [21]  0.046672350 -0.381816011 -1.503442349  2.273668675  0.675827056
    ## [26]  0.035544195  0.324880294  0.957827679 -0.365403113 -0.281548806

These should give erros

``` r
z_scores(3)
```

    ## [1] NA

``` r
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): Input must be numeric

``` r
## now it tells you input must be numeric
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): Input must be numeric

``` r
## now it tells you input must be numeric
z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): Input must be numeric

``` r
## now it tells you input must be numeric
```

``` r
z_scores = function(x) {
  
## conditional
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
## added another condition - I want things to break if vector is less than 3 numbers
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}
## the input is x

z_scores(x_vec)
```

    ##  [1] -0.485647876 -2.630563259 -0.603606225 -0.312251461  0.961694015
    ##  [6] -0.048981788  0.210305740  0.931705272  0.008258203 -0.087647205
    ## [11]  0.514274450  1.123036058  1.461688014 -0.549214077  0.138540836
    ## [16] -0.523342905  1.386806122 -0.798327831 -0.807362828 -1.671573225
    ## [21]  0.046672350 -0.381816011 -1.503442349  2.273668675  0.675827056
    ## [26]  0.035544195  0.324880294  0.957827679 -0.365403113 -0.281548806

These should give errors

``` r
z_scores(3)
```

    ## Error in z_scores(3): Input must have at least three numbers

``` r
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): Input must be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): Input must be numeric

``` r
z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): Input must be numeric

``` r
## now everything is working the way we want it to - responses are appropriate
```

## Multiple outputs

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }

  mean_x = mean(x)
  sd_x = sd(x)
 
## one option is list   
#  list(
#    mean = mean_x,
#    sd = sd_x
#  )
  
# we will do tibble
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
## the input is x

z_scores(x_vec)
```

    ##  [1] -0.485647876 -2.630563259 -0.603606225 -0.312251461  0.961694015
    ##  [6] -0.048981788  0.210305740  0.931705272  0.008258203 -0.087647205
    ## [11]  0.514274450  1.123036058  1.461688014 -0.549214077  0.138540836
    ## [16] -0.523342905  1.386806122 -0.798327831 -0.807362828 -1.671573225
    ## [21]  0.046672350 -0.381816011 -1.503442349  2.273668675  0.675827056
    ## [26]  0.035544195  0.324880294  0.957827679 -0.365403113 -0.281548806

Check that the function works.

``` r
x_vec = rnorm(1000)

mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0192 0.968

``` r
#awesome
```

## Multiple inputs

``` r
x_vec = rnorm(100, mean = 3, sd = 4)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.58  4.12

``` r
## can just run this to create multiple vectors and compute mean/sd multiple times
```

Create a simulated data set

``` r
sim_data =
  tibble(
    x = rnorm(n = 100, mean = 4, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.08  2.71

I’d like to do this with a function

``` r
sim_mean_sd = function(samp_size, mu, sigma) {
  
  sim_data =
    tibble(
      x = rnorm(n = samp_size, mean = mu, sd = sigma)
  )

  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
  ) 
  
  
}

sim_mean_sd(100, 6, 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.70  3.00

``` r
## so what happens with this line of code is that it runs all the previous above code - creates new tibble each time with new/different numbers, means, and sd with specified sample size, mean, and sd.
## this sets the stage for later lecture about simulation - learning something about randomness, the "sampling variation"
```

``` r
sim_mean_sd(100, 6, 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.32  2.81

``` r
## positional mapping - R knows that the numbers corespond to the position 

sim_mean_sd(samp_size = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.81  2.57

``` r
## named matching - this is preferred and best
sim_mean_sd(mu = 6, samp_size = 100, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.50  2.63

``` r
## in matched mapping, order does not matter

## next thing you can do is set default values:
sim_mean_sd = function(samp_size, mu = 3, sigma = 4) {
  
  sim_data =
    tibble(
      x = rnorm(n = samp_size, mean = mu, sd = sigma)
  )

  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
  ) 
  
  
}

sim_mean_sd(samp_size = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.42  2.83

``` r
#3 here, mu = 6 will overwrite the above default, and sigma will overwrite the above default
sim_mean_sd(mu = 6, samp_size = 100, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.87  2.84

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.84  3.72

``` r
#but here, when not specified, it will default to the avove default values specified

#so we are able to do good stuff with functions
```
