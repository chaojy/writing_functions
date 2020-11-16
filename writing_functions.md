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

    ##  [1] -0.941265214  0.025095396 -0.328989024  2.241102117  1.085882899
    ##  [6]  0.556551269 -0.427992842 -0.688500249 -1.716026408 -0.599991371
    ## [11] -0.707101210  0.823460005 -0.856860843  0.015417836  0.257642632
    ## [16] -1.135594061 -0.223965721 -0.005288951  0.493727829  0.905118606
    ## [21] -0.053783125  0.487993976  1.882154161 -0.822356049 -0.681183439
    ## [26] -2.116987009 -0.393359226  1.028802762  1.553754843  0.342540408

I want a function to compute z-scores

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}
## the input is x

z_scores(x_vec)
```

    ##  [1] -0.941265214  0.025095396 -0.328989024  2.241102117  1.085882899
    ##  [6]  0.556551269 -0.427992842 -0.688500249 -1.716026408 -0.599991371
    ## [11] -0.707101210  0.823460005 -0.856860843  0.015417836  0.257642632
    ## [16] -1.135594061 -0.223965721 -0.005288951  0.493727829  0.905118606
    ## [21] -0.053783125  0.487993976  1.882154161 -0.822356049 -0.681183439
    ## [26] -2.116987009 -0.393359226  1.028802762  1.553754843  0.342540408

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

    ##  [1] -0.941265214  0.025095396 -0.328989024  2.241102117  1.085882899
    ##  [6]  0.556551269 -0.427992842 -0.688500249 -1.716026408 -0.599991371
    ## [11] -0.707101210  0.823460005 -0.856860843  0.015417836  0.257642632
    ## [16] -1.135594061 -0.223965721 -0.005288951  0.493727829  0.905118606
    ## [21] -0.053783125  0.487993976  1.882154161 -0.822356049 -0.681183439
    ## [26] -2.116987009 -0.393359226  1.028802762  1.553754843  0.342540408

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

    ##  [1] -0.941265214  0.025095396 -0.328989024  2.241102117  1.085882899
    ##  [6]  0.556551269 -0.427992842 -0.688500249 -1.716026408 -0.599991371
    ## [11] -0.707101210  0.823460005 -0.856860843  0.015417836  0.257642632
    ## [16] -1.135594061 -0.223965721 -0.005288951  0.493727829  0.905118606
    ## [21] -0.053783125  0.487993976  1.882154161 -0.822356049 -0.681183439
    ## [26] -2.116987009 -0.393359226  1.028802762  1.553754843  0.342540408

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
## now everything is working the way we want it to
```
