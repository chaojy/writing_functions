Writing functions
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

    ##  [1]  1.57985856 -0.23108263 -0.21916923  0.56463074  1.25134064 -1.18588031
    ##  [7] -0.49854945  0.42191563 -1.93346638  0.37218639  0.45105859 -2.17778826
    ## [13]  0.37652409  0.94829419 -1.36100536 -0.37675943 -0.34826799 -1.20083911
    ## [19] -1.48563947  0.06616326  1.44880155  0.60574091  0.41934691 -0.44848129
    ## [25]  1.57365112  0.53292247  0.44804360 -0.13618456  1.00687382 -0.46423899

I want a function to compute z-scores

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}
## the input is x

z_scores(x_vec)
```

    ##  [1]  1.57985856 -0.23108263 -0.21916923  0.56463074  1.25134064 -1.18588031
    ##  [7] -0.49854945  0.42191563 -1.93346638  0.37218639  0.45105859 -2.17778826
    ## [13]  0.37652409  0.94829419 -1.36100536 -0.37675943 -0.34826799 -1.20083911
    ## [19] -1.48563947  0.06616326  1.44880155  0.60574091  0.41934691 -0.44848129
    ## [25]  1.57365112  0.53292247  0.44804360 -0.13618456  1.00687382 -0.46423899

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

    ##  [1]  1.57985856 -0.23108263 -0.21916923  0.56463074  1.25134064 -1.18588031
    ##  [7] -0.49854945  0.42191563 -1.93346638  0.37218639  0.45105859 -2.17778826
    ## [13]  0.37652409  0.94829419 -1.36100536 -0.37675943 -0.34826799 -1.20083911
    ## [19] -1.48563947  0.06616326  1.44880155  0.60574091  0.41934691 -0.44848129
    ## [25]  1.57365112  0.53292247  0.44804360 -0.13618456  1.00687382 -0.46423899

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

    ##  [1]  1.57985856 -0.23108263 -0.21916923  0.56463074  1.25134064 -1.18588031
    ##  [7] -0.49854945  0.42191563 -1.93346638  0.37218639  0.45105859 -2.17778826
    ## [13]  0.37652409  0.94829419 -1.36100536 -0.37675943 -0.34826799 -1.20083911
    ## [19] -1.48563947  0.06616326  1.44880155  0.60574091  0.41934691 -0.44848129
    ## [25]  1.57365112  0.53292247  0.44804360 -0.13618456  1.00687382 -0.46423899

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

    ##  [1]  1.57985856 -0.23108263 -0.21916923  0.56463074  1.25134064 -1.18588031
    ##  [7] -0.49854945  0.42191563 -1.93346638  0.37218639  0.45105859 -2.17778826
    ## [13]  0.37652409  0.94829419 -1.36100536 -0.37675943 -0.34826799 -1.20083911
    ## [19] -1.48563947  0.06616326  1.44880155  0.60574091  0.41934691 -0.44848129
    ## [25]  1.57365112  0.53292247  0.44804360 -0.13618456  1.00687382 -0.46423899

Check that the function works.

``` r
x_vec = rnorm(1000)

mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0304 0.948

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
    ## 1  3.12  4.38

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
    ## 1  4.00  2.95

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
    ## 1  5.97  2.81

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
    ## 1  5.67  2.97

``` r
## positional mapping - R knows that the numbers corespond to the position 

sim_mean_sd(samp_size = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.73  3.00

``` r
## named matching - this is preferred and best
sim_mean_sd(mu = 6, samp_size = 100, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.94  3.30

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
    ## 1  5.85  3.07

``` r
#3 here, mu = 6 will overwrite the above default, and sigma will overwrite the above default
sim_mean_sd(mu = 6, samp_size = 100, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.06  3.47

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.73  3.74

``` r
#but here, when not specified, it will default to the avove default values specified

#so we are able to do good stuff with functions
```

## Let’s review Napolean Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>% 
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>%
  str_replace_all("\n", "") %>%
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

What about the next page of review…

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>% 
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>%
  str_replace_all("\n", "") %>%
  str_trim()

reviews_page2 = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

So what’s going on is to iterate, you need to copy, paste code chunks,
manually change webpage number, change dataframe name. This is tedious
and also prone to error. Many many reviews and at 10 reviews each page,
this will take a long time. Hot mess. How to automate??

Let’s turn that code into a function

``` r
# the only change is the first line of code to pull in the url - the url is the only thing that changes

read_page_reviews = function(url) {
  
  html = read_html(url)

  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>% 
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>%
    str_replace_all("\n", "") %>%
    str_trim()
  
  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )

  reviews
  
}
```

Let me try my function.

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

read_page_reviews(dynamite_url)
```

    ## # A tibble: 10 x 3
    ##    title                    stars text                                          
    ##    <chr>                    <dbl> <chr>                                         
    ##  1 One big yawn                 1 Stupid movie.                                 
    ##  2 NAP-MITE                     5 I like this movie and recomend it.            
    ##  3 Vote for Pedro!              5 Just watch the movie. Gosh!                   
    ##  4 Just watch the freaking…     5 Its a great movie, gosh!!                     
    ##  5 Great Value                  5 Great Value                                   
    ##  6 I LOVE THIS MOVIE            5 THIS MOVIE IS SO FUNNY ONE OF MY FAVORITES    
    ##  7 Don't you wish you coul…     5 Watch it 100 times. Never. Gets. Old.         
    ##  8 Stupid, but very funny!      5 If you like stupidly funny '90s teenage movie…
    ##  9 The beat                     5 The best                                      
    ## 10 Hilarious                    5 Super funny! Loved the online rental.

``` r
## this works, and can go back into code to customize or change what you want
```

Let’s read a few pages of reviews.

``` r
dynamite_url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_urls = str_c(dynamite_url_base, 1:50)

dynamite_urls[1]
```

    ## [1] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

``` r
dynamite_urls[2]
```

    ## [1] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

``` r
dynamite_urls[5]
```

    ## [1] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=5"

``` r
all_reviews = 
  bind_rows(
    read_page_reviews(dynamite_urls[1]),
    read_page_reviews(dynamite_urls[2]),
    read_page_reviews(dynamite_urls[3]),
    read_page_reviews(dynamite_urls[4]),
    read_page_reviews(dynamite_urls[5])
)

## up to now, this is copying and pasting code and entering new numbers each time - this is also labor intensive.
## this sets the stage for future videos about how to streamline and make more efficient
```

If you look at environment, it is starting to have a lot of “Data”,
“Values”, and “Functions” - it can get confusing and predispose to
error - so Jeff restarts R at this point to be careful - and this gets
rid of stuff in the environment

This will make the environment empty

This is the concept of “scoping” - be careful about scoping - it can be
a difficult kind of thing

## Mean scoping example

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

next example

``` r
f = function(x1) {
  z = x1 + x2
  z
}

x = 1
y = 2

f(x1 = y)
```

    ## Error in f(x1 = y): object 'x2' not found

``` r
## this breaks because x2 is not found in the environment
```

So a lot of this can be confusing and problematic - and when you restart
next session, your code can break and will return the message like
“Error in f(x1 = y) : object ‘x2’ not found” - this is a scoping issue

## Functions as arguments

``` r
my_summary = function(x, summ_func) {
  
  summ_func(x)
  
}

x_vec = rnorm(100, 3, 7)

mean(x_vec)
```

    ## [1] 2.951014

``` r
median(x_vec)
```

    ## [1] 2.467033

``` r
my_summary(x_vec, mean)
```

    ## [1] 2.951014

``` r
my_summary(x_vec, median)
```

    ## [1] 2.467033

``` r
my_summary(x_vec, sd)
```

    ## [1] 7.964808

``` r
my_summary(x_vec, IQR)
```

    ## [1] 12.34136
