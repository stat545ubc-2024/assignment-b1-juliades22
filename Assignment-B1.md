Assignment B1?
================
Julia D
2024-10-24

## Assignment B1

``` r
#Installing library
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(palmerpenguins)
library(gapminder)
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

``` r
library(devtools)
```

    ## Loading required package: usethis
    ## 
    ## Attaching package: 'devtools'
    ## 
    ## The following object is masked from 'package:testthat':
    ## 
    ##     test_file

### Excerise 1 & 2

Creating a function, doesn’t have to be complex or serious. The function
**should** be flexible. My idea is to create a function that will be
able to group categories and calculate the mean as long as it is a
*numeric variable* Documenting a function, using roxygen2 tags

``` r
#'Grouped Means
#'@description
#'This is a function that groups by a variable and calculates the mean
#'@param Data Data is the dataframe 
#'@param group_var is the grouping variable you want to group by
#'@param mean_var is the column you want the means for 
#'@details This function will return an error for any non-numeric variables in the mean_var param. This function will automatically remove NA variables
#'@returns This function returns a tibble with two columns of the catgeories grouped by and the means for each 
#'@examples for palmer penguins dataset: group_mean(penguins, species, bill_length_mm), returns mean bill length by species.


group_mean <-function(data, group_var, mean_var) {
  if(!is.numeric(data[[deparse(substitute(mean_var))]])) {
    stop('Sorry, this function requires a numeric variable')
    }
  data %>%
    group_by({{group_var}}) %>%
     summarise(mean = mean({{mean_var}}, na.rm = TRUE))
  }
```

### Excerise 3

#### Examples of how this function may be used

Example 1:

``` r
#Finding the average bill length by species in the penguins dataset
answer1.0 <-group_mean(penguins, species, bill_length_mm)
print(answer1.0)
```

    ## # A tibble: 3 × 2
    ##   species    mean
    ##   <fct>     <dbl>
    ## 1 Adelie     38.8
    ## 2 Chinstrap  48.8
    ## 3 Gentoo     47.5

Example 2:

``` r
#Finding the average life expectancy by country in the gapminder dataset
answer2.0 <-group_mean(gapminder, country, lifeExp)
print(answer2.0)
```

    ## # A tibble: 142 × 2
    ##    country      mean
    ##    <fct>       <dbl>
    ##  1 Afghanistan  37.5
    ##  2 Albania      68.4
    ##  3 Algeria      59.0
    ##  4 Angola       37.9
    ##  5 Argentina    69.1
    ##  6 Australia    74.7
    ##  7 Austria      73.1
    ##  8 Bahrain      65.6
    ##  9 Bangladesh   49.8
    ## 10 Belgium      73.6
    ## # ℹ 132 more rows

Example3

``` r
#Finding the average GDP per capita by continent in the gapminder dataset
answer3.0 <-group_mean(gapminder, continent, gdpPercap)
print(answer3.0)
```

    ## # A tibble: 5 × 2
    ##   continent   mean
    ##   <fct>      <dbl>
    ## 1 Africa     2194.
    ## 2 Americas   7136.
    ## 3 Asia       7902.
    ## 4 Europe    14469.
    ## 5 Oceania   18622.

### Excerise 4

Testing the function

``` r
#I want to test that the function is running as expected and does not conduct an analysis on non-numeric variables. 
group_mean <-function(data, group_var, mean_var) {
  if(!is.numeric(data[[deparse(substitute(mean_var))]])) {
    stop('Sorry, this function requires a numeric variable')
    }
  data %>%
    group_by({{group_var}}) %>%
     summarise(mean = mean({{mean_var}}, na.rm = TRUE))
  }

test_that("Testing group_mean function", {
  expect_type(group_mean, "closure")
  expect_error(group_mean(data, group_var, (!is.numeric(data[[deparse(substitute(mean_var))]]))))
  expect_visible(group_mean)
 
})
```

    ## Test passed 😸
