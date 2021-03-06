
<!-- README.md is generated from README.Rmd. Please edit that file -->

# WordsChars

<!-- badges: start -->
<!-- badges: end -->

The goal of WordsChars is to count the number of characters and words in
a given string. You can decide weather you want to count spaces as
character or not. When given a string to count, you will get a vector in
return whose first element is the number of characters and second
element is the number of words.

## Installation

WordsChars is not yet in CRAN. But, you can install the development
version of WordsChars like so:

``` r
devtools::install_github("jburckhardt/WordsChars")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(WordsChars)
count_chr_words("Hello World", count_spaces = TRUE)
#> [1] "Number of words in string: 2"
#> [1] "Number of characters in string: 11"
#> [1]  2 11
```

This package also allows you to **not** count spaces as characters!

``` r
count_chr_words("Hello World", count_spaces = FALSE)
#> [1] "Number of words in string: 2"
#> [1] "Number of characters in string: 10"
#> [1]  2 10
```
