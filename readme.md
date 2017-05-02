[![Travis-CI Build
Status](https://travis-ci.org/Zelazny7/onehot.svg?branch=master)](https://travis-ci.org/Zelazny7/onehot)
![](http://cranlogs.r-pkg.org/badges/grand-total/onehot)

Onehot package
--------------

### Installation

    devtools::install_github("https://github.com/Zelazny7/onehot")

### Usage

    set.seed(100)
    test <- data.frame(
      factor    = factor(sample(c(NA, letters[1:3]), 100, T), exclude=NULL),
      integer   = as.integer(runif(100)),
      real      = rnorm(100),
      logical   = sample(c(T, F), 100, T),
      character = sample(letters, 100, T),
      stringsAsFactors = FALSE)

    head(test)

    ##   factor integer       real logical character
    ## 1      a       0 -0.3329234   FALSE         f
    ## 2      a       0  1.3631137    TRUE         t
    ## 3      b       0 -0.4691473    TRUE         h
    ## 4   <NA>       0  0.8428756    TRUE         k
    ## 5      a       0 -1.4579937   FALSE         k
    ## 6      a       0 -0.4003059   FALSE         l

### Create a onehot object

A onehot object contains information about the data.frame. This is used
to transform a data.frame into a onehot encoded matrix. It should be
saved to transform future datasets into the same exact layout.

    library(onehot)
    encoder <- onehot(test)

    ## printe a summary
    encoder

    ## onehot object with following types:
    ##  |-   1 characters
    ##  |-   1 factors
    ##  |-   1 integers
    ##  |-   1 logicals
    ##  |-   1 numerics
    ## Producing matrix with 7 columns

### Transforming data.frames

The onehot object has a predict method which may be used to transform a
data.frame. Factors are onehot encoded. Character variables are skipped.
However calling predict with `stringsAsFactors=TRUE` will convert
character vectors to factors first.

    train_data <- predict(encoder, test)
    head(train_data)

    ##      factor=a factor=b factor=c factor=NA integer       real logical
    ## [1,]        1        0        0         0       0 -0.3329234       0
    ## [2,]        1        0        0         0       0  1.3631137       1
    ## [3,]        0        1        0         0       0 -0.4691473       1
    ## [4,]        0        0        0         1       0  0.8428756       1
    ## [5,]        1        0        0         0       0 -1.4579937       0
    ## [6,]        1        0        0         0       0 -0.4003059       0
