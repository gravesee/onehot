[![Travis-CI Build
Status](https://travis-ci.org/Zelazny7/onehot.svg?branch=master)](https://travis-ci.org/Zelazny7/onehot)

Onehot package
--------------

### Installation

    devtools::install_github("https://github.com/Zelazny7/onehot")

### Usage

    set.seed(100)
    test <- data.frame(
      factor    = factor(sample(c(NA, letters[1:3]), 100, T), exclude=NULL),
      integer   = as.integer(runif(100) * 10),
      real      = rnorm(100),
      logical   = sample(c(T, F), 100, T),
      character = sample(letters, 100, T),
      stringsAsFactors = FALSE)

    head(test)

    ##   factor integer       real logical character
    ## 1      a       3 -0.3329234   FALSE         f
    ## 2      a       3  1.3631137    TRUE         t
    ## 3      b       0 -0.4691473    TRUE         h
    ## 4   <NA>       3  0.8428756    TRUE         k
    ## 5      a       5 -1.4579937   FALSE         k
    ## 6      a       6 -0.4003059   FALSE         l

### Create a onehot object

A onehot object contains information about the data.frame. This is used
to transform a data.frame into a onehot encoded matrix. It should be
saved to transform future datasets into the same exact layout.

    library(onehot)

    ## Loading required package: Matrix

    encoder <- onehot(test)

    ## Warning: Variables excluded for having unsupported types: character

    ## printe a summary
    encoder

    ## onehot object with following types:
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
    ## [1,]        1        0        0         0       3 -0.3329234       0
    ## [2,]        1        0        0         0       3  1.3631137       1
    ## [3,]        0        1        0         0       0 -0.4691473       1
    ## [4,]        0        0        0         1       3  0.8428756       1
    ## [5,]        1        0        0         0       5 -1.4579937       0
    ## [6,]        1        0        0         0       6 -0.4003059       0

### NA indicator columns

`addNA=TRUE` will create an indicator column for every valid input
column. This is useful for algorithms that do not support missing
values.

    encoder <- onehot(test, addNA=TRUE)

    ## Warning: Variables excluded for having unsupported types: character

    train_data <- predict(encoder, test)
    head(train_data)

    ##      factor=a factor=b factor=c factor=NA factor=NA integer integer=NA       real real=NA logical logical=NA
    ## [1,]        1        0        0         0         0       3          0 -0.3329234       0       0          0
    ## [2,]        1        0        0         0         0       3          0  1.3631137       0       1          0
    ## [3,]        0        1        0         0         0       0          0 -0.4691473       0       1          0
    ## [4,]        0        0        0         1         0       3          0  0.8428756       0       1          0
    ## [5,]        1        0        0         0         0       5          0 -1.4579937       0       0          0
    ## [6,]        1        0        0         0         0       6          0 -0.4003059       0       0          0

### Sparse Matrices

`onehot` also provides support for predicting sparse, column compressed
matrices from the `Matrix` package:

    encoder <- onehot(test)

    ## Warning: Variables excluded for having unsupported types: character

    train_data <- predict(encoder, test, sparse=TRUE)
    head(train_data)

    ## 6 x 7 sparse Matrix of class "dgCMatrix"
    ##      factor=a factor=b factor=c factor=NA integer       real logical
    ## [1,]        1        .        .         .       3 -0.3329234       .
    ## [2,]        1        .        .         .       3  1.3631137       1
    ## [3,]        .        1        .         .       . -0.4691473       1
    ## [4,]        .        .        .         1       3  0.8428756       1
    ## [5,]        1        .        .         .       5 -1.4579937       .
    ## [6,]        1        .        .         .       6 -0.4003059       .
