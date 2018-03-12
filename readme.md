[![Travis-CI Build
Status](https://travis-ci.org/Zelazny7/onehot.svg?branch=master)](https://travis-ci.org/Zelazny7/onehot)

Onehot package
--------------

### Installation

    devtools::install_github("https://github.com/Zelazny7/onehot")

### Usage

    set.seed(100)
    test <- data.frame(
      factor    = factor(sample(c(NA, letters[1:3]), 100, T)),
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

    ## Warning: Variables excluded for having levels > max_levels: character

    ## printe a summary
    encoder

    ## Onehot Specification
    ## |-   1 Factors  => 4 Indicators 
    ## |-   3 Numerics => (NA <- -999)

### Transforming data.frames

The onehot object has a predict method which may be used to transform a
data.frame. Factors are onehot encoded. Character variables are skipped.
However calling predict with `stringsAsFactors=TRUE` will convert
character vectors to factors first.

    train_data <- predict(encoder, test)
    head(train_data)

    ##      factor_a factor_b factor_c factor_NA integer       real logical
    ## [1,]        1        0        0         0       3 -0.3329234       0
    ## [2,]        1        0        0         0       3  1.3631137       1
    ## [3,]        0        1        0         0       0 -0.4691473       1
    ## [4,]        0        0        0         1       3  0.8428756       1
    ## [5,]        1        0        0         0       5 -1.4579937       0
    ## [6,]        1        0        0         0       6 -0.4003059       0

### NA indicator columns

`add_NA_factors=TRUE` (the default) will create an indicator column for
every factor column. Having NAs as a factor level will result in an
indicator column being created without using this option.

    encoder <- onehot(test, add_NA_factors=TRUE)

    ## Warning: Variables excluded for having levels > max_levels: character

    train_data <- predict(encoder, test)
    head(train_data)

    ##      factor_a factor_b factor_c factor_NA integer       real logical
    ## [1,]        1        0        0         0       3 -0.3329234       0
    ## [2,]        1        0        0         0       3  1.3631137       1
    ## [3,]        0        1        0         0       0 -0.4691473       1
    ## [4,]        0        0        0         1       3  0.8428756       1
    ## [5,]        1        0        0         0       5 -1.4579937       0
    ## [6,]        1        0        0         0       6 -0.4003059       0

### Sentinel values for numeric columns

The `sentinel=VALUE` argument will replace all numeric NAs with the
provided value. Some ML algorithms such as `randomForest` and `xgboost`
do not handle NA values. However, by using sentinel values such
algorithms are usually able to separate them with enough decision-tree
splits. The default value is `-999`

### Sparse Matrices

`onehot` also provides support for predicting sparse, column compressed
matrices from the `Matrix` package:

    encoder <- onehot(test)

    ## Warning: Variables excluded for having levels > max_levels: character

    train_data <- predict(encoder, test, sparse=TRUE)
    head(train_data)

    ## 6 x 7 sparse Matrix of class "dgCMatrix"
    ##      factor_a factor_b factor_c factor_NA integer       real logical
    ## [1,]        1        .        .         .       3 -0.3329234       .
    ## [2,]        1        .        .         .       3  1.3631137       1
    ## [3,]        .        1        .         .       . -0.4691473       1
    ## [4,]        .        .        .         1       3  0.8428756       1
    ## [5,]        1        .        .         .       5 -1.4579937       .
    ## [6,]        1        .        .         .       6 -0.4003059       .
