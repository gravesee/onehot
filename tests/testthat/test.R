
library(onehot)

# context("Encoder Creation")

test <- data.frame(
  integer = as.integer(runif(100) * 10),
  numeric = rnorm(100),
  factor  = factor(sample(letters[1:5], 100, TRUE)),
  logical = sample(c(TRUE, FALSE), 100, TRUE),
  complex = complex(100),
  character = sample(letters[1:7], 100, T),
  stringsAsFactors = FALSE)

test_that("Number of columns dense", {

  expect_equal(ncol(predict(onehot(test["integer"]), test)), 1L)
  expect_equal(ncol(predict(onehot(test["numeric"]), test)), 1L)
  expect_equal(ncol(predict(onehot(test["factor"]), test)), length(levels(test$factor)) + 1L)
  expect_equal(ncol(predict(onehot(test["logical"]), test)), 1L)
  expect_equal(ncol(predict(onehot(test["complex"]), test)), 1L)
  expect_equal(ncol(predict(onehot(test["character"]), test)), length(unique(test$character)) + 1L)

  expect_equal(ncol(predict(onehot(test), test)),
    sum(1L, 1L, length(levels(test$factor)) + 1L, 1L, 1L, length(unique(test$character)) + 1L))

})

test_that("Number of columns dense with add_NA_factors=FALSE", {

  expect_equal(ncol(predict(onehot(test["integer"], add_NA_factors=FALSE), test)), 1L)
  expect_equal(ncol(predict(onehot(test["numeric"], add_NA_factors=FALSE), test)), 1L)
  expect_equal(ncol(predict(onehot(test["factor"], add_NA_factors=FALSE), test)), length(levels(test$factor)))
  expect_equal(ncol(predict(onehot(test["logical"], add_NA_factors=FALSE), test)), 1L)
  expect_equal(ncol(predict(onehot(test["complex"], add_NA_factors=FALSE), test)), 1L)
  expect_equal(ncol(predict(onehot(test["character"], add_NA_factors=FALSE), test)), length(unique(test$character)))

  expect_equal(ncol(predict(onehot(test, add_NA_factors=FALSE), test)),
    sum(1L, 1L, length(levels(test$factor)), 1L, 1L, length(unique(test$character))))


})

test_that("Number of columns sparse", {

  expect_equal(ncol(predict(onehot(test["integer"]), test, sparse=TRUE)), 1L)
  expect_equal(ncol(predict(onehot(test["numeric"]), test, sparse=TRUE)), 1L)
  expect_equal(ncol(predict(onehot(test["factor"]), test, sparse=TRUE)), length(levels(test$factor)) + 1L)
  expect_equal(ncol(predict(onehot(test["logical"]), test, sparse=TRUE)), 1L)
  expect_equal(ncol(predict(onehot(test["complex"]), test, sparse=TRUE)), 1L)
  expect_equal(ncol(predict(onehot(test["character"]), test, sparse=TRUE)), length(unique(test$character)) + 1L)


  expect_equal(ncol(predict(onehot(test), test, sparse=TRUE)),
    sum(1L, 1L, length(levels(test$factor)) + 1L, 1L, 1L, length(unique(test$character)) + 1L))

})

test_that("Number of columns sparse & add_NA_factors=FALSE", {

  expect_equal(ncol(predict(onehot(test["integer"], add_NA_factors=FALSE), test, sparse=TRUE)), 1L)
  expect_equal(ncol(predict(onehot(test["numeric"], add_NA_factors=FALSE), test, sparse=TRUE)), 1L)
  expect_equal(ncol(predict(onehot(test["factor"], add_NA_factors=FALSE), test, sparse=TRUE)), length(levels(test$factor)))
  expect_equal(ncol(predict(onehot(test["logical"], add_NA_factors=FALSE), test, sparse=TRUE)), 1L)
  expect_equal(ncol(predict(onehot(test["complex"], add_NA_factors=FALSE), test, sparse=TRUE)), 1L)
  expect_equal(ncol(predict(onehot(test["character"], add_NA_factors=FALSE), test, sparse=TRUE)), length(unique(test$character)))

  expect_equal(ncol(predict(onehot(test, add_NA_factors=FALSE), test, sparse=TRUE)),
    sum(1L, 1L, length(levels(test$factor)), 1L, 1L, length(unique(test$character))))

})
