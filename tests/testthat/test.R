
library(onehot)
context("Encoder Creation")

test <- data.frame(
  integer = as.integer(runif(100) * 10),
  numeric = rnorm(100),
  factor  = factor(sample(letters[1:5], 100, TRUE)),
  logical = sample(c(TRUE, FALSE), 100, TRUE),
  complex = complex(100),
  character = sample(letters, 100, T),
  stringsAsFactors = FALSE)

test_that("Encoders of each variable type, addNA=FALSE", {

  expect_error(onehot(test["integer"]), NA)
  expect_error(onehot(test["numeric"]), NA)
  expect_error(onehot(test["factor"]), NA)
  expect_error(onehot(test["logical"]), NA)
  expect_error(onehot(test["complex"]), NA)
  expect_error(onehot(test["character"]), NA)

})

test_that("Encoders of each variable type, addNA=TRUE", {

  expect_error(onehot(test["integer"], addNA=TRUE), NA)
  expect_error(onehot(test["numeric"], addNA=TRUE), NA)
  expect_error(onehot(test["factor"], addNA=TRUE), NA)
  expect_error(onehot(test["logical"], addNA=TRUE), NA)
  expect_error(onehot(test["complex"], addNA=TRUE), NA)
  expect_error(onehot(test["character"], addNA=TRUE), NA)

})

