#' @importFrom Matrix sparseMatrix
NULL

#' @export
column_info <- function(name, type, ...) {
  res <- structure(list(name=name, type=type), class="column_info")
  switch(type,
    "numeric" = column_info_numeric(res, ...),
    "factor"  = column_info_factor(res, ...),
    "default" = stop("Unsupported column info type"))
}

#' @export
column_info_numeric <- function(x, sentinel) {
  x$sentinel <- sentinel
  class(x) <- c(class(x), "column_info_numeric")
  x
}

#' @export
column_info_factor <- function(x, levels) {
  x$levels <- levels
  class(x) <- c(class(x), "column_info_factor")
  x
}

#' @export
summary.column_info_factor <- function(object, ...) {
  with(object, c(type=2, ncol=length(levels), nas=sum(is.na(levels))))
}

#' @export
summary.column_info_numeric <- function(object, ...) {
  with(object, c(type=1, ncol=1, nas=0))
}

#' Summarize onehot object
#' @param object a onehot object
#' @param ... other arguments pass to or from other functions
#' @examples
#' ## Create some dummy data with different column types
#' x <- data.frame(HairEyeColor)
#' x$Hair <- as.character(x$Hair)
#'
#' ## Create a onehot object
#' encoder <- onehot(x)
#'
#' ## Return a list with summary information
#' summary(encoder)
#' @export
summary.onehot <- function(object, ...) {
  t(sapply(object, summary))
}


#' Print information about a onehot object
#' @param x onehot object to print
#' @param ... other arguments pass to or from other functions
#' @export
print.onehot <- function(x, ...) {

  sentinel <- attr(x, "sentinel")
  s <- summary(x)
  nf <- sum(s[,'type'] == 2)
  nn <- sum(s[,'type'] == 1)
  ni <- sum(s[s[,'type'] == 2, 'ncol'])

  cat(
    "Onehot Specification",
    sprintf("|- %3d Factors  => %d Indicators ", nf, ni),
    sprintf("|- %3d Numerics => (NA <- %.0f)", nn, sentinel),
    sep="\n")

}

get_column_info_ <- function(data, add_NA_factors, sentinel) {

  res <- list()

  for (i in seq_along(data)) {
    name <- names(data[i])
    x <- data[[i]]

    if (!(is.factor(x) || is.character(x))) {
      res[[i]] <- column_info(name, "numeric", sentinel=sentinel)
    } else {

      if (add_NA_factors) {
        res[[i]] <- column_info(name, "factor", levels=levels(addNA(factor(x))))
      } else {
        res[[i]] <- column_info(name, "factor", levels=levels(factor(x)))
      }
    }
  }

  names(res) <- names(data)
  res
}


#' Onehot Encode a data.frame
#'
#' @param data data.frame to convert factors into onehot encoded columns
#' @param sentinel Numeric value with which to replace NAs. Applies to numeric
#' columns only.
#' @param max_levels maximum number of levels to onehot encode per factor
#' variable. Factors with levels exceeding this number will be skipped.
#' @param add_NA_factors if TRUE, adds NA indicator column for factors.
#'
#' @return a \code{onehot} object descrbing how to transform the data
#' @examples
#' data(iris)
#' encoder <- onehot(iris)
#'
#' ## add NA indicator columns
#' encoder <- onehot(iris, add_NA_factors=TRUE)
#'
#' ## limit which factors are onehot encoded
#' encoder <- onehot(iris, max_levels=5)
#'
#' ## Impute numeric NA values with sentinel value
#' encoder <- onehot(iris, sentinel=-1)
#' @export
onehot <- function(data, sentinel=-999, max_levels=10, add_NA_factors=TRUE) {

  stopifnot(inherits(data, "data.frame"))

  info <- get_column_info_(data, add_NA_factors, sentinel)

  nlevels <- sapply(info, function(x) length(x$levels))
  f <- nlevels <= max_levels

  if (any(!f)) {
    n <- names(which(!f))
    warning(sprintf("Variables excluded for having levels > max_levels: %s", n),
      call. = F)
  }

  res <- structure(info[f], class = "onehot")
  attr(res, "call") <- match.call()
  attr(res, "sentinel") <- sentinel
  attr(res, "add_NA_factors") <- add_NA_factors
  res
}
