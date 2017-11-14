#' @importFrom Matrix sparseMatrix
NULL

column_info <- function(x, name) {
  structure(list(
    name = name,
    type = class(x),
    levels = if (is.factor(x)) levels(x) else character(0)),
    class="column_info")
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

  types <- modifyList(list("factor"=0, "numeric"=0),
    as.list(table(sapply(object, "[[", "type"))))
  types <- unlist(types)

  ncols <- vapply(object, function(info) {
    switch(info$type,
      "factor" = length(info$levels),
      "numeric" = 1L,
      0L)
  }, FUN.VALUE = integer(1))

  nas <- sum(sapply(object, function(i) any(is.na(i$levels))))

  list(
    types=types,
    ncols=sum(ncols),
    nas=nas)

}


#' Print information about a onehot object
#' @param x onehot object to print
#' @param ... other arguments pass to or from other functions
#' @export
print.onehot <- function(x, ...) {
  add_NA_factors <- attr(x, "add_NA_factors")
  sentinel <- as.numeric(attr(x, "sentinel"))

  factor_lvls <-

  s <- summary(x)
  cat("Onehot object with following specification:\n")
  cat(sprintf(" |- %d columns", s$ncols - s$types["numeric"]), sep="")
  cat(sprintf(" from %d factor variables\n", s$types["factor"]), sep="")
  cat(sprintf(" |- %d numeric columns\n", s$types["numeric"]), sep="")
  cat(sprintf(" |- Producing matrix with %d columns\n", s$ncols), sep="")

  cat(sprintf(" |- Replace numeric-NAs with %.0f\n", sentinel), sep="")
if (is.logical(add_NA_factors) && add_NA_factors[1]) {
    cat(sprintf(" |- Adding %d NA indicators for ALL factors\n", s$nas), sep="")
  } else if (is.character(add_NA_factors)) {
    cat(sprintf("Adding %d NA indicators for:\n", s$nas), sep="")
    cat(sprintf("  %s\n", add_NA_factors), sep="")
  }

}



preprocess_data_ <- function(data, sentinel=.Machine$double.xmin, add_NA_factors=TRUE) {

  for (i in seq_along(data)) {

    if (is.character(data[[i]])) data[[i]] <- factor(data[[i]])

    if (!is.factor(data[[i]])) {
      data[[i]] <- as.numeric(data[[i]])
      data[[i]][is.na(data[[i]])] <- sentinel
    }

    ## proccess ALL factors for NA if logical is passed
    if (is.logical(add_NA_factors) && add_NA_factors[1]) {
      if (is.factor(data[[i]])) data[[i]] <- addNA(data[[i]])
    }
  }

  ## set NA factors if a character vector is passed
  if (is.character(add_NA_factors)) {
    for (v in add_NA_factors) {
      if (!is.null(data[[v]]) && is.factor(data[[v]])) {
        data[[v]] <- addNA(data[[v]])
      } else {
        warning(v, " is not in data or is not a factor.", call. = FALSE)
      }
    }
  }

  data
}

#' Onehot encode a data.frame
#' @param data data.frame to convert factors into onehot encoded columns
#' @param addNA if TRUE, all variables get an NA indicator
#' @param stringsAsFactors if TRUE, converts character vectors to factors
#' @param max_levels maximum number of levels to onehot encode per factor
#' variable. Factors with levels exceeding this number will be skipped.
#' @details By default, with \code{addNA=FALSE}, no NAs are returned for
#' non-factor columns. Indicator columns are created for factor levels and NA
#' factors are ignored. The exception is when NA is an explicit factor level.
#'
#' \code{stringsAsFactrs=TRUE} will convert character columns to factors first.
#' Other wise characters are ignored. Only factor, numeric, integer, and logical
#' vectors are valid for onehot. Other classes will be skipped entirely.
#'
#' \code{addNA=TRUE} will create indicator columns for every field. This will
#' add ncols columns to the output matrix. A sparse matrix may be better in
#' such cases.
#' @return a \code{onehot} object descrbing how to transform the data
#' @examples
#' data(iris)
#' encoder <- onehot(iris)
#'
#' ## add NA indicator columns
#' encoder <- onehot(iris, addNA=TRUE)
#'
#' ## Convert character fields to factrs
#' encoder <- onehot(iris, stringsAsFactors=TRUE)
#'
#' ## limit which factors are onehot encoded
#' encoder <- onehot(iris, max_levels=5)
#' @export
onehot <- function(data, sentinel=-999, max_levels=10, add_NA_factors=TRUE) {

  stopifnot(inherits(data, "data.frame"))

  ## Process data.frame according to arguments
  data <- preprocess_data_(data, sentinel=sentinel, add_NA_factors=add_NA_factors)

  nlevels <- sapply(data, function(x) length(levels(x)))
  f <- nlevels <= max_levels

  if (any(!f)) {
    n <- names(which(!f))
    warning(sprintf("Variables excluded for having levels > max_levels: %s", n),
      call. = F)
  }

  n <- names(data)[f]

  info <- Map(column_info, data[n], n)

  res <- structure(info, class = "onehot")
  attr(res, "call") <- match.call()
  attr(res, "sentinel") <- sentinel
  attr(res, "add_NA_factors") <- add_NA_factors
  res
}
