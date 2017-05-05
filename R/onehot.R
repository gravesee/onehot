#' @useDynLib onehot
NULL

column_info <- function(x, name) {
  list(
    name = name,
    type = class(x),
    levels = levels(x))
}


#' @export
summary.onehot <- function(object, ...) {

  addNA <- attr(object, "addNA")

  types <- sapply(object, "[[", "type")
  ncols <- sapply(object, function(info) {
    switch(info$type,
      "factor" = length(info$levels),
      "numeric" = 1,
      "integer" = 1,
      "logcial" = 1,
      "default" = 0)
  })

  list(
    types=table(types),
    ncols=sum(ncols),
    NAcols=sum(ncols > 0))
}


#' @export
print.onehot <- function(x, ...) {
  s <- summary(x)
  cat("onehot object with following types:\n")
  cat(sprintf(" |- %3d %ss\n", s$types, names(s$types)), sep="")
  cat(sprintf("%d NA indicator columns\n", s$NAcols))
  cat(sprintf("Producing matrix with %d columns\n", s$ncols + s$NAcols))

}

#' Onehot encode a data.frame
#' @param data data.frame to convert factors into onehot encoded columns
#' @param addNA if TRUE, create indicator columns for all variables
#' @param stringsAsFactors if TRUE, converts character vectors to factors
#' @param max_levels maximum number of levels to onehot encode per factor
#' variable. Factors with levels exceeding this number will be skipped.
#' @param ... further arguments passed to or from other methods
#' @return a \code{onehot} object descrbing how to transform the data
#' @examples
#' data(iris)
#' encoder <- onehot(iris)
#'
#' ## add NAs indicator columns to all variables
#' encoder <- onehot(iris, addNA=TRUE)
#'
#' ## Convert character fields to factrs
#' encoder <- onehot(iris, stringsAsFactors=TRUE)
#'
#' ## limit which factors are onehot encoded
#' encoder <- onehot(iris, max_levels=5)
#' @export
onehot <- function(data, addNA=TRUE, stringsAsFactors=FALSE, max_levels=10, ...) {
  stopifnot(inherits(data, "data.frame"))

  if (stringsAsFactors) {
    for (i in seq_along(data)) {
      if (is.character(data[[i]])) data[[i]] <- factor(data[[i]], ...)
    }
  }

  nlevels <- sapply(data, function(x) length(levels(x)))
  f <- nlevels <= max_levels

  if (any(!f)) {
    n <- names(which(!f))
    warning(sprintf("Variables excluded for having levels > max_levels: %s", n),
      call. = F)
  }

  n <- names(data)[f]
  info <- Map(column_info, data[f], n)

  res <- structure(info, class = "onehot")
  attr(res, "addNA") <- addNA
  attr(res, "call") <- match.call()

  res
}
