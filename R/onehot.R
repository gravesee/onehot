#' @useDynLib onehot
NULL

allowed_classes <- c("integer", "numeric", "factor", "logical")

column_info <- function(x, name) {
  list(
    name = name,
    type = class(x),
    levels = levels(x))
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

  addNA <- attr(object, "addNA")

  types <- sapply(object, "[[", "type")
  ncols <- vapply(object, function(info) {
    switch(info$type,
      "factor" = length(info$levels),
      "numeric" = 1L,
      "integer" = 1L,
      "logical" = 1L,
      0L)
  }, FUN.VALUE = integer(1))

  list(
    types=table(types),
    ncols=sum(ncols),
    nas=if (addNA) length(object) else 0)

}


#' Print information about a onehot object
#' @param x onehot object to print
#' @param ... other arguments pass to or from other functions
#' @export
print.onehot <- function(x, ...) {
  addNA <- attr(x, "addNA")
  s <- summary(x)
  cat("onehot object with following types:\n")
  cat(sprintf(" |- %3d %ss\n", s$types, names(s$types)), sep="")
  if (addNA) cat(sprintf(" |- %3d NA indicators\n", s$nas), sep="")
  cat(sprintf("Producing matrix with %d columns\n", s$ncols + s$nas))
}


#' Onehot encode a data.frame
#' @param data data.frame to convert factors into onehot encoded columns
#' @param addNA if TRUE, all variables get an NA indicator
#' @param stringsAsFactors if TRUE, converts character vectors to factors
#' @param max_levels maximum number of levels to onehot encode per factor
#' variable. Factors with levels exceeding this number will be skipped.
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
onehot <- function(data, addNA=FALSE, stringsAsFactors=FALSE, max_levels=10) {
  stopifnot(inherits(data, "data.frame"))

  if (stringsAsFactors) {
    for (i in seq_along(data)) {
      if (is.character(data[[i]])) data[[i]] <- factor(data[[i]])
    }
  }

  nlevels <- sapply(data, function(x) length(levels(x)))
  f <- nlevels <= max_levels

  if (any(!f)) {
    n <- names(which(!f))
    warning(sprintf("Variables excluded for having levels > max_levels: %s", n),
      call. = F)
  }

  k <- sapply(data, class) %in% allowed_classes
  n <- names(data)[f & k]

  info <- Map(column_info, data[n], n)

  res <- structure(info, class = "onehot")
  attr(res, "call") <- match.call()
  attr(res, "addNA") <- addNA
  res
}