#' @useDynLib onehot
NULL

column_info <- function(x, name) {
  list(
    name = name,
    type = class(x),
    levels = levels(x))
}

make_names <- function(x) {
  res <- lapply(x, function(i) {
    if (i$type == "character") {
      NULL
    } else if (i$type == "factor") {
      paste(i$name, i$levels, sep="=")
    } else {
      i$name
    }
  })

  unname(unlist(res))
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

  types <- sapply(object, "[[", "type")
  ncols <- sapply(object, function(info) {
    switch(info$type,
      "factor" = length(info$levels),
      "numeric" = 1,
      "integer" = 1,
      "logical" = 1,
      0)
  })

  list(
    types=table(types),
    ncols=sum(ncols))

}


#' Print information about a onehot object
#' @param x onehot object to print
#' @param ... other arguments pass to or from other functions
#' @export
print.onehot <- function(x, ...) {
  s <- summary(x)
  cat("onehot object with following types:\n")
  cat(sprintf(" |- %3d %ss\n", s$types, names(s$types)), sep="")
  cat(sprintf("Producing matrix with %d columns\n", s$ncols))
}

#' Onehot encode a data.frame
#' @param data data.frame to convert factors into onehot encoded columns
#' @param stringsAsFactors if TRUE, converts character vectors to factors
#' @param addNA if TRUE, adds NA to factors as a level
#' @param max_levels maximum number of levels to onehot encode per factor
#' variable. Factors with levels exceeding this number will be skipped.
#' @return a \code{onehot} object descrbing how to transform the data
#' @examples
#' data(iris)
#' encoder <- onehot(iris)
#'
#' ## add NAs to factors
#' encoder <- onehot(iris, addNA=TRUE)
#'
#' ## Convert character fields to factrs
#' encoder <- onehot(iris, stringsAsFactors=TRUE)
#'
#' ## limit which factors are onehot encoded
#' encoder <- onehot(iris, max_levels=5)
#' @export
onehot <- function(data, stringsAsFactors=FALSE, addNA=FALSE, max_levels=10) {
  stopifnot(inherits(data, "data.frame"))

  if (stringsAsFactors) {
    for (i in seq_along(data)) {
      if (is.character(data[[i]])) data[[i]] <- factor(data[[i]])
    }
  }

  if (addNA) {
    for (i in seq_along(data)) {
      if (is.factor(data[[i]])) data[[i]] <- addNA(data[[i]])
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
  attr(res, "call") <- match.call()
  res
}

#' Predict onehot objects
#' @param object an object of class \code{\link{onehot}}
#' @param data a data.frame to onehot encode useing \code{object}
#' @param ... further arguments passed to or from other methods
#' @return a matrix with factor variable onehot encoded
#' @examples
#' data(iris)
#' encoder <- onehot(iris)
#' x <- predict(encoder, iris)
#' @export
predict.onehot <- function(object, data, ...) {

  ## check that vars are in data
  miss <- setdiff(names(object), names(data))
  if (length(miss)) {
    stop("onehot variables missing from data: ",
         paste(miss, collapse = ", "), call. = F)
  }

  ## make levels the same as the onehot object
  for (obj in object) {
    if (obj$type == "factor") {
      if (is.character(data[[obj$name]])) {
        data[[obj$name]] <- factor(data[[obj$name]])
      }
      attr(data[[obj$name]], "levels") <- obj$levels
    }
  }

  result <- c_predict_onehot(object, data)

  ## column names
  colnames(result) <- make_names(object)

  result
}

c_predict_onehot <- function(object, data) {
  .Call("predict_onehot", object, data[names(object)])
}

