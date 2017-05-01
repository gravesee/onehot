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
    if (i$type == "factor") {
      paste(i$name, i$levels, sep="=")
    } else {
      i$name
    }
  })

  unname(unlist(res))
}

#' Onehot encode a data.frame
#' @param d data.frame to convert factors into onehot encoded columns
#' @param stringsAsFactors if TRUE, converts character vectors to factors
#' @param addNA if TRUE, adds NA to factors as a level
#' @return a \code{onehot} object descrbing how to transform the data
#' @export
onehot <- function(data, stringsAsFactors=FALSE, addNA=FALSE) {
  stopifnot(inherits(data, "data.frame"))

  if (stringsAsFactors) {
    for (i in seq_along(data)) {
      if (is.character(data[[i]])) data[[i]] <- factor(data[[i]])
    }

    if (addNA) {
      if (is.factor(data[[i]])) data[[i]] <- addNA(data[[i]])
    }
  }

  n <- names(d)
  info <- Map(column_info, d, n)

  res <- structure(info, class = "onehot")
  attr(res, "call") <- match.call()
  res
}

#' Predict onehot objects
#' @param object an object of class \code{\link{onehot}}
#' @param data a data.frame to onehot encode useing \code{object}
#' @return a matrix with factor variable onehot encoded
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

