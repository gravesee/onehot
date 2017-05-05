make_names <- function(x) {

  addNA <- attr(x, "addNA")

  res <- lapply(x, function(i) {
    if (i$type == "character") {
      NULL
    } else if (i$type == "factor") {
      x <- paste(i$name, i$levels, sep="=")
      if (addNA) c(x, paste(i$name, "NA", sep="=")) else x
    } else {
      x <- i$name
      if (addNA) c(x, paste(i$name, "NA", sep="=")) else x
    }
  })

  unname(unlist(res))
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

