
col_info <- function(x, name) {
  list(
    name = name,
    type = class(x),
    levels = levels(x))
}

#' @export
onehot <- function(d, stringsAsFactors=FALSE) {
  stopifnot(inherits(d, "data.frame"))

  if (stringsAsFactors) {
    for (i in seq_along(d)) {
      if (is.character(d[[i]])) d[[i]] <- factor(d[[i]])
    }
  }

  n <- names(d)
  info <- Map(col_info, d, n)

  structure(info, class = "onehot")
}

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
      attr(data[[obj$name]], "levels") <- obj$levels
    }
  }

  c_predict_onehot(object, data)
}

#' @export
c_predict_onehot <- function(object, data) {
  .Call("predict_onehot", object, data[names(object)])
}

