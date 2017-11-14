#' Make column names for a onehot object
#' @param x a \code{\link{onehot}} object
#' @examples
#' data(iris)
#' encoder <- onehot(iris)
#' make_names(encoder$Species)
#' @export
make_names <- function(info) {

  with(info, switch(type,
    "factor"  = paste(name, levels, sep = "="),
    "numeric" = name,
    "default" = ""))
}


predict_column_info_factor_ <- function(info, x, sparse, ...) {

  i <- seq_along(x)
  j <- match(x, info$levels, nomatch = 0)
  f <- j > 0

  dims <-  c(length(x), length(info$levels))
  if (sparse) {
    result <- Matrix::sparseMatrix(i=i[f], j=j[f], x=1, dims = dims)
  } else {
    result <- matrix(0, nrow=dims[1], ncol = dims[2])
    result[cbind(i, j)[f,]] <- 1
  }

  result
}


predict_column_info_numeric_ <- function(info, x, sparse, ...) {

  i <- seq_along(x)
  j <- rep(1, length(x))
  f <- x != 0

  dims <-  c(length(x), 1L)
  if (sparse) {
    result <- Matrix::sparseMatrix(i=i[f], j=j[f], x=x[f], dims = dims)
  } else {
    result <- matrix(0, nrow=dims[1], ncol = dims[2])
    result[cbind(i, j)] <- x
  }

  result

}

predict.column_info <- function(object, data, sparse=FALSE, sentinel, add_NA_factors, ...) {

  if (is.factor(data)) {
    result <- predict_column_info_factor_(object, data, sparse, add_NA_factors, ...)
  } else {
    result <- predict_column_info_numeric_(object, data, sparse, sentinel, ...)
  }

  colnames(result) <- make_names(object)
  result
}


#' Predict onehot objects
#' @param object an object of class \code{\link{onehot}}
#' @param data a data.frame to onehot encode useing \code{object}
#' @param sparse if TRUE, returns a \code{\link[Matrix]{dgCMatrix-class}}
#' @param ... further arguments passed to or from other methods
#' @return a matrix with factor variable onehot encoded
#' @examples
#' data(iris)
#' encoder <- onehot(iris)
#' x <- predict(encoder, iris)
#' x_sparse <- predict(encoder, iris, sparse=TRUE)
#' @export
predict.onehot <- function(object, data, sparse=FALSE, ...) {

  sentinel <- attr(object, "sentinel")
  add_NA_factors <- attr(object, "add_NA_factors")

  data <- preprocess_data_(data, sentinel, add_NA_factors)

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

  result <- list()
  for (v in names(object)) {
    result[[v]] <- predict(object[[v]], data[[v]], sparse=sparse)
  }

  do.call(cbind, result)
}
