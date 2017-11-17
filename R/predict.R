#' @export
preprocess <- function(oh, x) UseMethod("preprocess")

#' @export
preprocess.column_info_factor <- function(oh, x, ...) {

  x <- factor(as.character(x))
  if (any(is.na(oh$levels))) {
    x <- addNA(x)
  }

  levels(x) <- oh$levels
  x
}

#' @export
preprocess.column_info_numeric <- function(oh, x, ...) {
  x <- as.numeric(x)
  x[is.na(x)] <- oh$sentinel
  x
}

#' @export
preprocess.onehot <- function(oh, x) {
  i <- names(oh)

  ## check all variables are in the data
  if (!all(i %in% names(x))) stop("Variables in onehot object not found in `data`")

  res <- mapply(preprocess, oh, x[i], SIMPLIFY = FALSE)
  data.frame(res)
}


#' Make column names for a onehot object
#' @param x a \code{\link{onehot}} object
#' @examples
#' data(iris)
#' encoder <- onehot(iris)
#' make_names(encoder$Species)
#' @export
make_names <- function(info, sep) {

  with(info, switch(type,
    "factor"  = paste(name, levels, sep = sep),
    "numeric" = name,
    "default" = ""))
}

#' @export
predict.column_info_factor <- function(info, x, sparse, ...) {

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

#' @export
predict.column_info_numeric <- function(info, x, sparse, ...) {

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
predict.onehot <- function(object, data, sparse=FALSE, sep="_", ...) {
  data <- preprocess(object, data)
  res <- do.call(cbind, mapply(predict, object, data, sparse=sparse, sep=sep, SIMPLIFY = FALSE))
  colnames(res) <- unlist(lapply(object, make_names, sep=sep), use.names = F)
  res
}

#' #' @export
#' `[.onehot` <- function(x, i, j, ...) {
#'
#'   rng <- abs(range(i))
#'   stopifnot(rng[1] > 0 && rng[2] <= length(x))
#'
#'   a <- attributes(x)
#'   res <- unclass(x)[i]
#'
#'   attr(res, "names") <- a$names[i]
#'   attr(res, "call") <- a$call
#'   attr(res, "sentinel") <- a$sentinel
#'   attr(res, "add_NA_factors") <- a$add_NA_factors
#'
#'   class(res) <- "onehot"
#'
#'   res
#' }
