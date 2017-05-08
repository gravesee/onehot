
#' @useDynLib onehot
#' @importFrom Rcpp sourceCpp
NULL

make_names <- function(x) {

  addNA <- attr(x, "addNA")

  res <- lapply(x, function(i) {
    if (i$type == "character") {
      NULL
    } else if (i$type == "factor") {
      nm <- paste(i$name, i$levels, sep="=")
      if (addNA) nm <- c(nm, paste(i$name, "NA", sep="="))
      nm
    } else {
      nm <- i$name
      if (addNA) nm <- c(nm, paste(i$name, "NA", sep="="))
      nm
    }
  })

  unname(unlist(res))
}


##' Predict onehot objects
##' @param object an object of class \code{\link{onehot}}
##' @param data a data.frame to onehot encode useing \code{object}
##' @param ... further arguments passed to or from other methods
##' @return a matrix with factor variable onehot encoded
##' @examples
##' data(iris)
##' encoder <- onehot(iris)
##' x <- predict(encoder, iris)
##' @export
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

 s <- summary(object)
 dims <- c(nrow(data), s$ncols + s$nas)

 m <- predict_onehot_sparse(object, data[names(object)])
 result <- Matrix::sparseMatrix(i=m$i, j=m$j, x=m$x, dims = dims)


 colnames(result) <- make_names(object)
 result
}