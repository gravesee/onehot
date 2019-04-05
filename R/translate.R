## functions to translate rules into other language code
level_for_sas_name <- function(lvl) {
  lvl <- gsub("^-(\\d+)$", "neg\\1", lvl)
  lvl <- gsub("[^A-Za-z0-9]", "_", lvl) # replace other strange characters with _
  lvl
}

#' Generate SAS code for onehot object
#' @param x a \code{onehot} object
#' @param sep a character vector used to separate the name of a factor from the
#' value.
#' @return Returns a character vector of SAS code that can be written to file
#' useing \code{writeLines}
#' @export
sas <- function(x, sep="_", ...) UseMethod("sas")

#' @export
sas.onehot <- function(x, sep="_", ...) {

  sentinel <- attr(x, "sentinel")

  c("*****************************************;",
     "*** Begin SAS onehot variable mapping ***;",
     "*****************************************;",

    do.call(c, lapply(x, sas, sep, sentinel)),

    "*****************************************;",
    "***  End SAS onehot variable mapping  ***;",
    "*****************************************;")

}

#' @export
sas_onehot_factor <- function(x, sep) {

  ifelse(is.na(x$levels),
    sprintf("%s%s%s = missing(%s);", x$name, sep, "NA", x$name),
    sprintf("%s%s%s = (%s = \"%s\");", x$name, sep, level_for_sas_name(x$levels), x$name, x$levels))

}

#' @export
sas_onehot_numeric <- function(x, sentinel) {

  ifelse(is.na(sentinel),
    sprintf("if missing(%s) then %s = .;", x$name, x$name),
    sprintf("if missing(%s) then %s = %.0f;", x$name, x$name, sentinel))

}

#' @export
sas.column_info <- function(x, sep="_", sentinel, ...) {
  switch(x$type,
    "factor" = sas_onehot_factor(x, sep),
    "numeric" = sas_onehot_numeric(x, sentinel),
    "default" = stop("Not Implemented"))
}