
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Transformer for \code{glue} to round numeric values to decimal places and convert NULL to text
#'
#' @param text,envir standard transformer arguments
#'
#' @return evaluated text value
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
transformer_round <- function(text, envir) {
  if (is.null(text)) {
    return("NULL")
  }

  res <- eval(parse(text = text, keep.source = FALSE), envir)

  if (is.double(res)) {
    res <- round(res, 2)
  }

  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Custom version of \code{glue} which rounds all numerics to 2 decimals
#'
#' @param ...,.envir standard glue args
#'
#' @return interpolated string
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
glue_two <- function(..., .envir = parent.frame()) {
  glue(..., .envir = .envir, .transformer = transformer_round)
}



'%||%' <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}