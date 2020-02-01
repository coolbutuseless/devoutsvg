

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a 4-element rgba vector to hex RGB
#'
#' @param rgba_vec 4 element rgba with integer values [0, 255]
#'
#' @return colour hex triplet as string e.g. "#0156fe"
#' @importFrom grDevices rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgba_to_hex <- function(rgba_vec) {
  rgb(rgba_vec[1], rgba_vec[2], rgba_vec[3], names = NULL, maxColorValue = 255)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calcuate the alpha value from the 4-element RGBA vector
#'
#' @param rgba_vec 4 element rgba with integer values [0, 255]
#'
#' @return alpha in range [0,1]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgba_to_alpha <- function(rgba_vec) {
  round(rgba_vec[4]/255, 2)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Is the colour black?
#'
#' @param rgba_vec 4 element rgba with integer values [0, 255]
#'
#' @return TRUE if black
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_black <- function(rgba_vec) {
  rgba_vec[1] == 0 && rgba_vec[2] == 0 && rgba_vec[3] == 0
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Is the colour white?
#'
#' @param rgba_vec 4 element rgba with integer values [0, 255]
#'
#' @return TRUE if white
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_white <- function(rgba_vec) {
  rgba_vec[1] == 255 && rgba_vec[2] == 255 && rgba_vec[3] == 255
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Is the colour transparent?
#'
#' @param rgba_vec 4 element rgba with integer values [0, 255]
#'
#' @return TRUE if alpha == 0
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_transparent <- function(rgba_vec) {
  rgba_vec[4] == 0
}
