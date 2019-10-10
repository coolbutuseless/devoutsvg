

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a 4-element rgba vector to hex RGB
#'
#' @param vec 4 element rgba with integer values [0, 255]
#'
#' @return colour hex triplet as string e.g. "#0156fe"
#' @importFrom grDevices rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vec2rgb <- function(vec) {
  rgb(vec[1], vec[2], vec[3], names = NULL, maxColorValue = 255)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calcuate the alpha value from the 4-element RGBA vector
#'
#' @param vec 4 element rgba with integer values [0, 255]
#'
#' @return alpha in range [0,1]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vec2alpha <- function(vec) {
  round(vec[4]/255, 2)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Is the colour black?
#'
#' @param vec 4 element rgba with integer values [0, 255]
#'
#' @return TRUE if black
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_black <- function(vec) {
  vec[1] == 0 && vec[2] == 0 && vec[3] == 0
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Is the colour white?
#'
#' @param vec 4 element rgba with integer values [0, 255]
#'
#' @return TRUE if white
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_white <- function(vec) {
  vec[1] == 255 && vec[2] == 255 && vec[3] == 255
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Is the colour transparent?
#'
#' @param vec 4 element rgba with integer values [0, 255]
#'
#' @return TRUE if alpha == 0
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_transparent <- function(vec) {
  vec[4] == 0
}
