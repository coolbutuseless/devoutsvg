

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Construct a vector of styles related to 'stroke'
#'
#' Note: 1 lwd = 1/96", but units in rest of document are 1/72"
#'
#' @param state list including 'gc' (graphics context)
#'
#' @return character vector of multiple "{attr_name}: {value}" strings for
#'         'stroke', 'stroke-width', 'stroke-opacity'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stroke_style <- function(state) {
  gc <- state$gc

  style <- c()

  if (is_transparent(gc$col)) {
    return("stroke: none")
  }

  if (!is_black(gc$col)) {
    style <- c(style, glue_two("stroke: {vec2rgb(gc$col)}"))
  }

  style <- c(style, glue_two("stroke-width: {round(gc$lwd / 96.0 * 72, 2)}"))

  if (vec2alpha(gc$col) != 1) {
    style <- c(style, glue_two("stroke-opacity: {vec2alpha(gc$col)}"))
  }

  style
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Construct a vector of styles related to 'font'
#'
#' TODO: add font choice and style
#'
#' @param state list including 'gc' (graphics context)
#'
#' @return character vector of multiple "{attr_name}: {value}" strings for
#'         'font-size', 'font-family', 'fill'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
font_style <- function(state) {

  gc <- state$gc

  # fontsize <- gc$cex * gc$ps
  font <- get_font_info(state)

  style <- c(
    glue_two("font-size: {font$size}px"),
    glue("font-family: {font$family}")
  )

  if (!is_black(gc$col)) {
    style <- c(style, glue_two("fill: {vec2rgb(gc$col)}"))
  }

  style
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Construct a vector of styles related to 'fill'
#'
#' @param state list including 'gc' (graphics context)
#' @param use_pattern_fill whether or not to use the pattern fill for this style
#'
#' @return character vector of multiple "{attr_name}: {value}" strings for 'fill',
#'         'fill-opacity'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fill_style <- function(state, use_pattern_fill) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Setup
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gc          <- state$gc
  fill        <- gc$fill

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Work out if both the following are true:
  #   1. the device wants to use a pattern fill for this
  #   2. the pattern package can create a pattern for this
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (use_pattern_fill) {
    pattern_pkg               <- state$rdata$pattern_pkg
    is_valid_pattern_encoding <- get('is_valid_pattern_encoding', asNamespace(pattern_pkg))
    pattern_ok                <- is_valid_pattern_encoding(fill)
  } else {
    pattern_ok <- FALSE
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the svg 'fill' style attribute to either
  #  1. point to a pattern (if wanted and available
  #  2. include the full hex-colour
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (pattern_ok) {
    pattern_id <- create_pattern_id_from_rgba_vec(pattern_pkg, fill)
    style      <- glue_two("fill: url(#{pattern_id})")
  } else {
    style <- glue_two("fill: {vec2rgb(fill)}")

    if (vec2alpha(gc$fill) != 1) {
      style <- c(style, glue_two("fill-opacity: {vec2alpha(fill)}"))
    }
  }


  style
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create stryle strings for the given attritute or set of attributes
#'
#' @param attr_name attribute name of name for a set of attributes
#' @param state list including 'gc' (graphics context)
#' @param use_pattern_fill default FALSE
#'
#' @return character vector of one or more "{attr_name}: {value}" strings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_style_string <- function(attr_name, state, use_pattern_fill = FALSE) {
  switch(
    attr_name,
    "fill"           = fill_style  (state, use_pattern_fill = use_pattern_fill),
    "stroke"         = stroke_style(state),
    "font"           = font_style  (state)
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create CSS styles for the given attributes
#'
#' @param attr_names character vector of e.g 'fill', 'stroke', 'font'
#' @param state list including 'gc' (graphics context)
#' @param use_pattern_fill default: FALSE
#'
#' @return style string ready for inclusion in an SVG tag e.g.
#'         "style='stroke: #ffffff; fill: #000000;'"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stylecss <- function(attr_names, state, use_pattern_fill = FALSE) {
  if (length(attr_names) == 0) { return('') }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the individual style strings for all the attributes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  styles <- lapply(attr_names, create_style_string, state, use_pattern_fill = use_pattern_fill)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Collapse them into a single string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  styles <- paste(unlist(styles), collapse = "; ")

  glue_two("style='{styles};'")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create CSS styles for the given attributes
#'
#' @param attr_names character vector of e.g 'fill', 'stroke', 'font'
#' @param state list including 'gc' (graphics context)
#' @param use_pattern_fill default: FALSE
#'
#' @return style string ready for inclusion in an SVG tag e.g.
#'         "stroke: #ffffff; fill: #000000;"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
style_string <- function(attr_names, state, use_pattern_fill = FALSE) {
  if (length(attr_names) == 0) { return(NULL) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the individual style strings for all the attributes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  styles <- lapply(attr_names, create_style_string, state, use_pattern_fill = use_pattern_fill)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Collapse them into a single string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  styles <- paste(unlist(styles), collapse = "; ")

  styles
}

