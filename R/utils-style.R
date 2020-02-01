

geoms_with_pattern_fill <- c('rect', 'polygon', 'path', 'circle')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Construct a vector of styles related to 'stroke'
#'
#' Note: 1 lwd = 1/96", but units in rest of document are 1/72"
#'
#' @param state list including 'gc' (graphics context)
#' @param geom which geometry has asked for a style
#'
#' @return character vector of multiple "{attr_name}: {value}" strings for
#'         'stroke', 'stroke-width', 'stroke-opacity'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stroke_style <- function(state, geom) {
  gc <- state$gc

  style <- c()

  if (is_transparent(gc$col)) {
    return("stroke: none")
  }

  if (!is_black(gc$col)) {
    style <- c(style, glue_two("stroke: {rgba_to_hex(gc$col)}"))
  }

  style <- c(style, glue_two("stroke-width: {round(gc$lwd / 96.0 * 72, 2)}"))

  if (rgba_to_alpha(gc$col) != 1) {
    style <- c(style, glue_two("stroke-opacity: {rgba_to_alpha(gc$col)}"))
  }

  style
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Construct a vector of styles related to 'font'
#'
#' TODO: add font choice and style
#'
#' @inheritParams stroke_style
#'
#' @return character vector of multiple "{attr_name}: {value}" strings for
#'         'font-size', 'font-family', 'fill'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
font_style <- function(state, geom) {

  gc <- state$gc

  # fontsize <- gc$cex * gc$ps
  font <- get_font_info(state)

  style <- c(
    glue_two("font-size: {font$size}px"),
    glue("font-family: {font$family}")
  )

  if (!is_black(gc$col)) {
    style <- c(style, glue_two("fill: {rgba_to_hex(gc$col)}"))
  }

  style
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Construct a vector of styles related to 'fill'
#'
#' @inheritParams stroke_style
#'
#' @return character vector of multiple "{attr_name}: {value}" strings for 'fill',
#'         'fill-opacity'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fill_style <- function(state, geom) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Setup
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gc          <- state$gc
  fill        <- gc$fill
  hexcolour   <- rgba_to_hex(fill)

  style <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check if:
  #  (1) This geom supports pattern fill
  #  (2) this hex colour is in the pattern list
  #  (3) the pattern_list entry for this hexcolour includes a 'pattern'
  #
  # Some patterns also have a filter. include this if present.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  plist <- state$rdata$pattern_list
  if (geom %in% geoms_with_pattern_fill) {
    pattern <- plist[[hexcolour]][['fill']]
    if (!is.null(pattern)) {
      if (inherits(pattern, 'SVGElement')) {
        state$rdata$msvg$defs(pattern)
        style <- glue::glue("fill: url(#{pattern$attribs$id})")
        if (!is.null(pattern$filter_def)) {
          state$rdata$msvg$defs(pattern$filter_def)
          style <- c(style, glue::glue("filter: url(#{pattern$filter_def$attribs$id})"))
        }
      } else {
        message("devoutsvg: 'fill' pattern for ", hexcolour, ' in the `pattern_list` must be of class SVGElement, not ',
                deparse(class(pattern)), ".  Using colour only.")
      }
    }
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If there was no pattern in the pattern_list, then just use the colour
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(style)) {
    style <- glue_two("fill: {hexcolour}")
  }


  if (rgba_to_alpha(gc$fill) != 1) {
    style <- c(style, glue_two("fill-opacity: {rgba_to_alpha(fill)}"))
  }

  style
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Construct a vector of styles related to 'filter'
#'
#' @inheritParams stroke_style
#'
#' @return character  "{attr_name}: {value}" strings for 'filter' or NULL
#'         if no filter available.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
filter_style <- function(state, geom) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Setup
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gc          <- state$gc
  fill        <- gc$fill
  hexcolour   <- rgba_to_hex(fill)

  style <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check if:
  #  (1) This element wanted to use the pattern list for 'fill'
  #  (2) this hex colour is in the pattern list
  #  (3) the pattern_list entry for this hexcolour includes a 'pattern'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  plist <- state$rdata$pattern_list
  pattern <- plist[[hexcolour]][['filter']]
  if (!is.null(pattern)) {
    if (inherits(pattern, 'SVGElement')) {
      state$rdata$msvg$defs(pattern)
      style <- glue("filter: url(#{pattern$attribs$id})")
    } else {
      message("devoutsvg: 'filter' pattern for ", hexcolour, ' in the `pattern_list` must be of class SVGElement, not ',
              deparse(class(pattern)), ".")
    }
  }

  style
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create stryle strings for the given attritute or set of attributes
#'
#' @param attr_name attribute name of name for a set of attributes
#' @inheritParams stroke_style
#'
#' @return character vector of one or more "{attr_name}: {value}" strings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_style_string <- function(attr_name, state, geom) {
  switch(
    attr_name,
    "fill"           = fill_style  (state = state, geom = geom),
    "stroke"         = stroke_style(state = state, geom = geom),
    "font"           = font_style  (state = state, geom = geom),
    "filter"         = filter_style(state = state, geom = geom)
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create CSS styles for the given attributes
#'
#' @param attr_names character vector of e.g 'fill', 'stroke', 'font'
#' @inheritParams stroke_style
#'
#' @return style string ready for inclusion in an SVG tag e.g.
#'         "stroke: #ffffff; fill: #000000;"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
style_string <- function(attr_names, state, geom) {
  if (length(attr_names) == 0) { return(NULL) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the individual style strings for all the attributes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  styles <- lapply(attr_names, create_style_string, state = state, geom = geom)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Collapse them into a single string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  styles <- paste(unlist(styles), collapse = "; ")

  styles
}

