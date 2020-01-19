


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# When opening a device
#  - create a "canvas".  For svg, the canvas is just a text string of SVG
#    commands that we'll keep adding to with each device call
#  - add the canvas to the 'state$rdata' list
#  - always return the state so we keep the canvas across different device calls
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_open <- function(args, state) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a 'minisvg' document. The 'viewBox' will be set by the width and
  # height, but then remove the 'width' and 'height' attributes and keep only
  # the 'viewBox'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  msvg <- minisvg::SVGDocument$new(
    width = state$dd$right,
    height = state$dd$bottom
  )$update(width = NULL, height = NULL)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add a default style
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  msvg$defs("<style type='text/css'><![CDATA[
        line, polyline, polygon, path, rect, circle {
          fill: none;
          stroke: #000000;
          stroke-linecap: round;
          stroke-linejoin: round;
          stroke-miterlimit: 10.00;
        }
      ]]></style>")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # add a blank white rectangle as the background
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  msvg$rect(width='100%', height='100%', style='stroke: none; fill: #ffffff')

  state$rdata$msvg <- msvg

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Keep track of what clip regions have been defined, and the ID of
  # the current clip region
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  state$rdata$current_clip_id <- NULL
  state$rdata$all_clip_ids    <- character(0)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Keep track of which patterns have been added
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  state$rdata$all_pattern_ids <- character(0)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # pre-check that a pattern package
  #  (a) exists
  #  (b) provides all the required functions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pattern_pkg <- state$rdata$pattern_pkg
  if (!is.null(pattern_pkg)) {
    message("pattern_pkg: ", pattern_pkg)
    if (!pattern_pkg %in% rownames(installed.packages())) {
      warning("svg_open(): specified 'pattern_pkg' is not installed: ", pattern_pkg, call.=FALSE)
      state$rdata$pattern_pkg <- NULL
    } else {
      func_names <- c('is_valid_pattern_encoding',
                      'decode_pattern_from_rgba_vec',
                      'create_pattern_id_from_rgba_vec')
      funcs <- mget(func_names, envir = asNamespace(pattern_pkg), ifnotfound = NA)
      if (any(is.na(funcs))) {
        warning("svg_open(): specified 'pattern_pkg' (", pattern_pkg,
                ") does not export all required functions: ", deparse(func_names),
                call. = FALSE)
        state$rdata$pattern_pkg <- NULL
      }
    }
  }

  state$rdata$use_pattern_pkg <- !is.null(state$rdata$pattern_pkg)

  state
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# When the device is closed
#   - add the closing </svg> tag
#   - output the SVG to file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_close <- function(args, state) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write svg text to file
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  state$rdata$msvg$save(filename = state$rdata$filename, include_declaration = TRUE)

  state
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add a circle to the SVG
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_circle <- function(args, state) {
  attr_names <- c('fill', 'stroke')

  state$rdata$msvg$circle(
    cx        = round(args$x, 2),
    cy        = round(args$y, 2),
    r         = paste0(round(args$r, 2), 'pt'),
    style     = style_string(attr_names, state),
    clip_path = clip_path_string(state)
  )


  state
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add a polyline to the SVG
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_polyline <- function(args, state) {
  attr_names <- c('stroke')

  state$rdata$msvg$polyline(
    xs        = round(args$x, 2),
    ys        = round(args$y, 2),
    style     = style_string(attr_names, state),
    clip_path = clip_path_string(state)
  )

  state
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Draw multiple paths
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_path <- function(args, state) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Path Fill respects the pattern_pkg - R.E.S.P.E.C.T.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (state$rdata$use_pattern_pkg) {
    state <- write_pattern(state)
  }


  attr_names <- c('stroke', 'fill')

  extents <- c(0, cumsum(args$nper))

  for (poly in seq_len(args$npoly)) {
    subargs   <- args
    lower     <- extents[poly     ] + 1L
    upper     <- extents[poly + 1L]
    subargs$x <- subargs$x[lower:upper]
    subargs$y <- subargs$y[lower:upper]
    state$rdata$msvg$polygon(
      xs        = subargs$x,
      ys        = subargs$y,
      style     = style_string(attr_names, state, state$rdata$use_pattern_pkg),
      clip_path = clip_path_string(state)
    )
  }



  state
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add a polygon to the SVG
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_polygon <- function(args, state) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Rectangle Fill respects the pattern_pkg - R.E.S.P.E.C.T.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (state$rdata$use_pattern_pkg) {
    state <- write_pattern(state)
  }

  attr_names <- c('fill', 'stroke')

  state$rdata$msvg$polygon(
    xs        = round(args$x, 2),
    ys        = round(args$y, 2),
    style     = style_string(attr_names, state, state$rdata$use_pattern_pkg),
    clip_path = clip_path_string(state)
  )

  state
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add a line to the SVG
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_line <- function(args, state) {
  attr_names <- c('fill', 'stroke')

  state$rdata$msvg$line(
    x1        = round(args$x1, 2),
    y1        = round(args$y1, 2),
    x2        = round(args$x2, 2),
    y2        = round(args$y2, 2),
    style     = style_string(attr_names, state),
    clip_path = clip_path_string(state)
  )

  state
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Unpack font information from the graphics context
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_font_info <- function(state) {
  fontfamily <- state$gc$fontfamily
  fontface   <- state$gc$fontface
  is_bold    <- fontface %in% c(2, 4)
  is_italic  <- fontface %in% c(3, 4)
  is_symbol  <- fontface == 5

  if (is_symbol) {
    fontfamily <- 'symbol'
  } else if (fontfamily == '') {
    fontfamily <- 'sans'
  }

  fontfamily <- gdtools::match_family(fontfamily)


  fontsize <- state$gc$cex * state$gc$ps


  list(family = fontfamily, bold = is_bold, italic = is_italic, size = fontsize)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add text to the SVG
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_text <- function(args, state) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the display width of the string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  font    <- get_font_info(state)
  metrics <- gdtools::str_metrics(args$str, fontname = font$family, fontsize = font$size, bold = font$bold,italic = font$italic, fontfile = "")
  width   <- metrics[['width']]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Only calculate a transform if the rotation is non-zero
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  transform <- NULL
  if (args$rot != 0) {
    transform <- minisvg::svg_prop$transform$rotate(
      a = -round(args$rot, 2),
      x =  round(args$x  , 2),
      y =  round(args$y  , 2)
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add a 'g' group to the SVG.
  #  - the clip-path goes on the group
  #  - the text element is a child of the group
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  state$rdata$msvg$g(
    clip_path    = clip_path_string(state),
    minisvg::stag$text(
      trimws(args$str),
      x            = round(args$x, 2),
      y            = round(args$y, 2),
      textLength   = paste0(round(width, 2), "px"),
      lengthAdjust = "spacingAndGlyphs",
      style        = style_string(attr_names = 'font', state),
      transform
    )
  )


  state
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add RECT to the SVG
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_rect <- function(args, state) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Rectangle Fill respects the pattern_pkg - R.E.S.P.E.C.T.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (state$rdata$use_pattern_pkg) {
    state <- write_pattern(state)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate rectangle extents
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x      <- min(args$x0,  args$x1)
  y      <- min(args$y0,  args$y1)
  width  <- abs(args$x1 - args$x0)
  height <- abs(args$y1 - args$y0)


  attr_names <- c('fill', 'stroke')

  state$rdata$msvg$rect(
    x         = round(x, 2),
    y         = round(y, 2),
    width     = round(width , 2),
    height    = round(height, 2),
    style     = style_string(attr_names, state, state$rdata$use_pattern_pkg),
    clip_path = clip_path_string(state)
  )

  state
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Return the width of the given string
#'
#' @param args,state standard pass-through from device driver
#'
#' @import gdtools
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_strWidth <- function(args, state) {

  fontsize    <- state$gc$cex * state$gc$ps
  metrics     <- gdtools::str_metrics(args$str, fontname = "sans", fontsize = fontsize, bold = FALSE, italic = FALSE, fontfile = "")
  state$width <- metrics[['width']]

  state
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return some info about font size
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_metricInfo <- function(args, state) {

  cint <- abs(args$c)
  str  <- intToUtf8(cint)

  fontsize <- state$gc$cex * state$gc$ps
  metrics  <- gdtools::str_metrics(str, fontname = "sans", fontsize = fontsize, bold = FALSE, italic = FALSE, fontfile = "")

  state$ascent  <- metrics[['ascent' ]]
  state$descent <- metrics[['descent']]
  state$width   <- metrics[['width'  ]]

  state
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SVG attribute for Current clip-path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clip_path <- function(state) {

  if (is.null(state$rdata$current_clip_id)) {
    ''
  } else {
    glue::glue("clip-path='url(#{state$rdata$current_clip_id})'")
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SVG attribute for Current clip-path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clip_path_string <- function(state) {
  if (is.null(state$rdata$current_clip_id)) {
    NULL
  } else {
    glue::glue("url(#{state$rdata$current_clip_id})")
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Update the clipping path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_clip <- function(args, state) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # What's the current clip ID.  Could be NULL if not yet set.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  current_clip_id <- state$rdata$current_clip_id

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create an ID string but just concatenating all the coordiantes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_clip    <- with(args, round(c(x0, y0, x1, y1), 2))
  this_clip_id <- paste0("clip_", gsub("\\.", "_", paste(this_clip, collapse="_")))
  # print(this_clip)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If the requested clipping is already active, no need to add anything to SVG
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (identical(this_clip_id, current_clip_id)) {
    return(state)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set this new clip ID as the current clipping
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  state$rdata$current_clip_id <- this_clip_id

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If the clip ID already exists in IDs we have output to the SVG, then
  # there is no need to output the clip definition again.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (this_clip_id %in% state$rdata$all_clip_ids) {
    return(state)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add the current clip ID to the list of all clip IDs in this SVG
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  state$rdata$all_clip_ids <- c(state$rdata$all_clip_ids, this_clip_id)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calcualte clipping rectangle extents
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x      <- min(args$x0,  args$x1)
  y      <- min(args$y0,  args$y1)
  width  <- abs(args$x1 - args$x0)
  height <- abs(args$y1 - args$y0)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add the clipPath to the SVG
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  state$rdata$msvg$defs()$clipPath(
    id = this_clip_id,
    minisvg::stag$rect(
      x      = round(x     , 2),
      y      = round(y     , 2),
      width  = round(width , 2),
      height = round(height, 2)
    )
  )


  state
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' The main SVG callback.
#'
#' @param device_call name of device call
#' @param args arguments to the call
#' @param state rdata, gc and dd
#'
#' @import glue
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_callback <- function(device_call, args, state) {
  switch(
    device_call,
    "open"         = svg_open      (args, state),
    "close"        = svg_close     (args, state),
    "circle"       = svg_circle    (args, state),
    "line"         = svg_line      (args, state),
    "polyline"     = svg_polyline  (args, state),
    "path"         = svg_path      (args, state),
    "polygon"      = svg_polygon   (args, state),
    "text"         = svg_text      (args, state),
    "textUTF8"     = svg_text      (args, state),
    'rect'         = svg_rect      (args, state),
    'strWidth'     = svg_strWidth  (args, state),
    'strWidthUTF8' = svg_strWidth  (args, state),
    'metricInfo'   = svg_metricInfo(args, state),
    'clip'         = svg_clip      (args, state),
    {
      # if (!device_call %in% c('size', 'mode')) {print(device_call)};
      state
    }
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' SVG device written in R. JUst sets up \code{rdevice} to call \code{svg_callback()}
#'
#' @param filename default: "svgout.svg"
#' @param width,height size in inches
#' @param ... arguments passed to \code{devout::rdevice}
#'
#' @importFrom utils installed.packages
#' @import devout
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svgout <- function(filename = "svgout.svg", width = 6, height = 4,
                   ...) {
  requireNamespace('devout')
  devout::rdevice("svg_callback", filename = filename, width = width, height = height, ...)
}

