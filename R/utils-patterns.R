





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Write a pattern to the state.
#'
#' @param state device state
#' @param ... other arguments to foreign \code{decode_pattern_from_rgba_vec()} call
#'
#' @return state
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_pattern <- function(state, ...) {

  fill <- state$gc$fill

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get the package name
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pattern_pkg <- state$rdata$pattern_pkg

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Confirm that this package can generate a pattern for this fill
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  is_valid_pattern_encoding <- get('is_valid_pattern_encoding', asNamespace(pattern_pkg))
  pattern_ok                <- is_valid_pattern_encoding(fill)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If it can't handle the fill, then return the 'state' unchanged
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!pattern_ok) {
    return(state)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Generate a pattern
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  decode_pattern_from_rgba_vec <- get('decode_pattern_from_rgba_vec', asNamespace(pattern_pkg))
  pattern    <- decode_pattern_from_rgba_vec(fill)
  pattern_id <- attr(pattern, 'id') %||% pattern$attribs$id

  if (is.null(pattern_id)) {
    warning("Null pattern_id in devoutsvg::write_pattern(): ", pattern)
    return(state)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If the pattern_id hasn't already been included, then include it
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!pattern_id %in% state$rdata$all_pattern_ids) {
    state$rdata$all_pattern_ids <- c(state$rdata$all_pattern_ids, pattern_id)

    if (!is.null(pattern$filter_def)) {
      state$rdata$msvg$defs(pattern$filter_def)
    }
    state$rdata$msvg$defs(pattern)

  }

  state
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a pattern ID
#'
#' @param pattern_pkg name of the package which contains the pattern handling
#' @param rgba_vec colour vector. elements in [0, 255]
#' @param ... other arguments to foreign \code{create_pattern_id()} call
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pattern_id_from_rgba_vec <- function(pattern_pkg, rgba_vec, ...) {
  create_pattern_id_from_rgba_vec <- get('create_pattern_id_from_rgba_vec', asNamespace(pattern_pkg))
  create_pattern_id_from_rgba_vec(rgba_vec)
}