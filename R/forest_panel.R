#' Create definition of a panel for \code{forest_model}
#'
#' @param width relative width of the panel
#' @param item specification of which type of item to use; overridden if display is not missing
#' @param display bare expression that specifies the variable or expresion to display
#' @param display_na what to display if a value is \code{NA}
#' @param hjust horizontal justification
#' @param heading heading to be used (defaults to the variable name)
#' @param fontface fontface to use
#' @param linetype line type to use
#' @param line_x position for dashed line in forest plot
#' @param parse whether text should be parsed as expressions
#' @param width_group grouping used when recalcualting widths of panels
#'
#' @return panel definition as a list
#' @import rlang
#' @export
#'
forest_panel <- function(width, item = c("", "forest", "vline"), display = NULL,
                         display_na = NULL, hjust = NULL, heading = NULL,
                         fontface = NULL, linetype = NULL, line_x = NULL, parse = NULL,
                         width_group = NULL) {
  item <- match.arg(item)
  if (item == "") {
    item <- NULL
  }
  display <- enquo(display)
  if (is.null(get_expr(display))) {
    display <- NULL
  }
  fontface = enquo(fontface)
  if (is.null(get_expr(fontface))) {
    fontface <- NULL
  }
  panel <- structure(list(
    width = width %||% NA_real_,
    item = item,
    display = display,
    display_na = display_na,
    hjust = hjust,
    heading = heading,
    fontface = fontface,
    linetype = linetype,
    line_x = line_x,
    parse = parse,
    width_group = width_group
  ),
  class = "forest_panel"
  )
  # Remove missing values
  panel[vapply(panel, is.null, logical(1))] <- NULL
  panel
}
