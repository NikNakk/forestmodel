#' Create format options for \code{forest_model}
#'
#' @param colour colour of the point estimate and error bars
#' @param color alias for colour
#' @param shape shape of the point estimate
#' @param text_size text size in mm
#' @param point_size point size
#' @param banded whether to show light grey bands behind alternate rows
#'
#'
#' @return list of format options
#' @export
#'
forest_model_format_options <- function(colour = "black", color = NULL, shape = 15,
                                        text_size = 5, point_size = 5, banded = TRUE) {
  structure(
    list(
      colour = colour %||% color,
      shape = shape,
      text_size = text_size,
      point_size = point_size,
      banded = banded
    ),
    class = "forest_model_format_options"
  )
}
