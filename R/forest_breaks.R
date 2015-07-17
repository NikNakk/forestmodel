#' Calculate default breaks for limits
#'
#' @param limits limits of plot
#' @param trans transformation that will be used on the limits
#'
#' @return a vector with breaks ready to pass to \code{\link{panel_forest_plot}}
#' @export
#'
forest_breaks <- function(limits, trans = I, relative_width) {
  if (identical(trans, exp)) {
    limits <- exp(limits)
    make_range <- function(log_cut, step) {
      if (log_cut <= 0) {
        cut <- 10 ^ log_cut
        divisor <- cut / 10 * step
        if (limits[1] < cut) {
          seq(max(divisor, ceiling(limits[1] / divisor) * divisor), cut, divisor)
        } else {
          NULL
        }
      } else {
        cut <- 10 ^ (log_cut - 1)
        divisor <- step * cut
        if (limits[2] > divisor) {
          seq(divisor, min(10 ^ log_cut, floor(limits[2] / divisor) * divisor), divisor)
        } else {
          NULL
        }
      }
    }
    log_range <- floor(log10(limits[1])):ceiling(log10(limits[2]))
    step <- if (length(log_range) < 6) 2 else 5
    breaks <- unlist(lapply(log_range, make_range, step = step)) %>%
      c(1) %>%
      unique %>%
      sort %>%
      log
  } else {
    divisor <- 10 ^ round(log10(diff(limits)) - 0.9)
    breaks <- divisor * ceiling(limits[1] / divisor):floor(limits[2] / divisor)
  }
  breaks
}

