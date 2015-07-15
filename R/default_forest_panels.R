#' Default panels for forest_model
#'
#' @param model model object to guess label and determine defaults
#' @param measure label for main forest plot
#' @param factor_separate_line changes defaults for widths of variable depending on whether
#'   factors have their name on separate line
#'
#' @return `list` ready to be passed to `forest_model`
#' @export
#'
default_forest_panels <- function(model, factor_separate_line = FALSE, measure = "Estimate") {
  if (missing(measure)) {
    if (inherits(measure, "coxph")) {
      measure <- "Hazard ratio"
    } else if (inherits(model, "glm") && model$family$link == "logit") {
      measure <- "Odds ratio"
    } else {
      measure <- "Estimate"
    }
  }
  if (inherits(model, "rma") || (is.list(model) && inherits(model[[1]], "rma"))) {
    list(
      list(width = 0.03),
      list(width = 0.02, display = ~study, fontface = "bold", heading = "Study"),
      list(width = 0.13, display = ~stat, parse = TRUE),
      list(width = 0.05, display = ~n, hjust = 1, heading = "N"),
      list(width = 0.03, item = "vline", hjust = 0.5),
      list(width = 0.55, item = "forest", hjust = 0.5, heading = measure,
           linetype = "dashed", line_x = 0),
      list(width = 0.03, item = "vline", hjust = 0.5),
      list(width = 0.12,
           display = ~sprintf("%0.2f (%0.2f, %0.2f)", trans(estimate), trans(conf.low), trans(conf.high)),
           display_na = NA),
      list(width = 0.03)
    )
  } else {
    list(
      list(width = 0.03),
      list(width = if (factor_separate_line) 0.02 else 0.1, display = ~variable, fontface = "bold", heading = "Variable"),
      list(width = if (factor_separate_line) 0.1 else 0.07, display = ~level),
      list(width = 0.05, display = ~n, hjust = 1, heading = "N"),
      list(width = 0.03, item = "vline", hjust = 0.5),
      list(width = 0.55, item = "forest", hjust = 0.5, heading = measure,
           linetype = "dashed", line_x = 0),
      list(width = 0.03, item = "vline", hjust = 0.5),
      list(width = 0.12,
           display = ~ifelse(reference, "Reference",
                             sprintf("%0.2f (%0.2f, %0.2f)",trans(estimate), trans(conf.low), trans(conf.high))),
           display_na = NA),
      list(width = 0.05, display = ~ifelse(reference, "", format.pval(p.value, eps = 1e-3)), display_na = NA,
           hjust = 1, heading = "p"),
      list(width = 0.03)
    )
  }
}


#' Default forest theme
#'
#' @return a theme object for use with ggplot2
#' @export
theme_forest <- function() {
  theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
    )
}

#' Calculate default breaks for limits
#'
#' @param limits limits of plot
#' @param trans transformation that will be used on the limits
#'
#' @return a vector with breaks ready to pass to \code{\link{panel_forest_plot}}
#' @export
#'
forest_breaks <- function(limits, trans = I) {
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
    divisor <- 10 ^ round(log10(diff(limits)) - 1)
    breaks <- divisor * ceiling(limits[1] / divisor):floor(limits[2] / divisor)
  }
  breaks
}

