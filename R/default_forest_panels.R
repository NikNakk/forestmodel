#' Default panels for forest_model
#'
#' @param model model object to guess label and determine defaults
#' @param measure label for main forest plot
#' @param factor_separate_line changes defaults for widths of variable depending on whether
#'   factors have their name on separate line
#' @param trans_char character representation of transform for axes
#'
#' @return `list` ready to be passed to `forest_model`
#' @export
#'
default_forest_panels <- function(model = NULL, factor_separate_line = FALSE, measure = NULL, trans_char = "I") {
  if (is.list(model) && inherits(model[[1]], "rma")) {
    model <- model[[1]]
  }
  if (inherits(model, "rma")) {
    if (trans_char == "I") {
      trans_char <- "FALSE"
    }
    measure <- measure %||% rma_setlab(model$measure, transf.char = "FALSE",
                                       atransf.char = trans_char,
                                       gentype = 1)
    panels <- list(
      list(width = 0.03),
      list(width = 0.01, display = ~study, fontface = "bold", heading = "Study",
           width_group = 1),
      list(width = 0.18, display = ~stat, parse = TRUE,
           width_group = 1),
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
    if (is.null(measure)) {
      if (inherits(model, "coxph")) {
        measure <- "Hazard ratio"
      } else if (inherits(model, "glm") && model$family$link == "logit") {
        measure <- "Odds ratio"
      } else {
        measure <- "Estimate"
      }
    }
    panels <- list(
      list(width = 0.03),
      list(width = 0.1, display = ~variable, fontface = "bold", heading = "Variable"),
      list(width = 0.1, display = ~level),
      list(width = 0.05, display = ~n, hjust = 1, heading = "N"),
      list(width = 0.03, item = "vline", hjust = 0.5),
      list(width = 0.55, item = "forest", hjust = 0.5, heading = measure,
           linetype = "dashed", line_x = 0),
      list(width = 0.03, item = "vline", hjust = 0.5),
      list(width = 0.12,
           display = ~ifelse(reference, "Reference",
                             sprintf("%0.2f (%0.2f, %0.2f)",trans(estimate), trans(conf.low), trans(conf.high))),
           display_na = NA),
      list(width = 0.05, display = ~ifelse(reference, "", format.pval(p.value, digits = 1, eps = 1e-3)), display_na = NA,
           hjust = 1, heading = "p"),
      list(width = 0.03)
    )
    if (factor_separate_line) {
      panels[[2]][c("width", "width_group", "width_fixed")] <- list(0.02, 1, TRUE)
      panels[[3]][c("width", "width_group", "width_fixed")] <- list(0.02, 1, TRUE)
    }
  }
  panels
}
