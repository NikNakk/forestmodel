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
default_forest_panels <- function(model = NULL, factor_separate_line = FALSE, measure = NULL, trans_char = "I", which_global_p = "none") {
    if (which_global_p != "none") {
        if (which_global_p == "logtest") {
            global_p_text <- "(Likelihood ratio test)"
        } else if (which_global_p == "waldtest") {
            global_p_text <- "(Wald test)"
        } else if (which_global_p == "sctest") {
            global_p_text <- "(Score (logrank))"
        }
    } else {
    global_p_text <- "(I was none)"
    }
  if (is.list(model) && inherits(model[[1]], "rma")) {
    model <- model[[1]]
  }
  if (inherits(model, "rma")) {
    if (trans_char == "I") {
      trans_char <- "FALSE"
    }
    measure <- measure %||% rma_setlab(model$measure,
      transf.char = "FALSE",
      atransf.char = trans_char,
      gentype = 1
    )
    panels <- list(
      forest_panel(width = 0.03),
      forest_panel(
        width = 0.01, display = study, fontface = "bold", heading = "Study",
        width_group = 1
      ),
      forest_panel(
        width = 0.18, display = stat, parse = TRUE,
        width_group = 1
      ),
      forest_panel(width = 0.05, display = n, hjust = 1, heading = "N"),
      forest_panel(width = 0.03, item = "vline", hjust = 0.5),
      forest_panel(
        width = 0.55, item = "forest", hjust = 0.5, heading = measure,
        linetype = "dashed", line_x = 0
      ),
      forest_panel(width = 0.03, item = "vline", hjust = 0.5),
      forest_panel(
        width = 0.12,
        display = sprintf("%0.2f (%0.2f, %0.2f)", trans(estimate), trans(conf.low), trans(conf.high)),
        display_na = NA
      ),
      forest_panel(width = 0.03)
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
    is_na <- function(x) sapply(x, is.na)
    panels <- list(
      forest_panel(width = 0.03),
      forest_panel(width = 0.1, display = variable, fontface = "bold", heading = "Variable"),
      forest_panel(width = 0.1, display = level),
      forest_panel(width = 0.05, display = n, hjust = 1, heading = "N"),
      forest_panel(width = 0.03, item = "vline", hjust = 0.5),
      forest_panel(
        width = 0.55, item = "forest", hjust = 0.5, heading = measure,
        linetype = "dashed", line_x = 0
      ),
      forest_panel(width = 0.03, item = "vline", hjust = 0.5),
      forest_panel(
        width = 0.12,
        display = if_else(reference, if_else(is_na(p.value), "Reference", paste0("Reference | Global p ", global_p_text, ":")),
          sprintf("%0.2f (%0.2f, %0.2f)", trans(estimate), trans(conf.low), trans(conf.high))
        ),
        display_na = NA
      ),
      forest_panel(
        width = 0.05, display = if_else(reference & is_na(p.value), "", format.pval(p.value, digits = 1, eps = 1e-3)), display_na = NA,
        hjust = 1, heading = "p"
      ),
      forest_panel(width = 0.03)
    )
    if (factor_separate_line) {
      panels[[2]][c("width", "width_group", "width_fixed")] <- list(0.02, 1, TRUE)
      panels[[3]][c("width", "width_group", "width_fixed")] <- list(0.02, 1, TRUE)
    }
  }
  panels
}
