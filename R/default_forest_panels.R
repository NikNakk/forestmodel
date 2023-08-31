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
#' @import rlang
#'
default_forest_panels <- function(model = NULL, factor_separate_line = FALSE, measure = NULL, trans_char = "I") {
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

    max_interaction_terms <- 0
    if (!is.null(model)) {
      forest_terms_basic <- make_forest_terms_basic(model)
      if (
        any(
          factor_separate_line *
          forest_terms_basic$is_interaction * vapply(forest_terms_basic$interaction_terms_are_factors, sum, integer(1)) > 0
        )
      ) {
        warning("`factor_separate_line = TRUE` is not supported when there are interacting terms that include one or more factors")
        factor_separate_line <- FALSE
      }
      max_interaction_terms <- max(
        vapply(forest_terms_basic$interaction_terms_are_factors, sum, integer(1))
      )
    }

    is_na <- function(x) sapply(x, is.na)
    panels <- list(
      forest_panel(width = 0.03),
      forest_panel(width = 0.1, display = variable, fontface = "bold", heading = "Variable"),
      forest_panel(width = 0.1, display = level_1),
      forest_panel(width = 0.05, display = n, hjust = 1, heading = "N"),
      forest_panel(width = 0.03, item = "vline", hjust = 0.5),
      forest_panel(
        width = 0.55, item = "forest", hjust = 0.5, heading = measure,
        linetype = "dashed", line_x = 0
      ),
      forest_panel(width = 0.03, item = "vline", hjust = 0.5),
      forest_panel(
        width = 0.12,
        display = if_else(reference, if_else(is_na(p.value), "Reference", "Reference;  Variable logrank:"),
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
    if (max_interaction_terms > 1) {
      interacting_level_panels <- lapply(
        seq_len(max_interaction_terms),
        function(i) {
          forest_panel(width = 0.1, display = !!rlang::sym(paste0("level_", i)),
                       fontface = ifelse(!is.na(level_heading) & level_heading, "italic", "plain")
          )
        }
      )
      panels <- c(panels[1:2], interacting_level_panels, panels[-(1:3)])
    }

    if (factor_separate_line) {
      panels[[2]][c("width", "width_group", "width_fixed")] <- list(0.02, 1, TRUE)
      panels[[3]][c("width", "width_group", "width_fixed")] <- list(0.02, 1, TRUE)
    }
  }
  panels
}
