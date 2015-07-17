#' Produce a forest plot based on a regression model
#'
#' @param model regression model produced by \code{\link[stats]{lm}},
#'   \code{\link[stats]{glm}}, \code{\link[survival]{coxph}}
#' @param panels \code{list} with details of the panels that make up the plot (See Details)
#' @param exponentiate whether the numbers on the x scale should be exponentiated for plotting
#' @param format_options formatting options as a list including \code{colour} of the point estimate and
#'   error bars, \code{shape} of the point estimate, \code{banded} whether to show light
#'   grey bands behind alternate rows, \code{text_size} size of text in mm
#' @param funcs optional list of functions required for formatting \code{panels$display}
#' @param factor_separate_line whether to show the factor variable name on a separate line
#' @param theme theme to apply to the plot
#' @param limits limits of the forest plot on the X-axis (taken as the range of the data
#'   by default)
#' @param breaks breaks to appear on the X-axis (note these will be exponentiated
#'   if \code{exponentiate == TRUE})
#' @param return_data return the data to produce the plot as well as the plot itself
#' @param covariates a character vector optionally listing the variables to include in the plot
#'   (defaults to all variables)
#' @param recalculate_width \code{TRUE} to recalculate panel widths using the current device
#'   or the desired plot width in inches
#' @param recalculate_height \code{TRUE} to shrink text size using the current device
#'   or the desired plot height in inches
#'
#' @return A ggplot ready for display or saving, or (with \code{return_data == TRUE},
#'   a \code{list} with the parameters to call \code{\link{panel_forest_plot}} in the
#'   element \code{plot_data} and the ggplot itself in the element \code{plot})
#'
#' @details This function takes the model output from one of the common model functions in
#'   R (e.g. \code{\link[stats]{lm}}, \code{\link[stats]{glm}},
#'  \code{\link[survival]{coxph}}).
#'
#'  The \code{panels} parameter is a \code{list} of lists each of which have an element
#'  \code{width}
#'  and, optionally, \code{item}, \code{display}, \code{display_na},
#'  \code{heading}, \code{hjust} and \code{fontface}. \code{item} can be \code{"forest"} for the forest
#'  plot (exactly one required) or \code{"vline"} for a vertical line.
#'  \code{display} indicates which column to display as text from the standard ones produced by
#'  \code{\link[broom]{tidy}} and in addition
#'  \code{variable} (the term in the model, for factors without the level),
#'  \code{level} (the level of factors),
#'  \code{reference} (TRUE for the reference level of a factor).
#'  It can also be a formula using these columns. The function \code{trans} is definded to be the
#'  transformation between the coefficients and the scales (e.g. \code{exp}). Other functions not
#'  in base R can be provided as a \code{list} with the parameter \code{funcs}.
#'  \code{display_na} allows for an alternative display for NA terms within \code{estimate}.
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
#'
#' @examples
#'
#' library("survival")
#' library("dplyr")
#' pretty_lung <- lung %>%
#'   transmute(time,
#'             status,
#'             Age = age,
#'             Sex = factor(sex, labels = c("Male", "Female")),
#'             ECOG = factor(lung$ph.ecog),
#'             `Meal Cal` = meal.cal)
#'
#' print(forest_model(coxph(Surv(time, status) ~ ., pretty_lung)))
#'
#' data_for_lm <- data_frame(x = rnorm(100, 4),
#'                           y = rnorm(100, 3, 0.5),
#'                           z = rnorm(100, 2, 2),
#'                           outcome = 3 * x - 2 * y + 4 * z + rnorm(100, 0, 0.1))
#'
#' print(forest_model(lm(outcome ~ ., data_for_lm)))
#'
#' data_for_logistic <- data_for_lm %>% mutate(
#'   outcome = (0.5 * (x - 4) * (y - 3) * (z - 2) + rnorm(100, 0, 0.05)) > 0.5
#' )
#'
#' print(forest_model(glm(outcome ~ ., binomial(), data_for_logistic)))

forest_model <- function(model,
                         panels = default_forest_panels(model, factor_separate_line = factor_separate_line),
                         covariates = NULL, exponentiate = NULL, funcs = NULL,
                         factor_separate_line = FALSE,
                         format_options = list(colour = "black", shape = 15,
                           text_size = 5, banded = TRUE),
                         theme = theme_forest(),
                         limits = NULL, breaks = NULL, return_data = FALSE,
                         recalculate_width = TRUE, recalculate_height = TRUE) {
  data <- model.frame(model)
  if (inherits(model, "coxph")) {
    tidy_model <- broom::tidy(model)
  } else {
    tidy_model <- broom::tidy(model, conf.int = TRUE)
  }
  if (is.null(exponentiate)) {
    exponentiate <- inherits(model, "coxph") ||
      (inherits(model, "glm") && model$family$link == "logit")
  }
  if (exponentiate) trans <- exp else trans <- I
  stopifnot(is.list(panels))
  forest_terms <- data_frame(variable = names(attr(model$terms, "dataClasses"))[-1],
                             term_label = attr(model$terms, "term.labels"),
                             class = attr(model$terms, "dataClasses")[-1])
  if (!is.null(covariates)) {
    forest_terms <- filter(forest_terms, variable %in% covariates)
  }
  forest_terms <- forest_terms %>%
    rowwise %>% do({
      if (.$class %in% c("factor")) {
        tab <- table(data[, .$variable])
        out <- cbind(as_data_frame(.),
                     data_frame(level = names(tab),
                                level_no = 1:length(tab),
                                n = as.integer(tab)))
        if (factor_separate_line) {
          out <- bind_rows(as_data_frame(.), out)
        }
        out
      } else {
        out <- data.frame(., level = NA, level_no = NA, n = sum(!is.na(data[, .$variable])),
                   stringsAsFactors = FALSE)
        if (.$class == "logical") {
          out$term_label <- paste0(.$term_label, "TRUE")
        }
        out
      }
    }) %>%
    ungroup %>%
    mutate(term = paste0(term_label, replace(level, is.na(level), ""))) %>%
    left_join(tidy_model, by = "term") %>%
    mutate(
      reference = ifelse(is.na(level_no), FALSE, level_no == 1),
      estimate = ifelse(reference, 0, estimate),
      variable = ifelse(is.na(level_no) | (level_no == 1 & !factor_separate_line), variable, NA)
    )
  plot_data <- list(
      forest_data = forest_terms,
      mapping = aes(estimate, xmin = conf.low, xmax = conf.high),
      panels = panels, trans = trans,
      funcs = funcs, format_options = format_options, theme = theme,
      limits = limits, breaks = breaks, recalculate_width = recalculate_width,
      recalculate_height = recalculate_height
  )
  main_plot <- do.call("panel_forest_plot", plot_data)
  if (return_data) {
    list(plot_data = plot_data, plot = main_plot)
  } else {
    main_plot
  }
}
