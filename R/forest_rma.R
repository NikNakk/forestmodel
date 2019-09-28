#' Generate a forest plot from a meta-analysis
#'
#' @param model a single \code{\link[metafor]{rma}} object or a \code{list} of them
#' @param study_labels a character vector of study labels or list of character vectors the same length as \code{model}
#' @param model_label a single model label or character vector of model labels the same length as \code{model}
#' @param show_stats a \code{list} of stats to show at the bottom of the forest plot for e.g. heterogeneity
#' @param additional_data a \code{data.frame} of additional data that can be referenced for the data
#'   shown in the panels of the forest plot
#' @param point_size a numeric vector with the point sizes for the individual studies, or a single value used for
#'   all studies, or a list of numeric vectors if more than one model is to be plotted
#' @param trans an optional transform function used on the numeric data for plotting the axes
#' @param show_individual_studies whether to show the individual studies (the default) or just the summary diamond
#' @param show_model a logical value, if `TRUE`, show model result, otherwise only show forest plots for studies
#' @inheritParams forest_model
#'
#' @details This produces a forest plot using the \code{\link[metafor]{rma}}
#'
#' @return plot
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#' if (require("metafor")) {
#'   data("dat.bcg")
#'   dat <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
#'   model <- rma(yi, vi, data = dat)
#'
#'   print(forest_rma(model,
#'     study_labels = paste(dat.bcg$author, dat.bcg$year),
#'     trans = exp
#'   ))
#'
#'   print(forest_rma(model,
#'     panels = forest_panels(
#'       Study = ~study,
#'       N = ~n, ~vline, `Log Relative Risk` = ~ forest(line_x = 0),
#'       ~ spacer(space = 0.10),
#'       ~ sprintf("%0.3f (%0.3f, %0.3f)", estimate, conf.low, conf.high)
#'     ),
#'     study_labels = paste(dat.bcg$author, dat.bcg$year),
#'     trans = exp
#'   ))
#' }
forest_rma <- function(model, panels = NULL,
                       study_labels = NULL,
                       additional_data = NULL,
                       point_size = NULL,
                       model_label = NULL,
                       show_individual_studies = TRUE,
                       show_model = TRUE,
                       show_stats = list(
                         "I^2" = rlang::quo(sprintf("%0.1f%%", I2)),
                         "p" = rlang::quo(format.pval(QEp,
                           digits = 4, eps = 1e-4,
                           scientific = 1
                         ))
                       ),
                       trans = I, funcs = NULL,
                       format_options = list(
                         colour = "black", shape = 15,
                         text_size = 5, banded = TRUE
                       ),
                       theme = theme_forest(),
                       limits = NULL, breaks = NULL, return_data = FALSE,
                       recalculate_width = TRUE, recalculate_height = TRUE) {
  stopifnot(is.list(model))
  panels <- panels %||% default_forest_panels(model, trans_char = deparse(substitute(trans)))
  if (!inherits(model, "rma")) {
    # List of models
    n_model <- length(model)
    stopifnot(all(vapply(model, inherits, logical(1), "rma")))
    if (length(study_labels) == 1L) study_labels <- rep(study_labels, n_model)
    if (length(model_label) == 1L) model_label <- rep(model_label, n_model)
    if (length(point_size) == 1L) point_size <- rep(model_label, n_model)
    if (show_individual_studies == 1L) {
      show_individual_studies <- rep(show_individual_studies, n_model)
    }
    if (is.data.frame(additional_data)) {
      additional_data <- rep(list(additional_data), n_model)
    }
    stopifnot(
      is.null(study_labels) || length(study_labels) == n_model,
      is.null(model_label) || length(model_label) == n_model,
      is.null(point_size) || length(point_size) == length(point_size),
      is.null(additional_data) || length(additional_data) == n_model,
      is.null(show_individual_studies) ||
        length(show_individual_studies) == n_model
    )

    forest_data_list <- lapply(seq(model), function(i) {
      get_data_for_rma(model[[i]],
        study_labels = study_labels[[i]],
        model_label = model_label[i], point_size = point_size[[i]],
        additional_data = additional_data[[i]],
        show_individual_studies = show_individual_studies[[i]],
        show_stats = show_stats
      )
    })
    forest_data <- bind_rows(forest_data_list)
    if (!is.null(names(model))) {
      forest_data$.section <- rep(names(model), vapply(forest_data_list, nrow, numeric(1)))
    }
  } else {
    forest_data <-
      get_data_for_rma(model,
        study_labels = study_labels,
        model_label = model_label, point_size = point_size,
        additional_data = additional_data,
        show_individual_studies = show_individual_studies,
        show_stats = show_stats
      )
    if (!show_model) {
      forest_data = forest_data %>%
        dplyr::slice(1:(nrow(forest_data)-2))
    }
  }

  if (!is.null(limits)) {
    forest_data <- forest_data %>%
      mutate(
        arrow_tag.l = limits[1],
        arrow_tag.r = limits[2],
        arrow_tag.l = ifelse(conf.low < .data$arrow_tag.l, TRUE, FALSE),
        arrow_tag.r = ifelse(conf.high > .data$arrow_tag.r, TRUE, FALSE)
      ) %>%
      mutate(
        plot_range.low = ifelse(.data$arrow_tag.l, limits[1], conf.low),
        plot_range.high = ifelse(.data$arrow_tag.r, limits[2], conf.high)
      )
  }

  plot_data <- list(
    forest_data = forest_data,
    mapping = aes(estimate,
      xmin = conf.low, xmax = conf.high, size = point_size,
      section = .section, band = .band, diamond = .diamond,
      whole_row = .whole_row
    ),
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

# Extract data from individual rma model
#
# @inheritParams forest_rma
#
# @importFrom rlang eval_tidy
# @return a data.frame with the extracted data
get_data_for_rma <-
  function(model, study_labels = NULL, model_label = NULL, point_size = NULL,
             additional_data = NULL, show_individual_studies = TRUE, show_stats = NULL) {
    if (model$level > 1) { # Has changed at some point in rma.uni
      alpha <- (100 - model$level) / 100
    } else {
      alpha <- model$level
    }
    k <- length(model$vi)

    model_label <- model_label %||% if (model$method == "FE") "FE Model" else "RE Model"
    study_labels <- study_labels %||% paste("Study", 1:k)

    if (is.null(point_size)) {
      if (is.null(model$weights)) {
        if (any(model$vi <= 0, na.rm = TRUE)) {
          point_size <- rep(1, k)
        }
        else {
          point_size <- 1 / sqrt(model$vi)
        }
      }
      else {
        point_size <- model$weights
      }
      if (!all(point_size == point_size[1])) {
        point_size <- (point_size - min(point_size, na.rm = TRUE)) /
          diff(range(point_size), na.rm = TRUE)
        point_size <- (point_size * 1) + 0.5
      }
      if (all(is.na(point_size))) point_size <- rep(1, k)
    }

    if (show_individual_studies) {
      forest_data <- as_data_frame(model[c("yi", "ni", "vi")]) %>%
        transmute(
          estimate = yi,
          se = sqrt(vi),
          n = ni,
          conf.low = estimate + stats::qnorm(alpha / 2) * se,
          conf.high = estimate - stats::qnorm(alpha / 2) * se,
          study = study_labels,
          point_size = point_size,
          stat = "",
          .section = NA,
          .diamond = FALSE,
          .band = TRUE
        )
    } else {
      forest_data <- data_frame()
    }
    forest_data <- data_frame(
      estimate = model$b[1],
      se = model$se[1],
      conf.low = model$ci.lb[1],
      conf.high = model$ci.ub[1],
      study = model_label,
      n = sum(model$ni),
      point_size = NA,
      stat = "",
      .section = NA,
      .diamond = TRUE,
      .band = FALSE
    ) %>%
      bind_rows(forest_data, .)

    if (!is.null(additional_data) && is.data.frame(additional_data)) {
      forest_data <- bind_cols(forest_data, additional_data)
    }

    if (!is.null(show_stats)) {
      forest_data <- lapply(seq(show_stats), function(i) {
        stat_result <- rlang::eval_tidy(show_stats[[i]], model)
        stat_sign <- regmatches(stat_result, regexec("^[=<>]=?", stat_result))[[1]]
        if (length(stat_sign) == 0) {
          stat_result <- paste0("= ", stat_result)
        }
        sprintf('%s ~ "%s"', names(show_stats)[i], stat_result)
      }) %>% {
        data_frame(
          stat = paste0("paste(", paste(., collapse = ', "; ", '), ")"),
          .band = FALSE,
          .whole_row = TRUE
        )
      } %>% {
        bind_rows(list(forest_data, .))
      }
    }
    forest_data
  }

utils::globalVariables(
  c("I2", "QEp")
)
