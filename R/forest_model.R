#' Produce a forest plot based on a regression model
#'
#' @param model regression model produced by \code{\link[stats]{lm}},
#'   \code{\link[stats]{glm}}, \code{\link[survival]{coxph}}
#' @param panels \code{data.frame} with details of the panels that make up the plot (See Details)
#' @param exponentiate whether the numbers on the x scale should be exponentiated for plotting
#' @param colour colour of the point estimate and error bars
#' @param shape shape of the point estimate
#' @param banded whether to show light grey bands behind alternate rows
#'
#' @return A ggplot ready for display or saving
#'
#' @details This function takes the model output from one of the common model functions in
#'   R (e.g. \code{\link[stats]{lm}}, \code{\link[stats]{glm}},
#'  \code{\link[survival]{coxph}}).
#'
#'  The \code{panels} parameter is a \code{data.frame} with columns
#'  \code{display} and \code{width} and, optionally, \code{display_reference},
#'  \code{heading} and \code{hjust}.
#'  \code{display} indicates which column to display as text from the standard ones produced by
#'  \code{\link[broom]{tidy}} and in addition
#'  \code{variable} (the term in the model, for factors without the level),
#'  \code{level} (the level of factors),
#'  \code{reference} (TRUE for the reference level of a factor).
#'  It can also be a formula using these columns either as a string or as a quoted expression.
#'  \code{display_reference} allows for an alternative display for reference levels of factors
#'  which typically do not have data within the model output.
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
#' print(forest_model(lm(outcome ~ ., data_for_lm), default_forest_panels("Estimate")))
#'
#' data_for_logistic <- data_for_lm %>% mutate(
#'   outcome = (0.5 * (x - 4) * (y - 3) * (z - 2) + rnorm(100, 0, 0.05)) > 0.5
#' )
#'
#' print(forest_model(glm(outcome ~ ., binomial(), data_for_logistic),
#'   default_forest_panels("Odds ratio")))

forest_model <- function(model, panels = default_forest_panels(), exponentiate = NULL,
                         colour = "black", shape = 15, banded = TRUE,
                         p_formatter = function(x) sprintf("%0.3f", x)) {
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
  stopifnot(is.data.frame(panels),
            c("display", "width") %in% names(panels),
            "forest" %in% panels$display)
  if (is.null(panels$hjust)) {
    panels$hjust <- 0
  }
  if (is.null(panels$heading)) {
    panels$heading <- panels$display
  }
  if (is.null(panels$display_reference)) {
    panels$display_reference <- panels_display
  }
  forest_terms <- data.frame(variable = names(attr(model$terms, "dataClasses"))[-1],
                             term_label = attr(model$terms, "term.labels"),
                             class = attr(model$terms, "dataClasses")[-1], stringsAsFactors = FALSE,
                             row.names = NULL) %>%
    group_by(term_no = row_number()) %>% do({
      if (.$class == "factor") {
        tab <- table(data[, .$variable])
        data.frame(.,
                   level = names(tab),
                   level_no = 1:length(tab),
                   n = as.integer(tab),
                   stringsAsFactors = FALSE, row.names = NULL)
      } else {
        data.frame(., level = NA, level_no = NA, n = sum(!is.na(data[, .$variable])),
                   stringsAsFactors = FALSE)
      }
    }) %>%
    ungroup %>%
    mutate(term = paste0(term_label, replace(level, is.na(level), "")),
           y = n():1
    ) %>%
    left_join(tidy_model, by = "term") %>%
    mutate(
      reference = ifelse(is.na(level_no), FALSE, level_no == 1),
      estimate = ifelse(reference, 0, estimate)
    )

  forest_min_max <- range(c(forest_terms$conf.low, forest_terms$conf.high), na.rm = TRUE)

  panels <- panels %>% mutate(
    display_reference = ifelse(is.na(display_reference), display, display_reference),
    rel_width = width / width[which(display == "forest")],
    rel_x = cumsum(c(0, width[-n()])),
    rel_x = (rel_x - rel_x[which(display == "forest")]) / width[which(display == "forest")],
    abs_x = rel_x * diff(forest_min_max) + forest_min_max[1],
    abs_width = rel_width * diff(forest_min_max),
    abs_end_x = abs_x + abs_width,
    text_x = ifelse(hjust == 0, abs_x,
                    ifelse(hjust == 0.5, abs_x + abs_width / 2, abs_end_x))
  )

  forest_lines <- panels %>%
    filter(abs(row_number() - which(display == "forest")) <= 1) %>%
    {expand.grid(y = c(0.5, max(forest_terms$y) + 1.5),
                 x = .$abs_x[c(1, 3)] + .$abs_width[c(1, 3)] / 2)} %>%
    rbind(data.frame(y = max(forest_terms$y) + 0.5, x = c(min(panels$abs_x), max(panels$abs_end_x)))) %>%
    rbind(data.frame(y = c(0.5, max(forest_terms$y) + 0.5), x = 0)) %>%
    cbind(group = rep(1:4, each = 2), linetype = rep(c("solid", "dashed"), c(6, 2)))

  forest_headings <- panels %>% filter(!is.na(heading)) %>%
    transmute(
      x = text_x,
      y = nrow(forest_terms) + 1,
      hjust = hjust,
      label = heading,
      fontface = "bold"
    )

  ft_for_eval <- c(as.list(forest_terms), trans = trans)
  forest_text <- panels %>%
    filter(!is.na(display) & display != "forest") %>%
    group_by(display) %>%
    do({
      data_frame(x = .$text_x,
                 y = forest_terms$y,
                 hjust = .$hjust,
                 label = as.character(ifelse(forest_terms$reference,
                                             lazyeval::lazy_eval(.$display_reference, ft_for_eval),
                                             lazyeval::lazy_eval(.$display, ft_for_eval))),
                 fontface = "plain")
    }) %>%
    ungroup %>%
    select(-display) %>%
    rbind(forest_headings)

  forest_rectangles <- data_frame(xmin = min(panels$abs_x),
                                  xmax = max(panels$abs_end_x),
                                  y = seq(max(forest_terms$y), 1, -2),
                                  ymin = y - 0.5,
                                  ymax = y + 0.5)

  forest_theme <- function() {
    theme_minimal() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank()
      )
  }

  forest_range <- trans(forest_min_max)
  if (identical(trans, exp)) {
    make_range <- function(log_cut, step) {
      if (log_cut <= 0) {
        cut <- 10 ^ log_cut
        divisor <- cut / 10 * step
        if (forest_range[1] < cut) {
          seq(max(divisor, ceiling(forest_range[1] / divisor) * divisor), cut, divisor)
        } else {
          NULL
        }
      } else {
        cut <- 10 ^ (log_cut - 1)
        divisor <- step * cut
        if (forest_range[2] > divisor) {
          seq(divisor, min(10 ^ log_cut, floor(forest_range[2] / divisor) * divisor), divisor)
        } else {
          NULL
        }
      }
    }
    forest_breaks <- unlist(lapply(floor(log10(forest_range[1])):ceiling(log10(forest_range[2])),
                                 make_range, step = 2)) %>%
      c(1) %>%
      unique %>%
      sort %>%
      log
  } else {
    divisor <- 10 ^ round(log10(diff(forest_range)) - 1)
    forest_breaks <- divisor * ceiling(forest_range[1] / divisor):floor(forest_range[2] / divisor)
  }

  main_plot <- ggplot(forest_terms, aes(y = y))
  if (banded) {
    main_plot <- main_plot +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                forest_rectangles, fill = "#EFEFEF")
  }
  main_plot <- main_plot +
    geom_point(aes(estimate, y), size = 5, shape = shape, colour = colour) +
    geom_errorbarh(aes(estimate,
                       xmin = conf.low,
                       xmax = conf.high,
                       y = y),
                   height = 0.15, colour = colour) +
    geom_line(aes(x = x, y = y, linetype = linetype, group = group),
              forest_lines) +
    geom_text(aes(x = x, y = y, label = label, hjust = hjust, fontface = fontface),
              forest_text, na.rm = TRUE) +
    scale_linetype_identity() +
    scale_alpha_identity() +
    scale_x_continuous(breaks = forest_breaks,
                       labels = sprintf("%g", trans(forest_breaks)),
                       expand = c(0, 0)) +
    forest_theme()
  main_plot
}

#' Default panels for forest_model
#'
#' @param measure label for main forest plot
#'
#' @return `data.frame` ready to be passed to `forest_model`
#' @export
#'
default_forest_panels <- function(measure = "Hazard ratio") {
  panels <- data.frame(display = c(NA, "variable", "level", "n", NA, "forest", NA,
                                   'sprintf("%0.2f (%0.2f-%0.2f)", trans(estimate), trans(conf.low), trans(conf.high))',
                                   'sprintf("%0.3f", p.value)', NA),
                       display_reference = c(rep(NA, 7), '"Reference\"', '""', NA),
                       heading = c(NA, "Variable", NA, "N", NA, measure, NA, NA, "p", NA),
                       hjust = c(NA, 0, 0, 1, NA, 0.5, NA, 0, 1, NA),
                       width = c(0.03, 0.1, 0.07, 0.05, 0.03, 0.55, 0.03, 0.12, 0.05, 0.03),
                       stringsAsFactors = FALSE)
}

# Work around for R CMD CHECK
utils::globalVariables(names = c(
  ".",
  "term_label",
  "level",
  "level_no",
  "estimate",
  "conf.low",
  "conf.high",
  "p.value",
  "width",
  "rel_x",
  "section",
  "abs_x",
  "hjust",
  "abs_width",
  "heading",
  "text_x",
  "display",
  "y",
  "xmin",
  "xmax",
  "ymin",
  "ymax",
  "x",
  "linetype",
  "group",
  "label",
  "fontface"
))
