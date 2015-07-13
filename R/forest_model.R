#' Produce a forest plot based on a regression model
#'
#' @param model regression model produced by `lm`, `glm`, `coxph`
#' @param panels `data.frame` with details of the panels that make up the plot (See Details)
#' @param exponentiate whether the numbers on the x scale should be exponentiated for plotting
#' @param colour colour of the point estimate and error bars
#' @param shape shape of the point estimate
#' @param banded whether to show light grey bands behind alternate rows
#' @param p_formatter function to format the p values for display
#'
#' @return a ggplot ready for display or saving
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
      (inherits(model, "glm") && model$family$family == "logit")
  }
  if (exponentiate) trans <- exp else trans <- I
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
      conf_int = ifelse(is.na(level_no) | level_no > 1,
                        sprintf("%0.2f (%0.2f-%0.2f)", trans(estimate), trans(conf.low), trans(conf.high)),
                        "Reference"),
      p_format = ifelse(is.na(level_no) | level_no > 1,
                        p_formatter(p.value),
                        ""),
      estimate = ifelse(is.na(level_no) | level_no > 1, estimate, 0)
    )

  panels[nrow(panels) + 1, ] <- data.frame("end", NA, NA, NA, NA, stringsAsFactors = FALSE)
  forest_min_max <- range(c(forest_terms$conf.low, forest_terms$conf.high), na.rm = TRUE)
  panels <- panels %>% mutate(
    rel_x = cumsum(c(0, width[-n()])),
    rel_x = (rel_x - rel_x[section == "forest"]) / width[section == "forest"],
    abs_x = rel_x * diff(forest_min_max) + forest_min_max[1],
    abs_width = c(diff(abs_x), NA),
    text_x = ifelse(hjust == 0, abs_x,
                    ifelse(hjust == 0.5, abs_x + abs_width / 2, abs_x + abs_width))
  )

  forest_lines <- panels %>%
    filter(section %in% c("forest_margin", "forest")) %>%
    {expand.grid(y = c(0.5, max(forest_terms$y) + 1.5),
                 x = .$abs_x[c(1, 3)] + .$abs_width[c(1, 3)] / 2)} %>%
    rbind(data.frame(y = max(forest_terms$y) + 0.5, x = range(panels$abs_x))) %>%
    rbind(data.frame(y = .$y[1:2], x = 0)) %>%
    cbind(group = rep(1:4, each = 2), linetype = rep(c("solid", "dashed"), c(6, 2)))

  forest_headings <- panels %>% filter(!is.na(heading)) %>%
    transmute(
      x = text_x,
      y = nrow(forest_terms) + 1,
      hjust = hjust,
      label = heading,
      fontface = "bold"
    )

  forest_text <- panels %>%
    filter(!is.na(display) & display != "forest") %>%
    group_by(display) %>%
    do({
      data_frame(x = .$text_x,
                 y = forest_terms$y,
                 hjust = .$hjust,
                 label = as.character(forest_terms[[.$display]]),
                 fontface = "plain")
    }) %>%
    ungroup %>%
    select(-display) %>%
    rbind(forest_headings)

  forest_rectangles <- data_frame(xmin = min(panels$abs_x),
                                  xmax = max(panels$abs_x),
                                  y = seq(max(forest_terms$y), 1, -2),
                                  ymin = y - 0.5,
                                  ymax = y + 0.5)

  forest_theme <- function() {
    theme_minimal() +
      theme(axis.ticks.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank()
      )
  }

  forest_range <- trans(forest_min_max)
  if (identical(trans, exp)) {
    forest_breaks <- log(c(
      if (forest_range[1] < 0.1) seq(max(0.02, ceiling(forest_range[1] / 0.02) * 0.02), 0.1, 0.02),
      if (forest_range[1] < 0.8) seq(max(0.2, ceiling(forest_range[1] / 0.2) * 0.2), 0.8, 0.2),
      1,
      if (forest_range[2] > 2) seq(2, min(10, floor(forest_range[2] / 2) * 2), 2),
      if (forest_range[2] > 20) seq(20, min(100, floor(forest_range[2] / 20) * 20), 20)
    ))
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
  panels <- data.frame(section = rep(c("left_margin", "left", "forest_margin", "forest", "forest_margin", "right", "right_margin"),
                                     c(1, 3, 1, 1, 1, 2, 1)),
                       display = c(NA, "variable", "level", "n", NA, "forest", NA, "conf_int","p_format", NA),
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
