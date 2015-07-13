#' Produce a forest plot based on a regression model
#'
#' @param model regression model produced by \code{\link[stats]{lm}},
#'   \code{\link[stats]{glm}}, \code{\link[survival]{coxph}}
#' @param panels \code{data.frame} with details of the panels that make up the plot (See Details)
#' @param exponentiate whether the numbers on the x scale should be exponentiated for plotting
#' @param colour colour of the point estimate and error bars
#' @param shape shape of the point estimate
#' @param banded whether to show light grey bands behind alternate rows
#' @param funcs optional list of functions required for formatting \code{panels$display}
#' @param factor_separate_line whether to show the factor variable name on a separate line
#'
#' @return A ggplot ready for display or saving
#'
#' @details This function takes the model output from one of the common model functions in
#'   R (e.g. \code{\link[stats]{lm}}, \code{\link[stats]{glm}},
#'  \code{\link[survival]{coxph}}).
#'
#'  The \code{panels} parameter is a \code{data.frame} with columns
#'  \code{display} and \code{width} and, optionally, \code{display_reference},
#'  \code{heading}, \code{hjust} and \code{fontface}.
#'  \code{display} indicates which column to display as text from the standard ones produced by
#'  \code{\link[broom]{tidy}} and in addition
#'  \code{variable} (the term in the model, for factors without the level),
#'  \code{level} (the level of factors),
#'  \code{reference} (TRUE for the reference level of a factor).
#'  It can also be a formula using these columns. The function \code{trans} is definded to be the
#'  transformation between the coefficients and the scales (e.g. \code{exp}). Other functions not
#'  in base R can be provided as a \code{list} with the parameter \code{funcs}.
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

forest_model <- function(model,
                         panels = default_forest_panels(factor_separate_line = factor_separate_line),
                         exponentiate = NULL, colour = "black", shape = 15,
                         banded = TRUE, funcs = NULL,
                         factor_separate_line = FALSE) {
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
  if (is.null(panels$fontface)) {
    panels$fontface <- "plain"
  }
  if (is.null(panels$heading)) {
    panels$heading <- panels$display
  }
  if (is.null(panels$display_reference)) {
    panels$display_reference <- panels_display
  }
  if (is.null(panels$display_na)) {
    panels$display_na <- panels_display
  }
  forest_terms <- data_frame(variable = names(attr(model$terms, "dataClasses"))[-1],
                             term_label = attr(model$terms, "term.labels"),
                             class = attr(model$terms, "dataClasses")[-1]) %>%
    rowwise %>% do({
      if (.$class == "factor") {
        tab <- table(data[, .$variable])
        out <- cbind(.,
                   data_frame(level = names(tab),
                   level_no = 1:length(tab),
                   n = as.integer(tab)))
        if (factor_separate_line) {
          out <- rbind(cbind(., data_frame(level = NA, level_no = NA, n = NA)),
                       out)
        }
        out
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
      estimate = ifelse(reference, 0, estimate),
      variable = ifelse(is.na(level_no) | (level_no == 1 & !factor_separate_line), variable, NA)
    )

  forest_min_max <- range(c(forest_terms$conf.low, forest_terms$conf.high), na.rm = TRUE)

  panels <- panels %>% mutate(
    display_reference = ifelse(is.na(display_reference), display, display_reference),
    display_na = ifelse(is.na(display_na), display, display_na),
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

  ft_for_eval <- c(as.list(forest_terms), trans = trans, funcs)
  display_panels <- panels %>%
    filter(!is.na(display) & display != "forest")
  forest_text <- rowwise(display_panels) %>%
    do({
      data_frame(x = .$text_x,
                 y = forest_terms$y,
                 hjust = .$hjust,
                 label = as.character(ifelse(forest_terms$reference,
                                             lazyeval::lazy_eval(.$display_reference, ft_for_eval),
                                      ifelse(!is.na(forest_terms$estimate),
                                             lazyeval::lazy_eval(.$display, ft_for_eval),
                                             lazyeval::lazy_eval(.$display_na, ft_for_eval)))),
                 fontface = .$fontface)
    })
  forest_text <- rbind(forest_text, forest_headings)

  forest_rectangles <- data_frame(xmin = min(panels$abs_x),
                                  xmax = max(panels$abs_end_x),
                                  y = seq(max(forest_terms$y), 1, -2),
                                  ymin = y - 0.5,
                                  ymax = y + 0.5)

  forest_theme <- function() {
    theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
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
    log_range <- floor(log10(forest_range[1])):ceiling(log10(forest_range[2]))
    step <- if (length(log_range) < 6) 2 else 5
    forest_breaks <- unlist(lapply(log_range, make_range, step = step)) %>%
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
    geom_point(aes(estimate, y), size = 5, shape = shape, colour = colour, na.rm = TRUE) +
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
    scale_y_continuous(expand = c(0, 0)) +
    forest_theme()
  main_plot
}

#' Default panels for forest_model
#'
#' @param measure label for main forest plot
#' @param factor_separate_line changes defaults for widths of variable depending on whether
#'   factors have their name on separate line
#'
#' @return `data.frame` ready to be passed to `forest_model`
#' @export
#'
default_forest_panels <- function(measure = "Hazard ratio", factor_separate_line = FALSE) {
  panels <-
    data.frame(
    display = c(
      NA, "variable", "level", "n", NA, "forest", NA,
      'sprintf("%0.2f (%0.2f-%0.2f)", trans(estimate), trans(conf.low), trans(conf.high))',
      'sprintf("%0.3f", p.value)', NA
    ),
    display_reference = c(rep(NA, 7), '"Reference\"', '""', NA),
    display_na = c(rep(NA, 7), '""', '""', NA),
    heading = c(NA, "Variable", NA, "N", NA, measure, NA, NA, "p", NA),
    hjust = c(NA, 0, 0, 1, NA, 0.5, NA, 0, 1, NA),
    width = c(0.03, 0.1, 0.07, 0.05, 0.03, 0.55, 0.03, 0.12, 0.05, 0.03),
    fontface = c("plain", "bold", rep("plain", 8)),
    stringsAsFactors = FALSE
  )
  if (factor_separate_line) {
    panels$width[2:3] <- c(0.02, 0.1)
  }
  panels
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
  "fontface",
  "panels_display",
  "reference",
  "display_reference",
  "display_na",
  "rel_width",
  "abs_end_x"
))
