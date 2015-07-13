#' Produce a forest plot based on a regression model
#'
#' @param model regression model produced by \code{\link[stats]{lm}},
#'   \code{\link[stats]{glm}}, \code{\link[survival]{coxph}}
#' @param panels \code{list} with details of the panels that make up the plot (See Details)
#' @param exponentiate whether the numbers on the x scale should be exponentiated for plotting
#' @param colour colour of the point estimate and error bars
#' @param shape shape of the point estimate
#' @param banded whether to show light grey bands behind alternate rows
#' @param funcs optional list of functions required for formatting \code{panels$display}
#' @param factor_separate_line whether to show the factor variable name on a separate line
#' @param forest_theme theme to apply to the plot
#'
#' @return A ggplot ready for display or saving
#'
#' @details This function takes the model output from one of the common model functions in
#'   R (e.g. \code{\link[stats]{lm}}, \code{\link[stats]{glm}},
#'  \code{\link[survival]{coxph}}).
#'
#'  The \code{panels} parameter is a \code{list} of lists each of which have an element
#'  \code{width}
#'  and, optionally, \code{display}, \code{display_na},
#'  \code{heading}, \code{hjust} and \code{fontface}.
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
                         factor_separate_line = FALSE, forest_theme = theme_forest()) {
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
  panel_forest_plot(forest_data = forest_terms, panels = panels, trans = trans,
                    funcs = funcs, forest_theme = forest_theme, colour = colour, shape = shape,
                    banded = banded)
}


#' Plot a forest plot with panels of text
#'
#' @param forest_data \code{data.frame} with the data needed for both the plot and text
#' @param forest_mapping mapping aesthetic created using \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_string}}
#' @param trans transform for scales
#'
#' @inheritParams forest_model
#'
#' @return A ggplot ready for display or saving
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom lazyeval lazy_eval
#'
#' @export
#'
panel_forest_plot <-
  function(forest_data,
           forest_mapping = aes(estimate, y, xmin = conf.low, xmax = conf.high),
           panels, trans = I, funcs = NULL, forest_theme = theme_~forest(),
           colour = "black", shape = 15, banded = TRUE) {
    fd_for_eval <- c(as.list(forest_data), trans = trans, funcs)
    max_y <- max(lazy_eval(forest_mapping$y, fd_for_eval))

    forest_min_max <- range(c(lazy_eval(forest_mapping$xmin, fd_for_eval),
                              lazy_eval(forest_mapping$xmax, fd_for_eval)), na.rm = TRUE)

    panel_positions <- lapply(panels, function(panel) {
      data_frame(width = panel$width,
                 display = paste(deparse(panel$display), collapse = "\n"),
                 hjust = panel$hjust %||% 0,
                 heading = panel$heading %||% NA,
                 fontface = panel$fontface %||% "plain")
    }) %>% rbind_all
    panel_positions <- panel_positions %>% mutate(
      rel_width = width / width[which(display == "~forest()")],
      rel_x = cumsum(c(0, width[-n()])),
      rel_x = (rel_x - rel_x[which(display == "~forest()")]) / width[which(display == "~forest()")],
      abs_x = rel_x * diff(forest_min_max) + forest_min_max[1],
      abs_width = rel_width * diff(forest_min_max),
      abs_end_x = abs_x + abs_width,
      text_x = ifelse(hjust == 0, abs_x,
                      ifelse(hjust == 0.5, abs_x + abs_width / 2, abs_end_x))
    )

    forest_lines <- panel_positions %>%
      filter(display == "~line()") %>%
      {expand.grid(y = c(0.5, max_y + 1.5),
                   x = .$abs_x + .$abs_width / 2)} %>%
      rbind(data.frame(y = max_y + 0.5, x = c(min(panel_positions$abs_x), max(panel_positions$abs_end_x)))) %>%
      rbind(data.frame(y = c(0.5, max_y + 0.5), x = 0)) %>%
      cbind(group = rep(1:4, each = 2), linetype = rep(c("solid", "dashed"), c(6, 2)))

    forest_headings <- panel_positions %>% filter(!is.na(heading)) %>%
      transmute(
        x = text_x,
        y = nrow(forest_data) + 1,
        hjust = hjust,
        label = heading,
        fontface = "bold"
      )

    forest_text <- lapply(1:length(panels), function(i) {
      if (!is.null(panels[[i]]$display) &&
          !(panel_positions$display[i] %in% c("~forest()", "~line()"))) {
        with(
          panel_positions[i, ],
          data_frame(x = text_x,
                     y = lazy_eval(forest_mapping$y, fd_for_eval),
                     hjust = hjust,
                     label = as.character(ifelse(!is.na(lazy_eval(forest_mapping$x, fd_for_eval)),
                                                 lazy_eval(panels[[i]]$display, fd_for_eval),
                                                 lazy_eval(panels[[i]]$display_na %||% panels[[i]]$display, fd_for_eval))),
                     fontface = fontface)
        )
      }
    }) %>% rbind_all
    forest_text <- rbind(forest_text, forest_headings)

    forest_rectangles <- data_frame(xmin = min(panel_positions$abs_x),
                                    xmax = max(panel_positions$abs_end_x),
                                    y = seq(max_y, 1, -2),
                                    ymin = y - 0.5,
                                    ymax = y + 0.5)

    forest_min_max_trans <- trans(forest_min_max)

    if (identical(trans, exp)) {
      make_range <- function(log_cut, step) {
        if (log_cut <= 0) {
          cut <- 10 ^ log_cut
          divisor <- cut / 10 * step
          if (forest_min_max_trans[1] < cut) {
            seq(max(divisor, ceiling(forest_min_max_trans[1] / divisor) * divisor), cut, divisor)
          } else {
            NULL
          }
        } else {
          cut <- 10 ^ (log_cut - 1)
          divisor <- step * cut
          if (forest_min_max_trans[2] > divisor) {
            seq(divisor, min(10 ^ log_cut, floor(forest_min_max_trans[2] / divisor) * divisor), divisor)
          } else {
            NULL
          }
        }
      }
      log_range <- floor(log10(forest_min_max_trans[1])):ceiling(log10(forest_min_max_trans[2]))
      step <- if (length(log_range) < 6) 2 else 5
      forest_breaks <- unlist(lapply(log_range, make_range, step = step)) %>%
        c(1) %>%
        unique %>%
        sort %>%
        log
    } else {
      divisor <- 10 ^ round(log10(diff(forest_min_max_trans)) - 1)
      forest_breaks <- divisor * ceiling(forest_min_max_trans[1] / divisor):floor(forest_min_max_trans[2] / divisor)
    }

    main_plot <- ggplot(forest_data, forest_mapping["y"])
    if (banded) {
      main_plot <- main_plot +
        geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  forest_rectangles, fill = "#EFEFEF")
    }
    main_plot <- main_plot +
      geom_point(forest_mapping[c("x", "y")], colour = colour, shape = shape, size = 5, na.rm = TRUE) +
      geom_errorbarh(forest_mapping[c("x", "y", "xmin", "xmax")],
                     colour = colour, height = 0.15) +
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
      forest_theme
    main_plot
  }

#' Default panels for forest_model
#'
#' @param measure label for main forest plot
#' @param factor_separate_line changes defaults for widths of variable depending on whether
#'   factors have their name on separate line
#'
#' @return `list` ready to be passed to `forest_model`
#' @export
#'
default_forest_panels <- function(measure = "Hazard ratio", factor_separate_line = FALSE) {
  list(list(width = 0.03),
       list(width = if (factor_separate_line) 0.02 else 0.1, display = ~variable, fontface = "bold", heading = "Variable"),
       list(width = if (factor_separate_line) 0.1 else 0.07, display = ~level),
       list(width = 0.05, display = ~n, hjust = 1, heading = "N"),
       list(width = 0.03, display = ~line()),
       list(width = 0.55, display = ~forest(), hjust = 0.5, heading = measure),
       list(width = 0.03, display = ~line()),
       list(width = 0.12,
            display = ~ifelse(reference, "Reference",
                              sprintf("%0.2f (%0.2f-%0.2f)",trans(estimate), trans(conf.low), trans(conf.high))),
            display_na = NA),
       list(width = 0.05, display = ~ifelse(reference, "", sprintf("%0.3f", p.value)), display_na = NA,
            hjust = 1, heading = "p"),
       list(width = 0.03)
  )
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


# Work around for R CMD CHECK
utils::globalVariables(names = c(
  ".",
  "term_label",
  "level",
  "level_no",
  "reference",
  "estimate",
  "variable",
  "estimate",
  "y",
  "conf.low",
  "conf.high",
  "width",
  "display",
  "rel_x",
  "rel_width",
  "abs_x",
  "abs_width",
  "hjust",
  "abs_end_x",
  "heading",
  "text_x",
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
