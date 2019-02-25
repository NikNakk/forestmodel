#' Plot a forest plot with panels of text
#'
#' @param forest_data \code{data.frame} with the data needed for both the plot and text
#' @param mapping mapping aesthetic created using \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_string}}
#' @param trans transform for scales
#'
#' @inheritParams forest_model
#'
#' @return A ggplot ready for display or saving
#'
#' @import ggplot2
#' @import dplyr
#' @import rlang
#'
#' @export
#'
panel_forest_plot <- function(forest_data,
           mapping = aes(estimate, xmin = conf.low, xmax = conf.high),
           panels = default_forest_panels(), trans = I, funcs = NULL,
           format_options = list(colour = "black", shape = 15, banded = TRUE, text_size = 5),
           theme = theme_forest(),
           limits = NULL, breaks = NULL,
           recalculate_width = TRUE, recalculate_height = TRUE,
           exclude_infinite_cis = TRUE) {

  stopifnot(is.list(panels), is.data.frame(forest_data))

  # Update older style panels to quosures
  for (i in seq_along(panels)) {
    if (!is.null(panels[[i]]$display)) {
      panels[[i]]$display <- as_quosure(panels[[i]]$display)
    }
    if (!is.null(panels[[i]]$display_na)) {
      panels[[i]]$display_na <- as_quosure(panels[[i]]$display_na)
    } else {
      panels[[i]]$display_na <- panels[[i]]$display
    }
  }

  format_options$colour <- format_options$colour %||% "black"
  format_options$shape <- format_options$shape %||% 15
  format_options$banded <- format_options$banded %||% TRUE
  format_options$text_size <- format_options$text_size %||% 5

  mapping$size <- mapping$size %||% 5
  mapping$whole_row <- mapping$whole_row %||% FALSE

  if (!is.null(mapping$section) && !all(is.na(eval_tidy(mapping$section, forest_data)))) {
    forest_data <- forest_data %>%
      mutate(
        .section = !!mapping$section,
        .whole_row = !!mapping$whole_row
      ) %>%
      group_by(.section) %>%
      do({
        bind_rows(
          tibble(
            .section = first(.$.section),
            .subheading = first(.$.section),
            .whole_row = TRUE
          ),
          as_tibble(.)
        )
      })
    mapping$subheading <- quo(.subheading)
    mapping$whole_row <- quo(.whole_row)
  }

  fd_for_eval <- c(as.list(forest_data), trans = trans, funcs)

  mapped_data <- lapply(mapping, function(el) {
    eval_tidy(el, fd_for_eval)
  }) %>%
    as_tibble() %>%
    default_cols(
      band = TRUE,
      diamond = FALSE,
      section = 1
    ) %>%
    mutate(
      diamond = if_else(is.na(diamond), FALSE, diamond),
      whole_row = if_else(is.na(whole_row), FALSE, whole_row),
      y = n():1
    )

  mapped_text <- lapply(seq(panels), function(i) {
    if (!is.null(panels[[i]]$display)) {

      as.character(ifelse(!is.na(mapped_data$x),
                          eval_tidy(panels[[i]]$display, fd_for_eval),
                          eval_tidy(panels[[i]]$display_na, fd_for_eval)))
    } else NULL
  })

  max_y <- max(mapped_data$y)

  panel_positions <- lapply(panels, function(panel) {
    tibble(
      width = panel$width %||% NA,
      item = panel$item %||% {if (!is.null(panel$display)) "text" else NA},
      display = quo_text(panel$display),
      hjust = as.numeric(panel$hjust %||% 0),
      heading = panel$heading %||% NA,
      fontface = panel$fontface %||% "plain",
      linetype = panel$linetype %||% {if (!is.na(item) && item == "vline") "solid" else NA},
      line_x = as.numeric(panel$line_x %||% NA),
      parse = as.logical(panel$parse %||% FALSE),
      width_group = panel$width_group %||% NA
    )
  }) %>%
    bind_rows()

  if (any(panel_positions$parse & panel_positions$fontface != "plain")) {
    warning("Fontface cannot be applied to parsed text; please use the plotmath functions (e.g. bold())")
  }

  if (any(is.na(panel_positions$width)) && !recalculate_width) {
    recalculate_width <- TRUE
    message("Some widths are undefined; defaulting to recalculate_width = TRUE")
  }

  if (sum(panel_positions$item == "forest", na.rm = TRUE) != 1) {
    stop("One panel must include item \"forest\".")
  }

  forest_panel <- which(panel_positions$item == "forest")

  if (is.null(limits)) {
    x_min_max <- c(mapped_data$xmin, mapped_data$xmax)
    x_min_max <- x_min_max[is.finite(x_min_max)]
    forest_min_max <- range(x_min_max)
    if (!is.na(forest_line_x <- panel_positions$line_x[forest_panel])) {
      if (forest_min_max[2] < forest_line_x) {
        forest_min_max[2] <- forest_line_x
        message("Resized limits to included dashed line in forest panel")
      }
      if (forest_min_max[1] > forest_line_x) {
        forest_min_max[1] <- forest_line_x
        message("Resized limits to included dashed line in forest panel")
      }
    }
  } else {
    forest_min_max <- limits
  }

  if (!is.null(recalculate_height) && !(identical(recalculate_height, FALSE))) {
    if (identical(recalculate_height, TRUE)) {
      recalculate_height <- graphics::par("din")[2]
    }
    max_text_size <- recalculate_height / (max_y + 1) / 1.3 * 25.4
    if (format_options$text_size > max_text_size) {
      format_options$text_size <- max_text_size
    }
  }

  if (!is.null(recalculate_width) && !(identical(recalculate_width, FALSE))) {
    panel_positions <-
      recalculate_width_panels(panel_positions, mapped_text = mapped_text,
                               mapped_data = mapped_data,
                               recalculate_width = recalculate_width,
                               format_options = format_options,
                               theme = theme)
  }

  panel_positions <- panel_positions %>%
    mutate(
      rel_width = width / width[forest_panel],
      rel_x = cumsum(c(0, width[-n()])),
      rel_x = (rel_x - rel_x[forest_panel]) / width[forest_panel],
      abs_x = rel_x * diff(forest_min_max) + forest_min_max[1],
      abs_width = rel_width * diff(forest_min_max),
      abs_end_x = abs_x + abs_width,
      text_x = ifelse(hjust == 0, abs_x,
                      ifelse(hjust == 0.5, abs_x + abs_width / 2, abs_end_x))
  )

  forest_vlines <- panel_positions %>%
    filter(item == "vline" | !is.na(linetype)) %>%
    transmute(
      x = case_when(
        !is.na(line_x) ~ line_x,
        hjust == 1 ~ abs_end_x,
        hjust == 0.5 ~ abs_x + abs_width / 2
      ),
      xend = x,
      y = 0.5,
      yend = if_else(is.na(line_x), max_y + 1.5, max_y + 0.5),
      linetype = linetype
    )

  forest_hlines <- tibble(
    x = c(min(panel_positions$abs_x), max(panel_positions$abs_end_x)),
    y = max_y + 0.5,
    linetype = "solid"
  )

  forest_headings <- panel_positions %>%
    filter(!is.na(heading)) %>%
    transmute(
      x = text_x,
      y = max_y + 1,
      hjust = hjust,
      label = heading,
      fontface = "bold",
      parse = FALSE
    )

  if (has_name(mapped_data, "subheading")) {
    forest_subheadings <- mapped_data %>%
      filter(!is.na(subheading)) %>%
      transmute(
        x = mean(forest_min_max),
        y,
        hjust = 0.5,
        label = subheading,
        fontface = "bold",
        parse = FALSE
      )
    forest_headings <- bind_rows(forest_headings, forest_subheadings)
  }

  if (any(mapped_data$whole_row)) {
    forest_whole_row_back <- mapped_data %>%
      filter(whole_row) %>%
      transmute(
        y,
        xmin = min(panel_positions$abs_x),
        xmax = max(panel_positions$abs_end_x),
        ymin = y - 0.5,
        ymax = y + 0.5
      )
    forest_hlines <- bind_rows(
      forest_hlines,
      forest_whole_row_back %>%
      rowwise %>% do({
        data_frame(
          x = rep(range(c(panel_positions$abs_x, panel_positions$abs_end_x)), 2),
          y = rep(.$y + c(-0.5, 0.5), each = 2),
          linetype = "solid"
        )
      })
    )
  }

  forest_text <- lapply(seq(panels), function(i) {
    if (!is.null(mapped_text[[i]])) {
      with(
        panel_positions[i, ],
        tibble(
          x = text_x,
          y = mapped_data$y,
          hjust = hjust,
          label = mapped_text[[i]],
          fontface = fontface,
          parse = parse)
      )
    }
  }) %>%
    bind_rows() %>%
    bind_rows(., forest_headings)

  if (format_options$banded) {
    forest_rectangles <- mapped_data %>%
      filter(band) %>%
      group_by(section) %>%
      filter(row_number() %% 2 == 1) %>%
      transmute(
        xmin = min(panel_positions$abs_x),
        xmax = max(panel_positions$abs_end_x),
        y = y,
        ymin = y - 0.5,
        ymax = y + 0.5
      )
  }

  if (any(mapped_data$diamond)) {
    forest_diamonds <- mapped_data %>%
      filter(diamond == TRUE) %>%
      rowwise %>% do({
        data_frame(x = c(.$xmin, .$x, .$xmax, .$x, .$xmin),
                   y = .$y + c(0, 0.15, 0, -0.15, 0)
        )
      }) %>%
      ungroup %>%
      mutate(group = (row_number() + 4) %/% 5)

    forest_hlines <- bind_rows(
      forest_hlines,
      mapped_data %>%
        filter(diamond) %>%
        rowwise %>% do({
          data_frame(
            x = rep(range(c(panel_positions$abs_x, panel_positions$abs_end_x)), 2),
            y = rep(.$y + c(-0.5, 0.5), each = 2),
            linetype = "solid"
          )
        })
    )
  }

  forest_hlines <- mutate(forest_hlines, group = (row_number() + 1) %/% 2)

  if (is.null(breaks)) {
    if (identical(trans, exp)) {
      breaks <- log(grDevices::axisTicks(log10(exp(forest_min_max)), TRUE))
    } else {
      breaks <- grDevices::axisTicks(forest_min_max, FALSE)
    }
    breaks <- breaks[breaks >= forest_min_max[1] & breaks <= forest_min_max[2]]
  }

  if (exclude_infinite_cis) {
    mapped_data <- mapped_data %>%
      mutate_at(
        vars(x, xmin, xmax),
        funs(if_else(
          is.infinite(x) | is.infinite(xmin) | is.infinite(xmax),
          NA_real_,
          .
          ))
      )
  }

  main_plot <- ggplot(forest_data)
  if (format_options$banded) {
    main_plot <- main_plot +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                forest_rectangles, fill = "#EFEFEF")
  }
  if (any(mapped_data$diamond)) {
    main_plot <- main_plot +
      geom_polygon(aes(x, y, group = group), forest_diamonds, fill = format_options$colour)
  }
  main_plot <- main_plot +
    geom_point(aes(x, y, size = size), filter(mapped_data, !diamond),
               colour = format_options$colour, shape = format_options$shape, na.rm = TRUE) +
    geom_errorbarh(aes(y = y, xmin = xmin, xmax = xmax),
                   filter(mapped_data, !diamond & !(is.na(xmin) & is.na(xmax))),
                   colour = format_options$colour, height = 0.15) +
    geom_segment(aes(x, y, xend = xend, yend = yend, linetype = linetype),
              forest_vlines)
  if (any(mapped_data$whole_row)) {
    main_plot <- main_plot +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                forest_whole_row_back, fill = "#FFFFFF")
  }
  for (parse_type in unique(forest_text$parse)) {
    main_plot <- main_plot +
      geom_text(aes(x, y, label = label, hjust = hjust, fontface = fontface),
                filter(forest_text, parse == parse_type), na.rm = TRUE, parse = parse_type,
                size = format_options$text_size)
  }
  main_plot <- main_plot +
    geom_line(aes(x, y, linetype = linetype, group = group),
              forest_hlines) +
    scale_linetype_identity() +
    scale_alpha_identity() +
    guides(size = "none") +
    scale_x_continuous(breaks = breaks,
                       labels = sprintf("%g", trans(breaks)),
                       expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme
  main_plot
}
