# Recalculate widths of panels using actual text
# Works using either the current device width or a user-specified width
recalculate_width_panels <-
  function(panel_positions, mapped_text, mapped_data, recalculate_width, format_options,
           theme) {
    # Convert from default pointsize to current text size
    cex <- 2.845 * format_options$text_size / graphics::par("ps") * 1.2
    if (identical(recalculate_width, TRUE)) {
      recalculate_width <- graphics::par("din")[1]
    }
    n_text <- max(lengths(mapped_text))

    family <- if (is.na(theme$text$family) || theme$text$family == "") "sans" else theme$text$family
    fonts <- as.integer(factor(panel_positions$fontface, levels = c("plain", "bold")))


    measured_widths <- lapply(seq(nrow(panel_positions)), function(i) {
      if (!is.null(mapped_text[[i]])) {
        mt <- mapped_text[[i]]
        mt[is.na(mt)] <- ""
        if (panel_positions$parse[i]) {
          mt <- lapply(mt, function(x) parse(text = x))
          mt[lengths(mt) == 0] <- ""
        }
        mt[mapped_data$whole_row] <- ""
        widths <- vapply(mt, graphics::strwidth, numeric(1), "inches", cex = cex,
                 family = family, font = fonts[i]) / recalculate_width
      } else {
        widths <- rep(panel_positions$width[i], n_text)
      }
      if (!is.na(panel_positions$heading[i])) {
        widths <- c(graphics::strwidth(panel_positions$heading[i], "inches", cex = cex,
                             family = family, font = 2) / recalculate_width,
                    widths)
      } else {
        widths <- c(0, widths)
      }
    })


    measured_widths <- do.call("cbind", measured_widths)

    if (any(!is.na(panel_positions$width_group))) {
      for (i in unique(stats::na.omit(panel_positions$width_group))) {
        in_group <- which(panel_positions$width_group == i)
        cum_fixed_width <- cumsum(c(0, panel_positions$width[in_group[-length(in_group)]]))
        effective_width <- apply(measured_widths[, in_group, drop = FALSE], 1, function(x)
          max(x + cum_fixed_width))
        measured_widths[, in_group[-length(in_group)]] <-
          rep(panel_positions$width[in_group[-length(in_group)]], each = n_text + 1)
        measured_widths[, in_group[length(in_group)]] <-
          effective_width
      }
    }
    max_widths <- apply(measured_widths, 2, max)
    panel_positions$width <- max_widths
    forest_panel <- which(panel_positions$item == "forest")
    panel_positions$width[forest_panel] <- 1 - sum(max_widths[-forest_panel])
    if (panel_positions$width[forest_panel] < measured_widths[1, forest_panel]) {
      panel_positions$width[forest_panel] <- measured_widths[1, forest_panel]
      warning("Unable to resize forest panel to be smaller than its heading; consider a smaller text size")

    } else if (panel_positions$width[forest_panel] < 0.1) {
      panel_positions$width[forest_panel] <- 0.1
      warning("Unable to resize forest panel to be smaller than 10% of width")
    }
    panel_positions
  }
