test_that("forest_rma works", {
  if (require("metafor")) {
    data("dat.bcg")
    dat <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
    model <- rma(yi, vi, data = dat)
    panels <- forest_panels(
      Study = ~study,
      N = ~n, ~vline, `Log Relative Risk` = ~ forest(line_x = 0),
      ~ spacer(space = 0.10),
      ~ sprintf("%0.3f (%0.3f, %0.3f)", estimate, conf.low, conf.high)
    )
    fixed_widths <- c(0.03, 0.335, 0.095, 0.03, 0.239, 0.1, 0.284, 0.03)
    for (i in seq_along(fixed_widths)) {
      panels[[i]]$width <- fixed_widths[i]
    }

    fr <- forest_rma(model,
      panels = panels,
      study_labels = paste(dat.bcg$author, dat.bcg$year),
      trans = exp,
      recalculate_width = FALSE,
      recalculate_height = FALSE
    )

    vdiffr::expect_doppelganger("Sample forest RMA plot for dat.bcg", fr)
  }
})
