library(survival)
test_that("forest_model function returns expected output", {
  pretty_lung <- lung %>%
    transmute(time,
              status,
              Age = age,
              Sex = factor(sex, labels = c("Male", "Female")),
              ECOG = factor(ph.ecog),
              `Meal Cal` = meal.cal
    )

  mdl <- coxph(Surv(time, status) ~ ., pretty_lung)

  panels <- default_forest_panels(mdl)

  fixed_widths <- c(0.03, 0.099, 0.078, 0.041, 0.03, 0.41, 0.03, 0.19, 0.06, 0.03)
  for (i in seq_along(fixed_widths)) {
    panels[[i]]$width <- fixed_widths[i]
  }

  fm <-
    forest_model(
      mdl,
      return_data = TRUE,
      panels = panels,
      recalculate_width = FALSE,
      recalculate_height = FALSE
    )

  vdiffr::expect_doppelganger("Sample forest_model plot for lung", fm$plot)
})
