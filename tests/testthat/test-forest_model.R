test_that("forest_model function returns expected output", {
  pretty_lung <- lung %>%
    transmute(time,
              status,
              Age = age,
              Sex = factor(sex, labels = c("Male", "Female")),
              ECOG = factor(lung$ph.ecog),
              `Meal Cal` = meal.cal
    )

  fm <- forest_model(coxph(Surv(time, status) ~ ., pretty_lung), return_data = TRUE)

  vdiffr::expect_doppelganger("Sample forest_model plot for lung", fm$plot)
})
