<!-- README.md is generated from README.Rmd. Please edit that file -->
forestmodel
===========

This is an R package to generate forest plots of the coefficients of models produced by `lm`, `glm`, `survival::coxph`, etc.

The main function is `forest_model`, with a helper function `default_forest_panels` to produce the necessary `panels` `data.frame`.

Example
-------

``` r
library("forestmodel")
library("survival")
library("dplyr")
pretty_lung <- lung %>%
  transmute(time,
            status,
            Age = age,
            Sex = factor(sex, labels = c("Male", "Female")),
            ECOG = factor(lung$ph.ecog),
            `Meal Cal` = meal.cal)

print(forest_model(coxph(Surv(time, status) ~ ., pretty_lung)))
```

Installation
------------

The package can be installed using `install.packages`. It needs Hadley Wickham's `broom`, `dplyr`, `gpplot2` and `lazyeval` packages.

Development takes place on the github repository <https://github.com/NikNakk/forestmodel/>.

[![Build Status](https://travis-ci.org/NikNakk/forestmodel.svg?branch=master)](https://travis-ci.org/NikNakk/forestmodel) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/NikNakk/forestmodel?branch=master&svg=true)](https://ci.appveyor.com/project/NikNakk/forestmodel)
