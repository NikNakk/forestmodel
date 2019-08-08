# This code taken directly from metafor
# Not called from package as is a private function

# Set measure for rma objects
#
# @param measure measure abbreviation from rma object
# @param transf.char character name of transform function
# @param atransf.char character name of transform function for axes
# @param gentype type of outcome
#
# @return name of estimate
rma_setlab <- function(measure, transf.char, atransf.char, gentype) {
  if (gentype == 1) {
    lab <- "Observed Outcome"
  }
  if (gentype == 2) {
    lab <- "Overall Estimate"
  }
  if (!is.null(measure)) {
    if (measure == "RR") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Log Relative Risk"
      }
      else {
        lab <- "Transformed Log Relative Risk"
        if (atransf.char == "exp" || atransf.char ==
          "transf.exp.int") {
          lab <- "Relative Risk (log scale)"
        }
        if (transf.char == "exp" || transf.char == "transf.exp.int") {
          lab <- "Relative Risk"
        }
      }
    }
    if (is.element(measure, c(
      "OR", "PETO", "D2OR", "D2ORN",
      "D2ORL"
    ))) {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Log Odds Ratio"
      }
      else {
        lab <- "Transformed Log Odds Ratio"
        if (atransf.char == "exp" || atransf.char ==
          "transf.exp.int") {
          lab <- "Odds Ratio (log scale)"
        }
        if (transf.char == "exp" || transf.char == "transf.exp.int") {
          lab <- "Odds Ratio"
        }
      }
    }
    if (measure == "RD") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Risk Difference"
      }
      else {
        lab <- "Transformed Risk Difference"
      }
    }
    if (measure == "AS") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Arcsine Transformed Risk Difference"
      }
      else {
        lab <- "Transformed Arcsine Transformed Risk Difference"
      }
    }
    if (measure == "PHI") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Phi Coefficient"
      }
      else {
        lab <- "Transformed Phi Coefficient"
      }
    }
    if (measure == "YUQ") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Yule's Q"
      }
      else {
        lab <- "Transformed Yule's Q"
      }
    }
    if (measure == "YUY") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Yule's Y"
      }
      else {
        lab <- "Transformed Yule's Y"
      }
    }
    if (measure == "IRR") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Log Incidence Rate Ratio"
      }
      else {
        lab <- "Transformed Log Incidence Relative Risk"
        if (atransf.char == "exp" || atransf.char ==
          "transf.exp.int") {
          lab <- "Incidence Rate Ratio (log scale)"
        }
        if (transf.char == "exp" || transf.char == "transf.exp.int") {
          lab <- "Incidence Rate Ratio"
        }
      }
    }
    if (measure == "IRD") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Incidence Rate Difference"
      }
      else {
        lab <- "Transformed Incidence Rate Difference"
      }
    }
    if (measure == "IRSD") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Square-Root Transformed Incidence Rate Difference"
      }
      else {
        lab <- "Transformed Square-Root Transformed Incidence Rate Difference"
      }
    }
    if (measure == "MD") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Mean Difference"
      }
      else {
        lab <- "Transformed Mean Difference"
      }
    }
    if (is.element(measure, c(
      "SMD", "SMDH", "PBIT", "OR2D",
      "OR2DN", "OR2DL"
    ))) {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Standardized Mean Difference"
      }
      else {
        lab <- "Transformed Standardized Mean Difference"
      }
    }
    if (measure == "ROM") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Log Ratio of Means"
      }
      else {
        lab <- "Transformed Log Ratio of Means"
        if (atransf.char == "exp" || atransf.char ==
          "transf.exp.int") {
          lab <- "Ratio of Means (log scale)"
        }
        if (transf.char == "exp" || transf.char == "transf.exp.int") {
          lab <- "Ratio of Means"
        }
      }
    }
    if (measure == "RPB") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Point-Biserial Correlation"
      }
      else {
        lab <- "Transformed Point-Biserial Correlation"
      }
    }
    if (is.element(measure, c("COR", "UCOR", "RTET", "RBIS"))) {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Correlation Coefficient"
      }
      else {
        lab <- "Transformed Correlation Coefficient"
      }
    }
    if (measure == "ZCOR") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Fisher's z Transformed Correlation Coefficient"
      }
      else {
        lab <- "Transformed Fisher's z Transformed Correlation Coefficient"
        if (atransf.char == "transf.ztor" || atransf.char ==
          "transf.ztor.int") {
          lab <- "Correlation Coefficient"
        }
        if (transf.char == "transf.ztor" || transf.char ==
          "transf.ztor.int") {
          lab <- "Correlation Coefficient"
        }
      }
    }
    if (measure == "PR") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Proportion"
      }
      else {
        lab <- "Transformed Proportion"
      }
    }
    if (measure == "PLN") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Log Proportion"
      }
      else {
        lab <- "Transformed Log Proportion"
        if (atransf.char == "exp" || atransf.char ==
          "transf.exp.int") {
          lab <- "Proportion (log scale)"
        }
        if (transf.char == "exp" || transf.char == "transf.exp.int") {
          lab <- "Proportion"
        }
      }
    }
    if (measure == "PLO") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Log Odds"
      }
      else {
        lab <- "Transformed Log Odds"
        if (atransf.char == "transf.ilogit" || atransf.char ==
          "transf.ilogit.int" || atransf.char == "plogis") {
          lab <- "Proportion (logit scale)"
        }
        if (transf.char == "transf.ilogit" || transf.char ==
          "transf.ilogit.int" || transf.char == "plogis") {
          lab <- "Proportion"
        }
        if (atransf.char == "exp" || atransf.char ==
          "transf.exp.int") {
          lab <- "Odds (log scale)"
        }
        if (transf.char == "exp" || transf.char == "transf.exp.int") {
          lab <- "Odds"
        }
      }
    }
    if (measure == "PAS") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Arcsine Transformed Proportion"
      }
      else {
        lab <- "Transformed Arcsine Transformed Proportion"
        if (atransf.char == "transf.iarcsin" || atransf.char ==
          "transf.iarcsin.int") {
          lab <- "Proportion (arcsine scale)"
        }
        if (transf.char == "transf.iarcsin" || transf.char ==
          "transf.iarcsin.int") {
          lab <- "Proportion"
        }
      }
    }
    if (measure == "PFT") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Double Arcsine Transformed Proportion"
      }
      else {
        lab <- "Transformed Double Arcsine Transformed Proportion"
        if (atransf.char == "transf.ift.hm") {
          lab <- "Proportion"
        }
        if (transf.char == "transf.ift.hm") {
          lab <- "Proportion"
        }
      }
    }
    if (measure == "IR") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Incidence Rate"
      }
      else {
        lab <- "Transformed Incidence Rate"
      }
    }
    if (measure == "IRLN") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Log Incidence Rate"
      }
      else {
        lab <- "Transformed Log Incidence Rate"
        if (atransf.char == "exp" || atransf.char ==
          "transf.exp.int") {
          lab <- "Incidence Rate (log scale)"
        }
        if (transf.char == "exp" || transf.char == "transf.exp.int") {
          lab <- "Incidence Rate"
        }
      }
    }
    if (measure == "IRS") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Square-Root Transformed Incidence Rate"
      }
      else {
        lab <- "Transformed Square-Root Transformed Incidence Rate"
        if (atransf.char == "transf.isqrt" || atransf.char ==
          "transf.isqrt.int") {
          lab <- "Incidence Rate (square-root scale)"
        }
        if (transf.char == "transf.isqrt" || transf.char ==
          "transf.isqrt.int") {
          lab <- "Incidence Rate"
        }
      }
    }
    if (measure == "IRFT") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Freeman-Tukey Transformed Incidence Rate"
      }
      else {
        lab <- "Transformed Freeman-Tukey Transformed Incidence Rate"
      }
    }
    if (measure == "MN") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Mean"
      }
      else {
        lab <- "Transformed Mean"
      }
    }
    if (measure == "MC") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Mean Change"
      }
      else {
        lab <- "Transformed Mean Change"
      }
    }
    if (is.element(measure, c("SMCC", "SMCR", "SMCRH"))) {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Standardized Mean Change"
      }
      else {
        lab <- "Transformed Standardized Mean Change"
      }
    }
    if (measure == "ROMC") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Log Ratio of Means"
      }
      else {
        lab <- "Transformed Log Ratio of Means"
        if (atransf.char == "exp" || atransf.char ==
          "transf.exp.int") {
          lab <- "Ratio of Means (log scale)"
        }
        if (transf.char == "exp" || transf.char == "transf.exp.int") {
          lab <- "Ratio of Means"
        }
      }
    }
    if (measure == "ARAW") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Coefficient alpha"
      }
      else {
        lab <- "Transformed Coefficient alpha"
      }
    }
    if (measure == "AHW") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Transformed Coefficient alpha"
      }
      else {
        lab <- "Transformed Coefficient alpha"
        if (atransf.char == "transf.iahw") {
          lab <- "Coefficient alpha"
        }
        if (transf.char == "transf.iahw") {
          lab <- "Coefficient alpha"
        }
      }
    }
    if (measure == "ABT") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- "Transformed Coefficient alpha"
      }
      else {
        lab <- "Transformed Coefficient alpha"
        if (atransf.char == "transf.iabt") {
          lab <- "Coefficient alpha"
        }
        if (transf.char == "transf.iabt") {
          lab <- "Coefficient alpha"
        }
      }
    }
  }
  return(lab)
}
