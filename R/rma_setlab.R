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
rma_setlab <- function(measure, transf.char, atransf.char, gentype, short=FALSE) {
  if (gentype == 1) {
    lab <- "Observed Outcome"
  }
  if (gentype == 2) {
    lab <- "Overall Estimate" # for forest.cumul.rma() function
  }
  if (gentype == 3) {
    lab <- "Estimate"         # for header
  }


  if (!is.null(measure)) {

    if (is.element(measure, c("RR","MPRR"))) {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Log[RR]", "Log Risk Ratio")
      } else {
        lab <- ifelse(short, lab, "Transformed Log Risk Ratio")
        if (atransf.char == "exp" || atransf.char == "transf.exp.int")
          lab <- ifelse(short, "Risk Ratio", "Risk Ratio (log scale)")
        if (transf.char == "exp" || transf.char == "transf.exp.int")
          lab <- ifelse(short, "Risk Ratio", "Risk Ratio")
      }
    }
    if (is.element(measure, c("OR","PETO","D2OR","D2ORN","D2ORL","MPOR","MPORC","MPPETO"))) {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Log[OR]", "Log Odds Ratio")
      } else {
        lab <- ifelse(short, lab, "Transformed Log Odds Ratio")
        if (atransf.char == "exp" || atransf.char == "transf.exp.int")
          lab <- ifelse(short, "Odds Ratio", "Odds Ratio (log scale)")
        if (transf.char == "exp" || transf.char == "transf.exp.int")
          lab <- ifelse(short, "Odds Ratio", "Odds Ratio")
      }
    }
    if (is.element(measure, c("RD","MPRD"))) {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Risk Difference", "Risk Difference")
      } else {
        lab <- ifelse(short, lab, "Transformed Risk Difference")
      }
    }
    if (measure == "AS") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Arcsine RD", "Arcsine Transformed Risk Difference")
      } else {
        lab <- ifelse(short, lab, "Transformed Arcsine Transformed Risk Difference")
      }
    }
    if (measure == "PHI") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Phi", "Phi Coefficient")
      } else {
        lab <- ifelse(short, lab, "Transformed Phi Coefficient")
      }
    }
    if (measure == "YUQ") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Yule's Q", "Yule's Q")
      } else {
        lab <- ifelse(short, lab, "Transformed Yule's Q")
      }
    }
    if (measure == "YUY") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Yule's Y", "Yule's Y")
      } else {
        lab <- ifelse(short, lab, "Transformed Yule's Y")
      }
    }

    if (measure == "IRR") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Log[IRR]", "Log Incidence Rate Ratio")
      } else {
        lab <- ifelse(short, lab, "Transformed Log Incidence Rate Ratio")
        if (atransf.char == "exp" || atransf.char == "transf.exp.int")
          lab <- ifelse(short, "Rate Ratio", "Incidence Rate Ratio (log scale)")
        if (transf.char == "exp" || transf.char == "transf.exp.int")
          lab <- ifelse(short, "Rate Ratio", "Incidence Rate Ratio")
      }
    }
    if (measure == "IRD") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "IRD", "Incidence Rate Difference")
      } else {
        lab <- ifelse(short, lab, "Transformed Incidence Rate Difference")
      }
    }
    if (measure == "IRSD") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "IRSD", "Square Root Transformed Incidence Rate Difference")
      } else {
        lab <- ifelse(short, lab, "Transformed Square Root Transformed Incidence Rate Difference")
      }
    }

    if (measure == "MD") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "MD", "Mean Difference")
      } else {
        lab <- ifelse(short, lab, "Transformed Mean Difference")
      }
    }
    if (is.element(measure, c("SMD","SMDH","PBIT","OR2D","OR2DN","OR2DL","SMD1"))) {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "SMD", "Standardized Mean Difference")
      } else {
        lab <- ifelse(short, lab, "Transformed Standardized Mean Difference")
      }
    }
    if (measure == "ROM") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Log[RoM]", "Log Ratio of Means")
      } else {
        lab <- ifelse(short, lab, "Transformed Log Ratio of Means")
        if (atransf.char == "exp" || atransf.char == "transf.exp.int")
          lab <- ifelse(short, "Ratio of Means", "Ratio of Means (log scale)")
        if (transf.char == "exp" || transf.char == "transf.exp.int")
          lab <- ifelse(short, "Ratio of Means", "Ratio of Means")
      }
    }
    if (measure == "RPB") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Correlation", "Point-Biserial Correlation")
      } else {
        lab <- ifelse(short, lab, "Transformed Point-Biserial Correlation")
      }
    }
    if (measure == "CVR") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Log[CVR]", "Log Coefficient of Variation Ratio")
      } else {
        lab <- ifelse(short, lab, "Transformed Log Coefficient of Variation Ratio")
        if (atransf.char == "exp" || atransf.char == "transf.exp.int")
          lab <- ifelse(short, "CVR", "Coefficient of Variation Ratio (log scale)")
        if (transf.char == "exp" || transf.char == "transf.exp.int")
          lab <- ifelse(short, "CVR", "Coefficient of Variation Ratio")
      }
    }
    if (measure == "VR") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Log[VR]", "Log Variability Ratio")
      } else {
        lab <- ifelse(short, lab, "Transformed Log Variability Ratio")
        if (atransf.char == "exp" || atransf.char == "transf.exp.int")
          lab <- ifelse(short, "VR", "Variability Ratio (log scale)")
        if (transf.char == "exp" || transf.char == "transf.exp.int")
          lab <- ifelse(short, "VR", "Variability Ratio")
      }
    }

    if (is.element(measure, c("COR","UCOR","RTET","RBIS"))) {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Correlation", "Correlation Coefficient")
      } else {
        lab <- ifelse(short, lab, "Transformed Correlation Coefficient")
      }
    }
    if (measure == "ZCOR") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, expression('Fisher\'s ' * z[r]), "Fisher's z Transformed Correlation Coefficient")
      } else {
        lab <- ifelse(short, lab, "Transformed Fisher's z Transformed Correlation Coefficient")
        if (atransf.char == "transf.ztor" || atransf.char == "transf.ztor.int")
          lab <- ifelse(short, "Correlation", "Correlation Coefficient")
        if (transf.char == "transf.ztor" || transf.char == "transf.ztor.int")
          lab <- ifelse(short, "Correlation", "Correlation Coefficient")
      }
    }

    if (measure == "PCOR") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Correlation", "Partial Correlation Coefficient")
      } else {
        lab <- ifelse(short, lab, "Transformed Partial Correlation Coefficient")
      }
    }
    if (measure == "ZPCOR") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, expression('Fisher\'s ' * z[r]), "Fisher's z Transformed Partial Correlation Coefficient")
      } else {
        lab <- ifelse(short, lab, "Transformed Fisher's z Transformed Partial Correlation Coefficient")
        if (atransf.char == "transf.ztor" || atransf.char == "transf.ztor.int")
          lab <- ifelse(short, "Correlation", "Partial Correlation Coefficient")
        if (transf.char == "transf.ztor" || transf.char == "transf.ztor.int")
          lab <- ifelse(short, "Correlation", "Partial Correlation Coefficient")
      }
    }
    if (measure == "SPCOR") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Correlation", "Semi-Partial Correlation Coefficient")
      } else {
        lab <- ifelse(short, lab, "Transformed Semi-Partial Correlation Coefficient")
      }
    }

    if (measure == "PR") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Proportion", "Proportion")
      } else {
        lab <- ifelse(short, lab, "Transformed Proportion")
      }
    }
    if (measure == "PLN") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Log[Pr]", "Log Proportion")
      } else {
        lab <- ifelse(short, lab, "Transformed Log Proportion")
        if (atransf.char == "exp" || atransf.char == "transf.exp.int")
          lab <- ifelse(short, "Proportion", "Proportion (log scale)")
        if (transf.char == "exp" || transf.char == "transf.exp.int")
          lab <- ifelse(short, "Proportion", "Proportion")
      }
    }
    if (measure == "PLO") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Log[Odds]", "Log Odds")
      } else {
        lab <- ifelse(short, lab, "Transformed Log Odds")
        if (atransf.char == "transf.ilogit" || atransf.char == "transf.ilogit.int" || atransf.char == "plogis")
          lab <- ifelse(short, "Proportion", "Proportion (logit scale)")
        if (transf.char == "transf.ilogit" || transf.char == "transf.ilogit.int" || transf.char == "plogis")
          lab <- ifelse(short, "Proportion", "Proportion")
        if (atransf.char == "exp" || atransf.char == "transf.exp.int")
          lab <- ifelse(short, "Odds", "Odds (log scale)")
        if (transf.char == "exp" || transf.char == "transf.exp.int")
          lab <- ifelse(short, "Odds", "Odds")
      }
    }
    if (measure == "PAS") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, expression(arcsin(sqrt(p))), "Arcsine Transformed Proportion")
      } else {
        lab <- ifelse(short, lab, "Transformed Arcsine Transformed Proportion")
        if (atransf.char == "transf.iarcsin" || atransf.char == "transf.iarcsin.int")
          lab <- ifelse(short, "Proportion", "Proportion (arcsine scale)")
        if (transf.char == "transf.iarcsin" || transf.char == "transf.iarcsin.int")
          lab <- ifelse(short, "Proportion", "Proportion")
      }
    }
    if (measure == "PFT") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "PFT", "Double Arcsine Transformed Proportion")
      } else {
        lab <- ifelse(short, lab, "Transformed Double Arcsine Transformed Proportion")
        if (atransf.char == "transf.ipft.hm")
          lab <- ifelse(short, "Proportion", "Proportion")
        if (transf.char == "transf.ipft.hm")
          lab <- ifelse(short, "Proportion", "Proportion")
      }
    }

    if (measure == "IR") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Rate", "Incidence Rate")
      } else {
        lab <- ifelse(short, lab, "Transformed Incidence Rate")
      }
    }
    if (measure == "IRLN") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Log[IR]", "Log Incidence Rate")
      } else {
        lab <- ifelse(short, lab, "Transformed Log Incidence Rate")
        if (atransf.char == "exp" || atransf.char == "transf.exp.int")
          lab <- ifelse(short, "Rate", "Incidence Rate (log scale)")
        if (transf.char == "exp" || transf.char == "transf.exp.int")
          lab <- ifelse(short, "Rate", "Incidence Rate")
      }
    }
    if (measure == "IRS") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Sqrt[IR]", "Square Root Transformed Incidence Rate")
      } else {
        lab <- ifelse(short, lab, "Transformed Square Root Transformed Incidence Rate")
        if (atransf.char == "transf.isqrt" || atransf.char == "transf.isqrt.int")
          lab <- ifelse(short, "Rate", "Incidence Rate (square root scale)")
        if (transf.char == "transf.isqrt" || transf.char == "transf.isqrt.int")
          lab <- ifelse(short, "Rate", "Incidence Rate")
      }
    }
    if (measure == "IRFT") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "IRFT", "Freeman-Tukey Transformed Incidence Rate")
      } else {
        lab <- ifelse(short, lab, "Transformed Freeman-Tukey Transformed Incidence Rate")
      }
    }

    if (measure == "MN") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Mean", "Mean")
      } else {
        lab <- ifelse(short, lab, "Transformed Mean")
      }
    }
    if (measure == "MNLN") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Log[Mean]", "Log Mean")
      } else {
        lab <- ifelse(short, lab, "Transformed Log Mean")
        if (atransf.char == "exp" || atransf.char == "transf.exp.int")
          lab <- ifelse(short, "Mean", "Mean (log scale)")
        if (transf.char == "exp" || transf.char == "transf.exp.int")
          lab <- ifelse(short, "Mean", "Mean")
      }
    }
    if (measure == "CVLN") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Log[CV]", "Log Coefficient of Variation")
      } else {
        lab <- ifelse(short, lab, "Transformed Log Coefficient of Variation")
        if (atransf.char == "exp" || atransf.char == "transf.exp.int")
          lab <- ifelse(short, "CV", "Coefficient of Variation (log scale)")
        if (transf.char == "exp" || transf.char == "transf.exp.int")
          lab <- ifelse(short, "CV", "Coefficient of Variation")
      }
    }
    if (measure == "SDLN") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Log[SD]", "Log Standard Deviation")
      } else {
        lab <- ifelse(short, lab, "Transformed Log Standard Deviation")
        if (atransf.char == "exp" || atransf.char == "transf.exp.int")
          lab <- ifelse(short, "SD", "Standard Deviation (log scale)")
        if (transf.char == "exp" || transf.char == "transf.exp.int")
          lab <- ifelse(short, "SD", "Standard Deviation")
      }
    }

    if (measure == "MC") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Mean Change", "Mean Change")
      } else {
        lab <- ifelse(short, lab, "Transformed Mean Change")
      }
    }
    if (is.element(measure, c("SMCC","SMCR","SMCRH"))) {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "SMC", "Standardized Mean Change")
      } else {
        lab <- ifelse(short, lab, "Transformed Standardized Mean Change")
      }
    }
    if (measure == "ROMC") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Log[RoM]", "Log Ratio of Means")
      } else {
        lab <- ifelse(short, lab, "Transformed Log Ratio of Means")
        if (atransf.char == "exp" || atransf.char == "transf.exp.int")
          lab <- ifelse(short, "Ratio of Means", "Ratio of Means (log scale)")
        if (transf.char == "exp" || transf.char == "transf.exp.int")
          lab <- ifelse(short, "Ratio of Means", "Ratio of Means")
      }
    }
    if (measure == "CVRC") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Log[CVR]", "Log Coefficient of Variation Ratio")
      } else {
        lab <- ifelse(short, lab, "Transformed Log Coefficient of Variation Ratio")
        if (atransf.char == "exp" || atransf.char == "transf.exp.int")
          lab <- ifelse(short, "CVR", "Coefficient of Variation Ratio (log scale)")
        if (transf.char == "exp" || transf.char == "transf.exp.int")
          lab <- ifelse(short, "CVR", "Coefficient of Variation Ratio")
      }
    }
    if (measure == "VRC") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Log[VR]", "Log Variability Ratio")
      } else {
        lab <- ifelse(short, lab, "Transformed Log Variability Ratio")
        if (atransf.char == "exp" || atransf.char == "transf.exp.int")
          lab <- ifelse(short, "VR", "Variability Ratio (log scale)")
        if (transf.char == "exp" || transf.char == "transf.exp.int")
          lab <- ifelse(short, "VR", "Variability Ratio")
      }
    }

    if (measure == "ARAW") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, "Alpha", "Cronbach's alpha")
      } else {
        lab <- ifelse(short, lab, "Transformed Cronbach's alpha")
      }
    }
    if (measure == "AHW") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, expression('Alpha'[HW]), "Transformed Cronbach's alpha")
      } else {
        lab <- ifelse(short, lab, "Transformed Cronbach's alpha")
        if (atransf.char == "transf.iahw")
          lab <- ifelse(short, "Alpha", "Cronbach's alpha")
        if (transf.char == "transf.iahw")
          lab <- ifelse(short, "Alpha", "Cronbach's alpha")
      }
    }
    if (measure == "ABT") {
      if (transf.char == "FALSE" && atransf.char == "FALSE") {
        lab <- ifelse(short, expression('Alpha'[B]), "Transformed Cronbach's alpha")
      } else {
        lab <- ifelse(short, lab, "Transformed Cronbach's alpha")
        if (atransf.char == "transf.iabt")
          lab <- ifelse(short, "Alpha", "Cronbach's alpha")
        if (transf.char == "transf.iabt")
          lab <- ifelse(short, "Alpha", "Cronbach's alpha")
      }
    }


  }
  return(lab)
}
