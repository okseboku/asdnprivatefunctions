#' Evaluate all components of a PLS model
#'
#' This function extracts validation metrics from all components of a fitted PLS model,
#' and flags each component as 'Good', 'Acceptable', 'Overfit', or 'Weak'.
#'
#' @param pls_model A fitted PLS model object from mdatools
#' @return A data.frame with metrics and performance flags
#' @export
ASDN_evaluate_all_components <- function(pls_model) {
  cal <- pls_model$res$cal
  cv  <- pls_model$res$cv
  max_comps <- pls_model$ncomp

  eval_table <- data.frame(
    ncomp = 1:max_comps,
    R2_Cal = cal$r2,
    R2_CV = cv$r2,
    RMSE_Cal = cal$rmse,
    RMSE_CV = cv$rmse,
    Slope_Cal = cal$slope,
    Slope_CV = cv$slope,
    Bias_Cal = cal$bias,
    Bias_CV = cv$bias,
    RPD_Cal = cal$rpd,
    RPD_CV = cv$rpd,
    Flag = rep(NA, max_comps)
  )

  for (i in 1:max_comps) {
    r2c <- cal$r2[i]
    r2v <- cv$r2[i]
    rpdc <- cal$rpd[i]
    rpdv <- cv$rpd[i]
    slopev <- cv$slope[i]

    if (any(sapply(list(r2c, r2v, rpdc, rpdv, slopev), function(x) is.null(x) || is.na(x)))) {
      eval_table$Flag[i] <- "Missing"
      next
    }

    if (r2c > 0.9 && r2v < 0.75) {
      eval_table$Flag[i] <- "Overfit"
    } else if (r2v >= 0.85 && rpdv >= 2.5) {
      eval_table$Flag[i] <- "Good"
    } else if (r2v < 0.6 || rpdv < 1.8) {
      eval_table$Flag[i] <- "Weak"
    } else {
      eval_table$Flag[i] <- "Acceptable"
    }
  }

  nsel <- pls_model$ncomp.selected
  if (!is.null(nsel)) {
    eval_table$Flag[nsel] <- paste(eval_table$Flag[nsel], "(selected)")
  }

  return(eval_table)
}

#' Print a summary assessment for the selected PLS model component
#'
#' This function prints a qualitative summary of calibration, prediction, and generalization quality
#' for the selected number of components in a PLS model.
#'
#' @param pls_model A fitted PLS model object from mdatools
#' @return Console output with model assessment
#' @export
ASDN_final_model_assessment <- function(pls_model) {
  cal <- pls_model$res$cal
  cv  <- pls_model$res$cv
  nsel <- pls_model$ncomp.selected

  if (is.null(nsel)) {
    cat("❌ No selected component found in model.\n")
    return()
  }

  r2_cal   <- cal$r2[nsel]
  r2_cv    <- cv$r2[nsel]
  rmse_cal <- cal$rmse[nsel]
  rmse_cv  <- cv$rmse[nsel]
  slope_cal <- cal$slope[nsel]
  slope_cv  <- cv$slope[nsel]
  rpd_cal <- cal$rpd[nsel]
  rpd_cv  <- cv$rpd[nsel]

  cat("\n--- Final Model Assessment (Component:", nsel, ") ---\n")

  if (!is.null(r2_cal) && r2_cal >= 0.9 && rpd_cal >= 3) {
    cat("✅ Calibration fit is GOOD.\n")
  } else if (!is.null(r2_cal) && r2_cal >= 0.75) {
    cat("➖ Calibration fit is MODERATE.\n")
  } else {
    cat("⚠️ Calibration fit is WEAK.\n")
  }

  if (!is.null(r2_cv) && r2_cv >= 0.85 && rpd_cv >= 2.5) {
    cat("✅ Prediction (CV) is GOOD. Model generalizes well.\n")
  } else if (!is.null(r2_cv) && r2_cv >= 0.7 && rpd_cv >= 2.0) {
    cat("➖ Prediction (CV) is MODERATE. Some generalization.\n")
  } else {
    cat("⚠️ Prediction (CV) is POOR. Model may not generalize well.\n")
  }

  if (!is.null(r2_cal) && !is.null(r2_cv)) {
    if (r2_cal > 0.9 && r2_cv < 0.75) {
      cat("❌ Likely OVERFITTED: performs well on training but poorly on validation.\n")
    } else if (r2_cal < 0.7 && r2_cv < 0.7) {
      cat("❌ Likely UNDERFITTED: performs poorly on both.\n")
    } else if (abs(r2_cal - r2_cv) < 0.1 && r2_cv >= 0.85) {
      cat("✅ Model fits and generalizes WELL.\n")
    } else {
      cat("ℹ️ Fit and generalization are acceptable but not ideal.\n")
    }
  }
}
