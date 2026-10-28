#' Run Multiple Unadjusted Poisson IRR Models
#'
#' @description
#' Fits separate Poisson regression models for multiple binary outcomes
#' using a single predictor variable.
#'
#' @param outcome_vars Character vector of binary outcome variables.
#' @param predictor Name of exposure variable.
#' @param data Data.frame containing modelling variables.
#' @param robust_type Type of robust variance estimator. Default `"HC0"`.
#'
#' @return
#' Data.frame of IRRs and confidence intervals for each outcome.
#'
#' @details
#' Intercept terms are excluded from the final output.
#'
#' @export


library(sandwich)
library(lmtest)
library(dplyr)

get_multiple_poisson_irr <- function(
    outcome_vars,
    predictor,
    data,
    robust_type = "HC0"
) {
  all_results <- list()
  
  for (outcome in outcome_vars) {
    formula <- as.formula(paste(outcome, "~", predictor))
    
    # Fit Poisson model
    model <- glm(formula, family = poisson(link = "log"), data = data)
    vcov_robust <- vcovHC(model, type = robust_type)
    robust_se_all <- sqrt(diag(vcov_robust))
    
    # Identify problematic SEs
    problem_vars <- names(robust_se_all)[is.na(robust_se_all)]
    if (length(problem_vars) > 0) {
      message("Model for ", outcome, " has NA SEs for: ", paste(problem_vars, collapse = ", "))
    }
    
    est <- coef(model)
    robust_se <- robust_se_all[names(est)]  # Match names
    
    irr <- exp(est)
    # 95% CI
    ci95_low <- exp(est - 1.96 * robust_se)
    ci95_high <- exp(est + 1.96 * robust_se)
    
    # 99% CI
    ci99_low <- exp(est - 2.576 * robust_se)
    ci99_high <- exp(est + 2.576 * robust_se)
    
    # Create result frame, then remove intercept row
    result_df <- data.frame(
      Outcome = outcome,
      Variable = names(irr),
      IRR = irr,
      CI95_Lower = ci95_low,
      CI95_Upper = ci95_high,
      CI99_Lower = ci99_low,
      CI99_Upper = ci99_high,
      row.names = NULL
    ) %>%
      filter(Variable != "(Intercept)")
    
    
  
    all_results[[outcome]] <- result_df
  }
  
  # Combine all into one table
  final_df <- bind_rows(all_results)
  return(final_df)
}
