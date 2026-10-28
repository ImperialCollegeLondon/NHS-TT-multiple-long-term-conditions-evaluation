#' Run Multiple Adjusted Poisson Models Using Reliable Improvement
#'
#' @description
#' Fits adjusted Poisson regression models for multiple LTC outcomes,
#' using Reliable Improvement as the primary predictor across models.
#'
#' @param ltc_cols Character vector of outcome variable names.
#' @param data Data.frame containing all modelling variables.
#' @param robust_type Type of robust variance estimator. Default `"HC0"`.
#'
#' @return
#' Combined data.frame of IRRs and confidence intervals across outcomes.
#'
#' @details
#' Mirrors `get_ltc_poisson_results()` but substitutes Reliable Improvement
#' as the key exposure.
#'
#' @export

library(sandwich)
library(lmtest)
library(dplyr)

get_ltc_poisson_results_RI <- function(ltc_cols, data, robust_type = "HC0") {
  all_results <- list()
  
  # Predictor variables (all the same for each model)
  predictors <- c(
    "Reliable_Improvement", "PHQ9_FirstScore", "GAD_FirstScore", "Gender", 
    "ExBAFIndicator", "TherapyType_Level", "EmployStatus", 
    "AE_Attendance_Before_Baseline", "APC_Attendance_Before_Baseline", 
    "OP_Attendance_Before_Baseline", "GP_Rural_Urban_Classification", 
    "GP_QOF_Percentage_Achieved", "GP_Total_Size", "GP_FTE_Ratio", 
    "Ethnic_Group", "Age_Group" ,"Baseline_LTC_Category","IMD_Quintile", "Waiting_under_6weeks", "Dosage", 
    "GP_IMD_Quintile", "EmpSupport", "ConsMechanism_Calc", 
    "SourceOfReferralIAPT_Category", "GP_Region_Code", "DNA_Rate" , "IAPTLTCServiceInd"
  )
  
  for (outcome in ltc_cols) {
    formula_text <- paste(outcome, "~", paste(predictors, collapse = " + "))
    formula <- as.formula(formula_text)
    
    model <- glm(formula, family = poisson(link = "log"), data = data)
    
    # Robust SEs
    vcov_robust <- vcovHC(model, type = robust_type)
    robust_se_all <- sqrt(diag(vcov_robust))
    
    # Check for NA SEs
    problem_vars <- names(robust_se_all)[is.na(robust_se_all)]
    if (length(problem_vars) > 0) {
      message("Model for ", outcome, " has NA SEs for: ", paste(problem_vars, collapse = ", "))
    }
    
    est <- coef(model)
    robust_se <- robust_se_all[names(est)]
    
    # IRRs and CIs
    irr <- exp(est)
    # 95% CI
    ci95_low <- exp(est - 1.96 * robust_se)
    ci95_high <- exp(est + 1.96 * robust_se)
    
    # 99% CI
    ci99_low <- exp(est - 2.576 * robust_se)
    ci99_high <- exp(est + 2.576 * robust_se)
    
    # Build tidy table (excluding intercept)
    result_df <- data.frame(
      Outcome = outcome,
      Variable = names(irr),
      IRR = irr,
      CI95_Lower = ci95_low,
      CI95_Upper = ci95_high,
      CI99_Lower = ci99_low,
      CI99_Upper = ci99_high,
      row.names = NULL
    ) #%>%
    #filter(Variable != "(Intercept)")
    #filter(Variable == "(Reliable_Recovery1)")
    
    # Add count and proportion of events
    # Add outcome name and outcome summary

    
    
    
    all_results[[outcome]] <- result_df
  }
  
  final_df <- bind_rows(all_results)
  return(final_df)
}
