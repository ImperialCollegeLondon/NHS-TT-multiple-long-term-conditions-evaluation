#' Run Adjusted Poisson Model for LTC Threshold (5-Year Outcome)
#'
#' @description
#' Fits a multivariable Poisson regression model with robust standard errors
#' to estimate Incidence Rate Ratios (IRRs) for having at least a specified
#' number of long-term conditions (LTCs) at 5-year follow-up.
#'
#' A binary outcome variable is dynamically created based on the specified
#' LTC threshold (e.g., ≥1, ≥2, ≥3 conditions).
#'
#' @param df A data.frame containing baseline covariates and LTC counts.
#' @param ltc_threshold Integer threshold for defining the binary outcome.
#'   Default is 1 (≥1 LTC).
#' @param ltc_variable Name of the numeric column representing total LTC
#'   count at 5-year follow-up. Default is `"ltc_baseline_5_year"`.
#'
#' @return
#' A data.frame containing:
#' \itemize{
#'   \item Variable names
#'   \item Incidence Rate Ratios (IRR)
#'   \item 95% confidence intervals
#'   \item 99% confidence intervals
#' }
#'
#' @details
#' Uses robust (sandwich) standard errors (HC0). Designed for adjusted
#' LTC progression analyses in matched cohorts.
#'
#' @export


run_poisson_model_for_ltc_threshold <- function(df, ltc_threshold = 1, ltc_variable = "ltc_baseline_5_year") {
  # Construct the new binary outcome variable name
  outcome_var <- paste0("atleast_", ltc_threshold, "LTC_LD_5year")
  
  # Create the binary variable
  df[[outcome_var]] <- ifelse(df[[ltc_variable]] >= ltc_threshold, 1, 0)
  df[[outcome_var]] <- as.numeric(df[[outcome_var]])
  
  # Define the formula for the Poisson model
  formula_str <- paste0(
    outcome_var, " ~ type + PHQ9_FirstScore + GAD_FirstScore + Gender + ",
    "ExBAFIndicator + TherapyType_Level + EmployStatus + ",
    "AE_Attendance_Before_Baseline + APC_Attendance_Before_Baseline + ",
    "OP_Attendance_Before_Baseline + GP_Rural_Urban_Classification + ",
    "GP_QOF_Percentage_Achieved + GP_Total_Size + GP_FTE_Ratio + ",
    "Ethnic_Group + IMD_Quintile + Age_Group + Baseline_LTC_Category + ",
    "Waiting_under_6weeks + Dosage + GP_IMD_Quintile + EmpSupport + ",
    "ConsMechanism_Calc + SourceOfReferralIAPT_Category + GP_Region_Code + DNA_Rate +",
    "IAPTLTCServiceInd "
    
    
  )
  
  formula_obj <- as.formula(formula_str)
  
  # Fit the Poisson model
  model <- glm(
    formula = formula_obj,
    family = poisson(link = "log"),
    data = df
  )
  
  # Robust standard errors
  robust_vcov <- sandwich::vcovHC(model, type = "HC0")
  robust_se_all <- sqrt(diag(robust_vcov))
  
  # Handle potential NA standard errors
  est <- coef(model)
  robust_se <- robust_se_all[names(est)]
  
  # Compute IRRs and 95% CIs
  irr <- exp(est)
  # 95% CI
  ci95_low <- exp(est - 1.96 * robust_se)
  ci95_high <- exp(est + 1.96 * robust_se)
  
  # 99% CI
  ci99_low <- exp(est - 2.576 * robust_se)
  ci99_high <- exp(est + 2.576 * robust_se)
  # Output table
  results <- data.frame(
    Variable = names(irr),
    IRR = irr,
    CI95_Lower = ci95_low,
    CI95_Upper = ci95_high,
    CI99_Lower = ci99_low,
    CI99_Upper = ci99_high,
    row.names = NULL
  )
  
  #print(outcome_var , "done!")
  # Optionally print problematic variables (NA SEs)
  problem_vars <- names(robust_se_all)[is.na(robust_se_all)]
  if (length(problem_vars) > 0) {
    message("Variables with NA standard errors: ", paste(problem_vars, collapse = ", "))
  }
  
  message("Threshold ", ltc_threshold, " done")
  
  return(results)
}
