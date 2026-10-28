#' Primary Analysis: Physical Long-Term Conditions (PLD)
#'
#' @description
#' Conducts the primary analysis of physical long-term conditions (PLD)
#' at baseline, 1-year, and 5-year follow-up within the matched cohort.
#'
#' The function:
#' \itemize{
#'   \item Cleans and filters condition indicators
#'   \item Constructs LTC count variables
#'   \item Generates ≥N LTC binary outcomes (1–24)
#'   \item Runs adjusted and unadjusted Poisson models
#'   \item Computes count and percentage summaries
#' }
#'
#' @param matched_data Matched cohort dataset containing LTC indicators
#'   and covariates.
#'
#' @return
#' A named list containing:
#' \itemize{
#'   \item Adjusted IRRs for ≥N LTCs
#'   \item Unadjusted IRRs
#'   \item Count and percentage summaries
#'   \item Individual LTC model results
#' }
#'
#' @details
#' Robust (sandwich) standard errors are used for all Poisson models.
#' Designed for secondary PLD outcome analysis.
#'
#' @export



get_primary_analysis_PLD <- function(matched_data) {

df_2 <- matched_data %>%
  dplyr::select("pseudo_nhs_number_ncdr"	,
                "Gender"	,
                "ExBAFIndicator"	,
                "PHQ9_FirstScore"	,
                "GAD_FirstScore"	,
                "EmployStatus"	,
                "ConsMechanism_Calc"	,
                "TherapyType_Level"	,
                "DNA_Rate"	,
                "EmpSupport"	,
                "GP_Rural_Urban_Classification"	,
                "GP_Total_Size"	,
                "GP_FTE_Ratio"	,
                "GP_QOF_Percentage_Achieved"	,
                "AE_Attendance_Before_Baseline"	,
                "APC_Attendance_Before_Baseline"	,
                "OP_Attendance_Before_Baseline"	,
                "Ethnic_Group"	,
                "IMD_Quintile"	,
                "Age_Group"	,
                "Baseline_LTC_Category"	,
                "Waiting_under_6weeks"	,
                "SourceOfReferralIAPT_Category"	,
                "Dosage"	,
                "GP_IMD_Quintile", 
                "GP_Region_Code",
                "IAPTLTCServiceInd",
                #"entry_quarter",
                #"entry_year",
                "type",
                "alcohol_dependence_Baseline",
                "Asthma_Baseline",
                "atrial_fibrillation_Baseline",
                "bronchiectasis_Baseline",
                "cancer_Baseline",
                "chronic_kidney_disease_Baseline",
                "chronic_liver_disease_Baseline",
                "chronic_pain_Baseline",
                "copd_Baseline",
                "epilepsy_Baseline",
                "frailty_risk_Baseline",
                "hypertension_Baseline",
                "inflammatory_bowel_disease_Baseline",
                "multiple_sclerosis_Baseline",
                "neurological_organ_failure_Baseline",
                "osteoarthritis_Baseline",
                "osteoporosis_Baseline",
                "parkinsons_disease_Baseline",
                "peripheral_vascular_disease_Baseline",
                "Physical_Disability_Baseline",
                "pulmonary_heart_disease_Baseline",
                "rheumatoid_arthritis_Baseline",
                "sarcoidosis_Baseline",
                "severe_interstitial_lung_disease_Baseline",
                "alcohol_dependence_Baseline_1_year",
                "Asthma_Baseline_1_year",
                "atrial_fibrillation_Baseline_1_year",
                "bronchiectasis_Baseline_1_year",
                "cancer_Baseline_1_year",
                "chronic_kidney_disease_Baseline_1_year",
                "chronic_liver_disease_Baseline_1_year",
                "chronic_pain_Baseline_1_year",
                "copd_Baseline_1_year",
                "epilepsy_Baseline_1_year",
                "frailty_risk_Baseline_1_year",
                "hypertension_Baseline_1_year",
                "inflammatory_bowel_disease_Baseline_1_year",
                "multiple_sclerosis_Baseline_1_year",
                "neurological_organ_failure_Baseline_1_year",
                "osteoarthritis_Baseline_1_year",
                "osteoporosis_Baseline_1_year",
                "parkinsons_disease_Baseline_1_year",
                "peripheral_vascular_disease_Baseline_1_year",
                "Physical_Disability_Baseline_1_year",
                "pulmonary_heart_disease_Baseline_1_year",
                "rheumatoid_arthritis_Baseline_1_year",
                "sarcoidosis_Baseline_1_year",
                "severe_interstitial_lung_disease_Baseline_1_year",
                "alcohol_dependence_Baseline_5_year",
                "Asthma_Baseline_5_year",
                "atrial_fibrillation_Baseline_5_year",
                "bronchiectasis_Baseline_5_year",
                "cancer_Baseline_5_year",
                "chronic_kidney_disease_Baseline_5_year",
                "chronic_liver_disease_Baseline_5_year",
                "chronic_pain_Baseline_5_year",
                "copd_Baseline_5_year",
                "epilepsy_Baseline_5_year",
                "frailty_risk_Baseline_5_year",
                "hypertension_Baseline_5_year",
                "inflammatory_bowel_disease_Baseline_5_year",
                "multiple_sclerosis_Baseline_5_year",
                "neurological_organ_failure_Baseline_5_year",
                "osteoarthritis_Baseline_5_year",
                "osteoporosis_Baseline_5_year",
                "parkinsons_disease_Baseline_5_year",
                "peripheral_vascular_disease_Baseline_5_year",
                "Physical_Disability_Baseline_5_year",
                "pulmonary_heart_disease_Baseline_5_year",
                "rheumatoid_arthritis_Baseline_5_year",
                "sarcoidosis_Baseline_5_year",
                "severe_interstitial_lung_disease_Baseline_5_year",
                "alcohol_dependence_first_occurance_date",
                "Asthma_first_occurance_date",
                "atrial_fibrillation_first_occurance_date",
                "bronchiectasis_first_occurance_date",
                "cancer_first_occurance_date",
                "chronic_kidney_disease_first_occurance_date",
                "chronic_liver_disease_first_occurance_date",
                "chronic_pain_first_occurance_date",
                "copd_first_occurance_date",
                "epilepsy_first_occurance_date",
                "frailty_risk_first_occurance_date",
                "hypertension_first_occurance_date",
                "inflammatory_bowel_disease_first_occurance_date",
                "multiple_sclerosis_first_occurance_date",
                "neurological_organ_failure_first_occurance_date",
                "osteoarthritis_first_occurance_date",
                "osteoporosis_first_occurance_date",
                "parkinsons_disease_first_occurance_date",
                "peripheral_vascular_disease_first_occurance_date",
                "Physical_Disability_first_occurance_date",
                "pulmonary_heart_disease_first_occurance_date",
                "rheumatoid_arthritis_first_occurance_date",
                "sarcoidosis_first_occurance_date",
                "severe_interstitial_lung_disease_first_occurance_date"
                
  ) %>%
  as.data.frame()


condition_columns <- c(      "alcohol_dependence_Baseline",
                             "Asthma_Baseline",
                             "atrial_fibrillation_Baseline",
                             "bronchiectasis_Baseline",
                             "cancer_Baseline",
                             "chronic_kidney_disease_Baseline",
                             "chronic_liver_disease_Baseline",
                             "chronic_pain_Baseline",
                             "copd_Baseline",
                             "epilepsy_Baseline",
                             "frailty_risk_Baseline",
                             "hypertension_Baseline",
                             "inflammatory_bowel_disease_Baseline",
                             "multiple_sclerosis_Baseline",
                             "neurological_organ_failure_Baseline",
                             "osteoarthritis_Baseline",
                             "osteoporosis_Baseline",
                             "parkinsons_disease_Baseline",
                             "peripheral_vascular_disease_Baseline",
                             "Physical_Disability_Baseline",
                             "pulmonary_heart_disease_Baseline",
                             "rheumatoid_arthritis_Baseline",
                             "sarcoidosis_Baseline",
                             "severe_interstitial_lung_disease_Baseline",
                             "alcohol_dependence_Baseline_1_year",
                             "Asthma_Baseline_1_year",
                             "atrial_fibrillation_Baseline_1_year",
                             "bronchiectasis_Baseline_1_year",
                             "cancer_Baseline_1_year",
                             "chronic_kidney_disease_Baseline_1_year",
                             "chronic_liver_disease_Baseline_1_year",
                             "chronic_pain_Baseline_1_year",
                             "copd_Baseline_1_year",
                             "epilepsy_Baseline_1_year",
                             "frailty_risk_Baseline_1_year",
                             "hypertension_Baseline_1_year",
                             "inflammatory_bowel_disease_Baseline_1_year",
                             "multiple_sclerosis_Baseline_1_year",
                             "neurological_organ_failure_Baseline_1_year",
                             "osteoarthritis_Baseline_1_year",
                             "osteoporosis_Baseline_1_year",
                             "parkinsons_disease_Baseline_1_year",
                             "peripheral_vascular_disease_Baseline_1_year",
                             "Physical_Disability_Baseline_1_year",
                             "pulmonary_heart_disease_Baseline_1_year",
                             "rheumatoid_arthritis_Baseline_1_year",
                             "sarcoidosis_Baseline_1_year",
                             "severe_interstitial_lung_disease_Baseline_1_year",
                             "alcohol_dependence_Baseline_5_year",
                             "Asthma_Baseline_5_year",
                             "atrial_fibrillation_Baseline_5_year",
                             "bronchiectasis_Baseline_5_year",
                             "cancer_Baseline_5_year",
                             "chronic_kidney_disease_Baseline_5_year",
                             "chronic_liver_disease_Baseline_5_year",
                             "chronic_pain_Baseline_5_year",
                             "copd_Baseline_5_year",
                             "epilepsy_Baseline_5_year",
                             "frailty_risk_Baseline_5_year",
                             "hypertension_Baseline_5_year",
                             "inflammatory_bowel_disease_Baseline_5_year",
                             "multiple_sclerosis_Baseline_5_year",
                             "neurological_organ_failure_Baseline_5_year",
                             "osteoarthritis_Baseline_5_year",
                             "osteoporosis_Baseline_5_year",
                             "parkinsons_disease_Baseline_5_year",
                             "peripheral_vascular_disease_Baseline_5_year",
                             "Physical_Disability_Baseline_5_year",
                             "pulmonary_heart_disease_Baseline_5_year",
                             "rheumatoid_arthritis_Baseline_5_year",
                             "sarcoidosis_Baseline_5_year",
                             "severe_interstitial_lung_disease_Baseline_5_year"
)


df_2 <- df_2[!apply(df_2[ , condition_columns] == -1, 1, any), ]




condition_names = c("Asthma",
                    "atrial_fibrillation",
                    "bronchiectasis",
                    "cancer",
                    "chronic_kidney_disease",
                    "chronic_liver_disease",
                    "chronic_pain",
                    "copd",
                    "epilepsy",
                    "frailty_risk",
                    "hypertension",
                    "inflammatory_bowel_disease",
                    "multiple_sclerosis",
                    "neurological_organ_failure",
                    "osteoarthritis",
                    "osteoporosis",
                    "parkinsons_disease",
                    "peripheral_vascular_disease",
                    "Physical_Disability",
                    "pulmonary_heart_disease",
                    "rheumatoid_arthritis",
                    "sarcoidosis",
                    "severe_interstitial_lung_disease"
)




df_2$ltc_baseline <- Matrix::rowSums(df_2[ ,paste0(condition_names, "_Baseline") ])
df_2$ltc_baseline_1_year <- rowSums(df_2[ ,paste0(condition_names, "_Baseline_1_year")])
df_2$ltc_baseline_5_year <- rowSums(df_2[ ,paste0(condition_names, "_Baseline_5_year")])


df_2$type <- relevel(factor(df_2$type) , ref = "0")

df_2$Gender <- relevel(factor(df_2$Gender) , ref = "2")
df_2$ExBAFIndicator <- relevel(factor(df_2$ExBAFIndicator) , ref = "0")
df_2$TherapyType_Level <- relevel(factor(df_2$TherapyType_Level) , ref = "Mixed")
df_2$EmployStatus <- relevel(factor(df_2$EmployStatus) , ref = "1")
df_2$GP_Rural_Urban_Classification <- relevel(factor(df_2$GP_Rural_Urban_Classification) , ref = "Urban")
df_2$Ethnic_Group <- relevel(factor(df_2$Ethnic_Group) , ref = "White")
df_2$IMD_Quintile <- relevel(factor(df_2$IMD_Quintile) , ref = "5")
df_2$Age_Group <- relevel(factor(df_2$Age_Group) , ref = "18-39")
df_2$Baseline_LTC_Category <- relevel(factor(df_2$Baseline_LTC_Category) , ref = "No LTC")
df_2$Waiting_under_6weeks <- relevel(factor(df_2$Waiting_under_6weeks) , ref = "1")
df_2$Dosage <- relevel(factor(df_2$Dosage) , ref = "1")
df_2$ConsMechanism_Calc <- relevel(factor(df_2$ConsMechanism_Calc) , ref = "1")
df_2$EmpSupport <- relevel(factor(df_2$EmpSupport) , ref = "0")
df_2$SourceOfReferralIAPT_Category <- relevel(factor(df_2$SourceOfReferralIAPT_Category) , ref = "1")
df_2$IAPTLTCServiceInd <- relevel(factor(df_2$IAPTLTCServiceInd) , ref = "0")


#############################################################

#Atleast one ltc at followup 5 year

########################################################

#df_2$number_ltc_5_year <- df_2$ltc_baseline_5_year - df_2$ltc_baseline


df_2$atleast_oneLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 0, 1, 0)
df_2$atleast_twoLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 1, 1, 0)
df_2$atleast_threeLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 2, 1, 0)
df_2$atleast_fourLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 3, 1, 0)
df_2$atleast_fiveLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 4, 1, 0)
df_2$atleast_sixLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 5, 1, 0)
df_2$atleast_sevenLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 6, 1, 0)
df_2$atleast_eightLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 7, 1, 0)
df_2$atleast_nineLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 8, 1, 0)
df_2$atleast_tenLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 9, 1, 0)
df_2$atleast_elevenLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 10, 1, 0)
df_2$atleast_twelveLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 11, 1, 0)
df_2$atleast_thirteenLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 12, 1, 0)
df_2$atleast_fourteenLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 13, 1, 0)
df_2$atleast_fifteenLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 14, 1, 0)
df_2$atleast_sixteenLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 15, 1, 0)
df_2$atleast_seventeenLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 16, 1, 0)
df_2$atleast_eighteenLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 17, 1, 0)
df_2$atleast_ninteenLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 18, 1, 0)
df_2$atleast_twentyLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 19, 1, 0)
df_2$atleast_twentyoneLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 20, 1, 0)
df_2$atleast_twentytwoLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 21, 1, 0)
df_2$atleast_twentythreeLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 22, 1, 0)
df_2$atleast_twentyfourLTC_PLD_5year <- ifelse(df_2$ltc_baseline_5_year > 23, 1, 0)

########Unadjusted IRR#########



outcome_vars <- paste0("atleast_", c("one",
                                     "two",
                                     "three",
                                     "four",
                                     "five",
                                     "six",
                                     "seven",
                                     "eight",
                                     "nine",
                                     "ten",
                                     "eleven",
                                     "twelve",
                                     "thirteen",
                                     "fourteen",
                                     "fifteen",
                                     "sixteen",
                                     "seventeen",
                                     "eighteen",
                                     "ninteen",
                                     "twenty",
                                     "twentyone",
                                     "twentytwo",
                                     "twentythree",
                                     "twentyfour"
), "LTC_PLD_5year")


####  Unadjusted
atleast_n_IRR_Unadjusted_5year <- get_multiple_poisson_irr(
  outcome_vars = outcome_vars,
  # predictor = "Reliable_Recovery",
  predictor = "type",
  data = df_2
)


########Adjusted IRR#########
# Run for thresholds 1 to 5
atleast_n_IRR <- lapply(1:24, function(thresh) {
  run_poisson_model_for_ltc_threshold(df = df_2, ltc_threshold = thresh)
  
})
names(atleast_n_IRR) <- paste0("atleast_", 1:24, "_LTCs")

# check individually if there is a warning message or if algorithm doesnot fit

# Names to loop through
ltc_names <- paste0("atleast_", 1:24, "_LTCs")

# Apply filter to each element of the list
filtered_list <- map(ltc_names, function(name) {
  df <- atleast_n_IRR[[name]]
  if (!is.null(df) && "Variable" %in% names(df)) {
    df %>% filter(Variable == "type1")
  } else {
    NULL
  }
})

# Optional: name each element
names(filtered_list) <- ltc_names

# Optional: bind into one data frame
atleast_n_IRR_Adjusted_5year <- bind_rows(filtered_list, .id = "LTC_Level")


################Count percent tabl########################################

ltc_cols <- grep("atleast_.*LTC_PLD_5year", names(df_2), value = TRUE)
#ltc_cols = "atleast_oneLTC_PLD_5year"
atleast_n_count_percent_5year <- countPercent_fn(df_2, group_column = "type", ltc_cols)


##############Individual LTC

ltc_vars <- c(
  "alcohol_dependence_Baseline_5_year",
  "Asthma_Baseline_5_year",
  "atrial_fibrillation_Baseline_5_year",
  "bronchiectasis_Baseline_5_year",
  "cancer_Baseline_5_year",
  "chronic_kidney_disease_Baseline_5_year",
  "chronic_liver_disease_Baseline_5_year",
  "chronic_pain_Baseline_5_year",
  "copd_Baseline_5_year",
  "epilepsy_Baseline_5_year",
  "frailty_risk_Baseline_5_year",
  "hypertension_Baseline_5_year",
  "inflammatory_bowel_disease_Baseline_5_year",
  "multiple_sclerosis_Baseline_5_year",
  "neurological_organ_failure_Baseline_5_year",
  "osteoarthritis_Baseline_5_year",
  "osteoporosis_Baseline_5_year",
  "parkinsons_disease_Baseline_5_year",
  "peripheral_vascular_disease_Baseline_5_year",
  "Physical_Disability_Baseline_5_year",
  "pulmonary_heart_disease_Baseline_5_year",
  "rheumatoid_arthritis_Baseline_5_year",
  "sarcoidosis_Baseline_5_year",
  "severe_interstitial_lung_disease_Baseline_5_year"
  
)

# Define covariates used in all models
covariates <- paste(
  "type",
  "PHQ9_FirstScore",
  "GAD_FirstScore",
  "Gender",
  "ExBAFIndicator",
  "TherapyType_Level",
  "EmployStatus",
  "AE_Attendance_Before_Baseline",
  "APC_Attendance_Before_Baseline",
  "OP_Attendance_Before_Baseline",
  "GP_Rural_Urban_Classification",
  "GP_QOF_Percentage_Achieved",
  "GP_Total_Size",
  "GP_FTE_Ratio",
  "Ethnic_Group",
  "IMD_Quintile",
  "Age_Group",
  "Baseline_LTC_Category",
  "Waiting_under_6weeks",
  "Dosage",
  "GP_IMD_Quintile",
  "EmpSupport",
  "ConsMechanism_Calc",
  "GP_Region_Code",
  "SourceOfReferralIAPT_Category",
  "DNA_Rate",
  "IAPTLTCServiceInd",
  
  sep = " + "
)

# Initialize result holders
results_list <- list()

# Loop over each LTC variable
for (ltc in ltc_vars) {
  # Ensure numeric outcome
  df_2[[ltc]] <- as.numeric(df_2[[ltc]])
  
  # Construct model formula
  formula <- as.formula(paste(ltc, "~", covariates))
  
  # Fit Poisson model
  model <- glm(formula, family = poisson(link = "log"), data = df_2)
  
  # Robust standard errors
  vcov_robust <- vcovHC(model, type = "HC0")
  se_robust <- sqrt(diag(vcov_robust))
  
  # Ensure alignment
  est <- coef(model)
  se_robust <- se_robust[names(est)]
  
  # IRR and CI
  irr <- exp(est["type1"])
  ci95_low <- exp(est["type1"] - 1.96 * se_robust["type1"])
  ci95_high <- exp(est["type1"] + 1.96 * se_robust["type1"])
  
  
  
  # 99% CI (z = 2.576)
  ci99_low <- exp(est["type1"] - 2.576 * se_robust["type1"])
  ci99_high <- exp(est["type1"] + 2.576 * se_robust["type1"])
  
  # Store results
  results_list[[ltc]] <- data.frame(
    Condition = ltc,
    IRR = irr,
    CI95_Lower = ci95_low,
    CI95_Upper = ci95_high,
    CI99_Lower = ci99_low,
    CI99_Upper = ci99_high,
    
 
    row.names = NULL
  )
}

# Combine all into one data frame
individual_LTC_IRR_Adjusted_5year <- bind_rows(results_list)
#####################Unadjusted############################


individual_LTC_IRR_Unadjusted_5year <- get_multiple_poisson_irr(
  outcome_vars = ltc_vars,
  # predictor = "Reliable_Recovery",
  predictor = "type",
  data = df_2
)


individual_LTC_count_percent_5year <- countPercent_fn(df_2, group_column = "type", ltc_vars)


# Return as a list
return(list(
  # atleast_n_IRR_Unadjusted_5year      = atleast_n_IRR_Unadjusted_5year,
  # atleast_n_IRR_Adjusted_5year        = atleast_n_IRR_Adjusted_5year,
  # atleast_n_PLD_5year_count_percent = atleast_n_PLD_5year_count_percent,
  # individual_LTC_IRR_Adjusted_5year = individual_LTC_IRR_Adjusted_5year,
  # individual_LTC_IRR_Unadjusted_5year = individual_LTC_IRR_Unadjusted_5year,
  # individual_LTC_count_percent_5year  = individual_LTC_count_percent_5year
  # 
  Secondary_atleast_n_IRR_Unadjusted_5year      = atleast_n_IRR_Unadjusted_5year,
  Secondary_atleast_n_IRR_Adjusted_5year        = atleast_n_IRR_Adjusted_5year,
  Secondary_atleast_n_count_percent_5year = atleast_n_count_percent_5year,
  Secondary_individual_LTC_IRR_Adjusted_5year = individual_LTC_IRR_Adjusted_5year,
  Secondary_individual_LTC_IRR_Unadjusted_5year = individual_LTC_IRR_Unadjusted_5year,
  Secondary_individual_LTC_count_percent_5year  = individual_LTC_count_percent_5year
  
  
))

}
