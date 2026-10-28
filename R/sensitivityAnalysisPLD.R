#' Sensitivity analysis for outcome measures
#'
#' @description
#' Performs sensitivity analyses for specified outcomes using alternative
#' datasets (e.g. matched vs full cohort).
#'
#' @param df A matched or full cleaned dataset.
#'
#' @return
#' Sensitivity analysis results.
#'
#' @export


get_sensitivity_analysis_PLD <- function(matched_data){


df_2 <- matched_data %>%
  dplyr::select("pseudo_nhs_number_ncdr"	,
                "Gender"	,
                "ExBAFIndicator"	,
                "PHQ9_FirstScore"	,
                "GAD_FirstScore"	,
                "PHQ9_LastScore" ,
                "GAD_LastScore",
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
                # "entry_quarter",
                # "entry_year",
                "Reliable_Recovery",
                "IAPTLTCServiceInd",
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

condition_columns <- c(           "alcohol_dependence_Baseline",
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
                                  "severe_interstitial_lung_disease_Baseline_5_year"	)


df_2 <- df_2[!apply(df_2[ , condition_columns] == -1, 1, any), ]



condition_names = c( "alcohol_dependence",
                     "Asthma",
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

df_2$Reliable_Recovery <- ifelse(df_2$Reliable_Recovery == "Yes", 1, 0)

df_2$Reliable_Recovery <- relevel(factor(df_2$Reliable_Recovery) , ref = "0")



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



# intervention patients only

df_2_intervention <-  df_2 %>%
  filter(type == 1)


df_2_intervention$Reliable_Improvement <-  ifelse( (df_2_intervention$PHQ9_FirstScore - df_2_intervention$PHQ9_LastScore >= 6) &
                                                     (df_2_intervention$GAD_LastScore - df_2_intervention$GAD_FirstScore < 4 ), 1, 0)

##############Reliable Recovery######################################

##############Unadjusted Analysis###############################


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

Sensitivity_atleast_n_IRR_Unadjusted_5year_RR <- get_multiple_poisson_irr(
  outcome_vars = outcome_vars,
  predictor = "Reliable_Recovery",
  # predictor = "Reliable_Improvement",
  data = df_2_intervention
)




#############Adjusted Analysis####################


# Run for thresholds 1 to 5
sensitivity_atleast_n_IRR <- lapply(1:24, function(thresh) {
  sensitivity_Atleast_n_IRR_Adjusted_5year(df = df_2_intervention, ltc_threshold = thresh)
  
})
names(sensitivity_atleast_n_IRR) <- paste0("atleast_", 1:24, "_LTCs")

# check individually if there is a warning message or if algorithm doesnot fit


# Names to loop through
ltc_names <- paste0("atleast_", 1:24, "_LTCs")

# Apply filter to each element of the list
filtered_list <- map(ltc_names, function(name) {
  df <- sensitivity_atleast_n_IRR[[name]]
  if (!is.null(df) && "Variable" %in% names(df)) {
    df %>% filter(Variable == "Reliable_Recovery1")
  } else {
    NULL
  }
})

# Optional: name each element
names(filtered_list) <- ltc_names

# Optional: bind into one data frame
Sensitivity_atleast_n_IRR_Adjusted_5year_RR <- bind_rows(filtered_list, .id = "LTC_Level")


###Count Percentage


Sensitivity_atleast_n_LD_5year_count_percent_RR <- get_ReliableRecoverycountPercent_fn(df_2_intervention, outcome_vars)




#####Adjusted LTC's#########################

ltc_cols <- c(
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

poisson_ltc_results <- get_ltc_poisson_results(ltc_cols, df_2_intervention)

Sensitivity_individual_LTC_IRR_Adjusted_5year_RR <- poisson_ltc_results %>%
  filter(Variable == "Reliable_Recovery1")




######Unadjusted



Sensitivity_individual_LTC_IRR_Unadjusted_5year_RR <- get_multiple_poisson_irr(
  outcome_vars = ltc_cols,
  # predictor = "Reliable_Recovery",
  predictor = "Reliable_Recovery",
  data = df_2_intervention
)



###Count Percentage



Sensitivity_individual_LTC_5year_count_percent_RR <- get_ReliableRecoverycountPercent_fn(df_2_intervention, ltc_cols)


############################Reliable Improvement##############################################

##############Unadjusted Analysis###############################

outcome_vars <- paste0("atleast_",  c("one",
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


Sensitivity_atleast_n_IRR_Unadjusted_5year_RI <- get_multiple_poisson_irr(
  outcome_vars = outcome_vars,
  predictor = "Reliable_Improvement",
  data = df_2_intervention
)




#############Adjusted Analysis####################


# Run for thresholds 1 to 5
sensitivity_atleast_n_IRR_IR <- lapply(1:24, function(thresh) {
  sensitivity_Atleast_n_IRR_Adjusted_RI_5year(df = df_2_intervention, ltc_threshold = thresh)
  
})
names(sensitivity_atleast_n_IRR_IR) <- paste0("atleast_", 1:24, "_LTCs")

# check individually if there is a warning message or if algorithm doesnot fit



# Names to loop through
ltc_names <- paste0("atleast_", 1:24, "_LTCs")

# Apply filter to each element of the list
filtered_list <- map(ltc_names, function(name) {
  df <- sensitivity_atleast_n_IRR_IR[[name]]
  if (!is.null(df) && "Variable" %in% names(df)) {
    df %>% filter(Variable == "Reliable_Improvement")
  } else {
    NULL
  }
})

# Optional: name each element
names(filtered_list) <- ltc_names

# Optional: bind into one data frame
Sensitivity_atleast_n_IRR_Adjusted_5year_RI <- bind_rows(filtered_list, .id = "LTC_Level")


###Count Percentage




Sensitivity_atleast_n_LD_5year_count_percent_RI <- get_ReliableImprovementcountPercent_fn(df_2_intervention, outcome_vars)





#####Adjusted LTC's#########################

ltc_cols <- c("alcohol_dependence_Baseline_5_year",
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

poisson_ltc_results_RI <- get_ltc_poisson_results_RI(ltc_cols, df_2_intervention)

Sensitivity_individual_LTC_IRR_Adjusted_5year_RI <- poisson_ltc_results_RI %>%
  filter(Variable == "Reliable_Improvement")



######Unadjusted






Sensitivity_individual_LTC_IRR_Unadjusted_5year_RI <- get_multiple_poisson_irr(
  outcome_vars = ltc_cols,
  # predictor = "Reliable_Recovery",
  predictor = "Reliable_Improvement",
  data = df_2_intervention
)




###Count Percentage

Sensitivity_individual_LTC_5year_count_percent_RI <- get_ReliableImprovementcountPercent_fn(df_2_intervention, ltc_cols)


# Return as a list
return(list(
  Sensitivity_atleast_n_IRR_Unadjusted_5year_RR      = Sensitivity_atleast_n_IRR_Unadjusted_5year_RR  ,   
  Sensitivity_atleast_n_IRR_Adjusted_5year_RR        = Sensitivity_atleast_n_IRR_Adjusted_5year_RR    ,   
  Sensitivity_atleast_n_LD_5year_count_percent_RR    = Sensitivity_atleast_n_LD_5year_count_percent_RR   ,
  Sensitivity_individual_LTC_IRR_Adjusted_5year_RR   = Sensitivity_individual_LTC_IRR_Adjusted_5year_RR,  
  Sensitivity_individual_LTC_IRR_Unadjusted_5year_RR = Sensitivity_individual_LTC_IRR_Unadjusted_5year_RR,
  Sensitivity_individual_LTC_5year_count_percent_RR  = Sensitivity_individual_LTC_5year_count_percent_RR ,
  Sensitivity_atleast_n_IRR_Unadjusted_5year_RI      = Sensitivity_atleast_n_IRR_Unadjusted_5year_RI    , 
  Sensitivity_atleast_n_IRR_Adjusted_5year_RI        = Sensitivity_atleast_n_IRR_Adjusted_5year_RI    ,   
  Sensitivity_atleast_n_LD_5year_count_percent_RI    = Sensitivity_atleast_n_LD_5year_count_percent_RI,   
  Sensitivity_individual_LTC_IRR_Adjusted_5year_RI   = Sensitivity_individual_LTC_IRR_Adjusted_5year_RI , 
  Sensitivity_individual_LTC_IRR_Unadjusted_5year_RI = Sensitivity_individual_LTC_IRR_Unadjusted_5year_RI,
  Sensitivity_individual_LTC_5year_count_percent_RI  = Sensitivity_individual_LTC_5year_count_percent_RI 
))

}


