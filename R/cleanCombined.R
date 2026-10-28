#' Final cleaning of combined intervention–control datasets
#'
#' @description
#' Applies additional cleaning steps after intervention and control datasets
#' have been combined. This includes harmonisation of factor levels, removal
#' of invalid records, and final derivation of analysis-ready variables.
#'
#' @param combined_df A data.frame containing both intervention and control
#'   records.
#'
#' @return
#' A cleaned combined dataset suitable for matching and outcome analysis.
#'
#' @export


get_cleaning_combined <- function(df) {
  
## This is to apply the steps before matching the dataset

  ## Convert type to 1 and 0 (case, Control)
  ## remove duplicates
  ## remove the dead patients
  ## remove Source of Referral - IAPT ( this is internal referral for changing 
  ##         from High intensity to Low intensity treatment and viceversa
  ## remove the records where there are no source of referrals (Incomplete records)
  ## Convert referrals to Self_Referral or not
  
 combined_df <-  df %>%
    mutate(type = ifelse(type == "case", 1, 0)) %>%
    group_by(pseudo_nhs_number_ncdr) %>%
    filter(n() == 1) %>%
    ungroup() %>%
   filter(is.na(date_of_death) | as.Date(date_of_death) > as.Date("2024-09-30"))
   filter(SourceOfReferralIAPT_Category != 'IAPT') %>%
   filter(SourceOfReferralIAPT_Category != 'NA') %>%
    mutate(SourceOfReferralIAPT_Category = ifelse(SourceOfReferralIAPT_Category == 'Self_Referral' , 1, 0))
 
 
 ##  Remove any patients that are -1 (dead) in any of the below column list . The below column should be either 0 or 1
 condition_columns <- c(         "coronary_heart_disease_Baseline"	,
                                 "coronary_heart_disease_Baseline_1_year"	,
                                 "coronary_heart_disease_Baseline_5_year"	,
                                 
                                 "heart_failure_Baseline"	,
                                 "heart_failure_Baseline_1_year"	,
                                 "heart_failure_Baseline_5_year"	,
                                 
                                 "cerebrovascular_disease_Baseline"	,
                                 "cerebrovascular_disease_Baseline_1_year"	,
                                 "cerebrovascular_disease_Baseline_5_year"	,
                                 
                                 "dementia_Baseline"	,
                                 "dementia_Baseline_1_year"	,
                                 "dementia_Baseline_5_year"	,
                                 
                                 "diabetes_Baseline"	,
                                 "diabetes_Baseline_1_year"	,
                                 "diabetes_Baseline_5_year",
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
                                 "cystic_fibrosis_Baseline",
                                 "sickle_cell_disease_Baseline",
                                 "Autism_Baseline",
                                 "learning_disability_Baseline",
                                 "cystic_fibrosis_Baseline_1_year",
                                 "sickle_cell_disease_Baseline_1_year",
                                 "Autism_Baseline_1_year",
                                 "learning_disability_Baseline_1_year",
                                 "cystic_fibrosis_Baseline_5_year",
                                 "sickle_cell_disease_Baseline_5_year",
                                 "Autism_Baseline_5_year",
                                 "learning_disability_Baseline_5_year")
 
 
 combined_df <- combined_df[!apply(combined_df[ , condition_columns] == -1, 1, any), ]
 
 
 
 
 return(combined_df)
}
