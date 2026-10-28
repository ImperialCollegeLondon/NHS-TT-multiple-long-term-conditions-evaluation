#' Primary analysis: Directly Linked Conditions
#'
#' @description
#' Executes the primary statistical analysis for the Directly Linked Conditions(LD)
#' outcome using matched or full datasets.
#'
#' @param df A matched or cleaned dataset containing outcome and covariate data.
#'
#' @return
#' A list or data.frame of model outputs and summary statistics.
#'
#' @export

get_primary_analysis_LD <- function(matched_data){

  
#df_2 <- df_1Matched %>%
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
               # "entry_quarter",
               # "entry_year",
                "type",
                "coronary_heart_disease_Baseline"	,
                "coronary_heart_disease_Baseline_1_year"	,
                "coronary_heart_disease_Baseline_5_year"	,
                "coronary_heart_disease_first_occurance_date"		,
                "heart_failure_Baseline"	,
                "heart_failure_Baseline_1_year"	,
                "heart_failure_Baseline_5_year"	,
                "heart_failure_first_occurance_date"		,
                "cerebrovascular_disease_Baseline"	,
                "cerebrovascular_disease_Baseline_1_year"	,
                "cerebrovascular_disease_Baseline_5_year"	,
                "cerebrovascular_disease_first_occurance_date"		,
                "dementia_Baseline"	,
                "dementia_Baseline_1_year"	,
                "dementia_Baseline_5_year"	,
                "dementia_first_occurance_date"		,
                "diabetes_Baseline"	,
                "diabetes_Baseline_1_year"	,
                "diabetes_Baseline_5_year"	,
                "diabetes_first_occurance_date"	
                
  )%>%
  as.data.frame()

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
                                "diabetes_Baseline_5_year"	)

df_2 <- df_2[!apply(df_2[ , condition_columns] == -1, 1, any), ]



condition_names = c("coronary_heart_disease", "heart_failure", "cerebrovascular_disease", "dementia", "diabetes")

#df_2 <- as.data.frame(df_2)

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

df_2$atleast_oneLTC_LD_5year <- ifelse(df_2$ltc_baseline_5_year > 0, 1, 0)
df_2$atleast_twoLTC_LD_5year <- ifelse(df_2$ltc_baseline_5_year > 1, 1, 0)
df_2$atleast_threeLTC_LD_5year <- ifelse(df_2$ltc_baseline_5_year > 2, 1, 0)
df_2$atleast_fourLTC_LD_5year <- ifelse(df_2$ltc_baseline_5_year > 3, 1, 0)
df_2$atleast_fiveLTC_LD_5year <- ifelse(df_2$ltc_baseline_5_year > 4, 1, 0)


########Unadjusted IRR#########



outcome_vars <- paste0("atleast_", c("one", "two", "three", "four", "five"), "LTC_LD_5year")

atleast_n_IRR_Unadjusted_5year <- get_multiple_poisson_irr(
  outcome_vars = outcome_vars,
  # predictor = "Reliable_Recovery",
  predictor = "type",
  data = df_2
)


########Adjusted IRR#########
# Run for thresholds 1 to 5
atleast_n_IRR <- lapply(1:5, function(thresh) {
  run_poisson_model_for_ltc_threshold(df = df_2, ltc_threshold = thresh)
  
})
names(atleast_n_IRR) <- paste0("atleast_", 1:5, "_LTCs")

# check individually if there is a warning message or if algorithm doesnot fit




# Names to loop through
ltc_names <- paste0("atleast_", 1:5, "_LTCs")

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

# Output 1
#print(atleast_n_ltc_IRR_type_5year)

################Count percent tabl########################################

ltc_cols <- grep("atleast_.*LTC_LD_5year", names(df_2), value = TRUE)
#ltc_cols = "atleast_oneLTC_LD_5year"
atleast_n_count_percent_5year <- countPercent_fn(df_2, group_column= "type", ltc_cols)

##################################################################################

# for each LTC-LD

#source("primaryAnalysisIndividualLTC.R")

# Ensure 'type' is a factor
#df_2$type <- factor(df_2$type, levels = c("0", "1"))

# List of LTC outcome variables
ltc_vars <- c(
  "dementia_Baseline_5_year",
  "coronary_heart_disease_Baseline_5_year",
  "heart_failure_Baseline_5_year",
  "cerebrovascular_disease_Baseline_5_year",
  "diabetes_Baseline_5_year"
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
  "SourceOfReferralIAPT_Category",
  "GP_Region_Code",
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


library(sandwich)

outcome_vars <- c( "dementia_Baseline_5_year"	,
                   "coronary_heart_disease_Baseline_5_year"	,
                   "heart_failure_Baseline_5_year"	,
                   "cerebrovascular_disease_Baseline_5_year"	,
                   "diabetes_Baseline_5_year")


individual_LTC_IRR_Unadjusted_5year <- get_multiple_poisson_irr(
  outcome_vars = outcome_vars,
  # predictor = "Reliable_Recovery",
  predictor = "type",
  data = df_2
)



#ltc_cols <- grep("atleast_.*LTC_LD_5year", names(df_2), value = TRUE)
#ltc_cols = "atleast_oneLTC_LD_5year"
individual_LTC_count_percent_5year <- countPercent_fn(df_2, group_column = "type", outcome_vars)

# Return as a list
return(list(
  Primary_atleast_n_IRR_Unadjusted_5year      = atleast_n_IRR_Unadjusted_5year,
  Primary_atleast_n_IRR_Adjusted_5year        = atleast_n_IRR_Adjusted_5year,
  Primary_atleast_n_count_percent_5year = atleast_n_count_percent_5year,
  Primary_individual_LTC_IRR_Adjusted_5year = individual_LTC_IRR_Adjusted_5year,
  Priamry_individual_LTC_IRR_Unadjusted_5year = individual_LTC_IRR_Unadjusted_5year,
  Primary_individual_LTC_count_percent_5year  = individual_LTC_count_percent_5year
  
  
))

}