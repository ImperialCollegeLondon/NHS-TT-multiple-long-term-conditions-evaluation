#' Sensitivity analysis for outcome measures
#'
#' @description
#' Performs sensitivity analyses for specified outcomes using alternative
#' datasets 
#'
#' @param df A matched or full cleaned dataset.
#'
#' @return
#' Sensitivity analysis results.
#'
#' @export

get_sensitivity_analysis_LD <- function(matched_data){

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
                
  ) %>%
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



df_2$atleast_oneLTC_LD_5year <- ifelse(df_2$ltc_baseline_5_year > 0, 1, 0)
df_2$atleast_twoLTC_LD_5year <- ifelse(df_2$ltc_baseline_5_year > 1, 1, 0)
df_2$atleast_threeLTC_LD_5year <- ifelse(df_2$ltc_baseline_5_year > 2, 1, 0)
df_2$atleast_fourLTC_LD_5year <- ifelse(df_2$ltc_baseline_5_year > 3, 1, 0)
df_2$atleast_fiveLTC_LD_5year <- ifelse(df_2$ltc_baseline_5_year > 4, 1, 0)


## get the intervention data alone

df_2_intervention <-  df_2 %>%
  filter(type == 1)


## Add reliable improvement column
df_2_intervention$Reliable_Improvement <-  ifelse( (df_2_intervention$PHQ9_FirstScore - df_2_intervention$PHQ9_LastScore >= 6) &
                                                     (df_2_intervention$GAD_LastScore - df_2_intervention$GAD_FirstScore < 4 ), 1, 0)

##############Reliable Recovery######################################

##############Unadjusted Analysis###############################

outcome_vars <- paste0("atleast_", c("one", "two", "three", "four", "five"), "LTC_LD_5year")

Sensitivity_atleast_n_IRR_Unadjusted_5year_RR <- get_multiple_poisson_irr(
  outcome_vars = outcome_vars,
  predictor = "Reliable_Recovery",
  # predictor = "Reliable_Improvement",
  data = df_2_intervention
)



#############Adjusted Analysis####################


# Run for thresholds 1 to 5
sensitivity_atleast_n_IRR <- lapply(1:5, function(thresh) {
  sensitivity_Atleast_n_IRR_Adjusted_5year(df = df_2_intervention, ltc_threshold = thresh)
  
})
names(sensitivity_atleast_n_IRR) <- paste0("atleast_", 1:5, "_LTCs")


# Names to loop through
ltc_names <- paste0("atleast_", 1:5, "_LTCs")

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

# Output 1

###Count Percentage

outcome_vars <- paste0("atleast_", c("one", "two", "three", "four", "five"), "LTC_LD_5year")


Sensitivity_atleast_n_LD_5year_count_percent_RR <- get_ReliableRecoverycountPercent_fn(df_2_intervention, outcome_vars)




#####Adjusted LTC's#########################

ltc_cols <- c(
  "dementia_Baseline_5_year",
  "coronary_heart_disease_Baseline_5_year",
  "heart_failure_Baseline_5_year",
  "cerebrovascular_disease_Baseline_5_year",
  "diabetes_Baseline_5_year"
)

poisson_ltc_results <- get_ltc_poisson_results(ltc_cols, df_2_intervention)

Sensitivity_individual_LTC_IRR_Adjusted_5year_RR <- poisson_ltc_results %>%
  filter(Variable == "Reliable_Recovery1")



Sensitivity_individual_LTC_IRR_Unadjusted_5year_RR <- get_multiple_poisson_irr(
  outcome_vars = ltc_cols,
  # predictor = "Reliable_Recovery",
  predictor = "Reliable_Recovery",
  data = df_2_intervention
)



Sensitivity_individual_LTC_5year_count_percent_RR <- get_ReliableRecoverycountPercent_fn(df_2_intervention, ltc_cols)

#print(sensitivity_individual_ltc5year_count_percent)


############################Reliable Improvement##############################################

##############Unadjusted Analysis###############################

outcome_vars <- paste0("atleast_", c("one", "two", "three", "four", "five"), "LTC_LD_5year")

Sensitivity_atleast_n_IRR_Unadjusted_5year_RI <- get_multiple_poisson_irr(
  outcome_vars = outcome_vars,
  predictor = "Reliable_Improvement",
  data = df_2_intervention
)




#############Adjusted Analysis####################


# Run for thresholds 1 to 5
sensitivity_atleast_n_IRR_IR <- lapply(1:5, function(thresh) {
  sensitivity_Atleast_n_IRR_Adjusted_RI_5year(df = df_2_intervention, ltc_threshold = thresh)
  
})
names(sensitivity_atleast_n_IRR_IR) <- paste0("atleast_", 1:5, "_LTCs")

# check individually if there is a warning message or if algorithm doesnot fit


# Names to loop through
ltc_names <- paste0("atleast_", 1:5, "_LTCs")

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




Sensitivity_atleast_n_LD_5year_count_percent_RI <- get_ReliableImprovementcountPercent_fn(df_2_intervention, outcome_vars)



#####Adjusted LTC's#########################

poisson_ltc_results_RI <- get_ltc_poisson_results_RI(ltc_cols, df_2_intervention)

Sensitivity_individual_LTC_IRR_Adjusted_5year_RI <- poisson_ltc_results_RI %>%
  filter(Variable == "Reliable_Improvement")



######Unadjusted


outcome_vars <- c( "dementia_Baseline_5_year"	,
                   "coronary_heart_disease_Baseline_5_year"	,
                   "heart_failure_Baseline_5_year"	,
                   "cerebrovascular_disease_Baseline_5_year"	,
                   "diabetes_Baseline_5_year")


Sensitivity_individual_LTC_IRR_Unadjusted_5year_RI <- get_multiple_poisson_irr(
  outcome_vars = outcome_vars,
  # predictor = "Reliable_Recovery",
  predictor = "Reliable_Improvement",
  data = df_2_intervention
)



#Count percent

Sensitivity_individual_LTC_5year_count_percent_RI <- get_ReliableImprovementcountPercent_fn(df_2_intervention, outcome_vars)


#########################################
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
