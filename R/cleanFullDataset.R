#' Clean full (unmatched) dataset for sensitivity analyses
#'
#' @description
#' Prepares the full combined dataset (without matching) for sensitivity
#' analyses by applying consistent cleaning and variable derivation rules.
#'
#' @param df A data.frame containing combined intervention and control records.
#'
#' @return
#' A cleaned dataset suitable for full-cohort sensitivity analyses.
#'
#' @export

get_cleaning_full_dataset <- function(df_1) {

  
df_1$Gender <- relevel(factor(df_1$Gender) , ref = "2")
df_1$ExBAFIndicator <- relevel(factor(df_1$ExBAFIndicator) , ref = "0")
df_1$TherapyType_Level <- relevel(factor(df_1$TherapyType_Level) , ref = "Mixed")
df_1$EmployStatus <- relevel(factor(df_1$EmployStatus) , ref = "1")
df_1$GP_Rural_Urban_Classification <- relevel(factor(df_1$GP_Rural_Urban_Classification) , ref = "Urban")
df_1$Ethnic_Group <- relevel(factor(df_1$Ethnic_Group) , ref = "White")
df_1$IMD_Quintile <- relevel(factor(df_1$IMD_Quintile) , ref = "5")
df_1$Age_Group <- relevel(factor(df_1$Age_Group) , ref = "18-39")
df_1$Baseline_LTC_Category <- relevel(factor(df_1$Baseline_LTC_Category) , ref = "No LTC")
df_1$Waiting_under_6weeks <- relevel(factor(df_1$Waiting_under_6weeks) , ref = "1")
df_1$Dosage <- relevel(factor(df_1$Dosage) , ref = "1")
df_1$ConsMechanism_Calc <- relevel(factor(df_1$ConsMechanism_Calc) , ref = "F2F")
df_1$EmpSupport <- relevel(factor(df_1$EmpSupport) , ref = "0")
df_1$SourceOfReferralIAPT_Category <- relevel(factor(df_1$SourceOfReferralIAPT_Category) , ref = "1")
df_1$IAPTLTCServiceInd <- relevel(factor(df_1$IAPTLTCServiceInd) , ref = "0")


df_1 <- df_1[df_1$AE_Attendance_Before_Baseline <= 10, ]
df_1 <- df_1[df_1$APC_Attendance_Before_Baseline <= 10, ]
df_1 <- df_1[df_1$OP_Attendance_Before_Baseline <= 10, ]


df_1 <- df_1[!is.na(df_1$GP_Name), ]
df_1 <- df_1[!is.na(df_1$GP_Region_Code), ]  
df_1 <- df_1[df_1$GP_FTE_Ratio <= 20, ]

df_1_cleaned <- df_1[complete.cases(df_1[, c("pseudo_nhs_number_ncdr"	,
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
                                             "GP_IMD_Quintile" ,
                                             "GP_Region_Code",
                                             "IAPTLTCServiceInd",
                                             
                                             "type")]), ]

df_1_cleaned$Baseline_date <- as.Date(df_1_cleaned$Baseline_date )  # Ensure it's a Date object

# # Option 1: Extract year
# df_1_cleaned$entry_year <- format(df_1_cleaned$Baseline_date, "%Y")
# 
# # Option 2: Extract quarter (e.g., "2016 Q2")
# 
# df_1_cleaned$entry_quarter <- paste0(year(df_1_cleaned$Baseline_date), " Q", quarter(df_1_cleaned$Baseline_date))

df_1_cleaned$ConsMechanism_Calc <- ifelse(df_1_cleaned$ConsMechanism_Calc == "Remote", 1, 0)

###### duplicates

df_1_cleaned <- df_1_cleaned %>%
  arrange(pseudo_nhs_number_ncdr, desc(Baseline_date)) %>%
  distinct(pseudo_nhs_number_ncdr, .keep_all = TRUE)

return(df_1_cleaned)
}