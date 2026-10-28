################################################################################
# Project: Multiple Long-Term Conditions (MLTC) Research Analysis
#
# Script: main.R
#
# Description:
# This script implements the end-to-end analytical pipeline for the MLTC
# research programme. It extracts intervention and control cohort data from
# Azure Data Lake Storage, performs data cleaning and transformation, constructs
# matched cohorts using propensity score matching, and executes primary,
# secondary, and sensitivity analyses across predefined outcomes.
#
# The outputs are summary datasets for clinical, academic, and stakeholder
# reporting.
#
# Audience:
# - Analysts and data scientists maintaining or extending the pipeline
# - Clinical and academic reviewers assessing methodology
# - Clients and stakeholders reviewing analytical outputs
#
# Author: Anju Paul
# Date: 29/08/2025
#
# Dependencies:
# - Azure authentication via Azure Key Vault
# - Parquet-formatted datasets stored in ADLS Gen2
# - Custom functions sourced from /functions
#
# Notes:
# - This script assumes valid Azure credentials and network access.
# - Outputs are written to CSV using standardised helper functions.
################################################################################



################################################################################
#Workflow Overview
#
#1. Data Access
#    - Authenticate with Azure Key Vault
#    - Retrieve intervention, control, GP practice, and LTC service indicator
#datasets from Azure Data Lake Storage
#
#2. Data Preparation
#    - Standardise formats and dates
#    - Derive age at reference date
#    - Attach IAPT LTC service indicators
#    - Apply cohort-specific cleaning rules
#
#3. Dataset Construction
#    - Select analysis-ready variables
#    - Combine intervention and control cohorts
#    - Attach GP practice region information
#
#4. Matching
#    - Perform propensity score matching for predefined cohort comparisons
#
#5. Analysis
#    - Run primary, secondary, and sensitivity analyses
#    - Generate outputs for reporting
################################################################################



################################################################################
# Package Management
#
# Required packages are defined explicitly. Missing packages are installed
# automatically to ensure reproducibility across environments.
################################################################################


load_packages <- c(
  "AzureStor",
  "AzureKeyVault",
  "arrow",
  "data.table",
  "dplyr",
  "readxl",
  "tableone",
  "survey",
  "reshape2",
  "ggplot2",
  "MatchIt",
  "lubridate",
  "broom",
  "stringr",
  "sandwich",
  "lmtest",
  "tidyr",
  "purrr",
  "rlang"
)

installed <- load_packages %in% installed.packages()
if (length(load_packages[!installed]) > 0)
  install.packages(load_packages[!installed], type = "binary")
lapply(load_packages, library, character.only = TRUE)


#options(scipen = 0)
################################################################################
# Step 1: Data Access and Extraction
#
# This section:
# - Connects to Azure Data Lake Storage using environment variables
# - Loads intervention and control cohorts
# - Loads GP practice mapping data
# - Loads IAPT Long-Term Condition (LTC) service indicator data
#
# NOTE:
# Credentials must be stored in environment variables and MUST NOT be
# committed to version control.
################################################################################

###############################################################################
# Configuration
###############################################################################

USE_LOCAL_DATA <- FALSE  # Set to TRUE for local/sample data

get_storage_container <- function() {
  storage_endpoint_url <- Sys.getenv("AZURE_STORAGE_ENDPOINT")
  sas_token            <- Sys.getenv("AZURE_SAS_TOKEN")
  container_name       <- Sys.getenv("AZURE_CONTAINER")

  if (storage_endpoint_url == "" ||
      sas_token == "" || container_name == "") {
      stop("Azure credentials not found. Configure environment variables.")
  }

  dl_endp <- storage_endpoint(storage_endpoint_url, sas = sas_token)
  storage_container(dl_endp, container_name)
}

###############################################################################
# Generic Parquet Loader
###############################################################################

load_parquet_dataset <- function(container, path, group_label, type_label) {
  files <- list_adls_files(
    container,
    dir = path,
    info = c("all", "name"),
    recursive = FALSE
  )

  parquet_files <- files$name[grepl("\\.parquet$", files$name, ignore.case = TRUE)]

  if (length(parquet_files) == 0) {
    stop(paste("No parquet files found in:", path))
  }

  df <- rbindlist(lapply(parquet_files, function(f) {
    rawdata <- storage_download(container, f, dest = NULL)
    read_parquet(rawdata, scientific = FALSE)
  }))

  df[, Group := group_label] %>%
    mutate(type = type_label)
}

###############################################################################
# Load Data
###############################################################################

if (USE_LOCAL_DATA) {
  message("Loading local sample datasets...")

  intervention1N2_df <- readRDS("data/sample_intervention12.rds")
  intervention3_df   <- readRDS("data/sample_intervention3.rds")
  control1_df        <- readRDS("data/sample_control1.rds")
  control2_df        <- readRDS("data/sample_control2.rds")
  control3_df        <- readRDS("data/sample_control3.rds")
  gp_data            <- readRDS("data/sample_gp_data.rds")
  LTC_ServiceInd_df  <- readRDS("data/sample_ltc_indicator.rds")

} else {
  message("Connecting to Azure Data Lake...")

  cont <- get_storage_container()

  # -----------------------------
  # Intervention Cohorts
  # -----------------------------

  intervention1N2_df <- load_parquet_dataset(cont,
                                             "IAPT_Intervention_1_2_Patients.parquet",
                                             "intervention",
                                             "case")

  intervention3_df <- load_parquet_dataset(cont,
                                           "IAPT_Intervention_3_Patients.parquet",
                                           "intervention",
                                           "case")

  # -----------------------------
  # Control Cohorts
  # -----------------------------

  control1_df <- load_parquet_dataset(cont,
                                      "IAPT_Control_1_Patients.parquet",
                                      "control",
                                      "control")

  control2_df <- load_parquet_dataset(cont,
                                      "IAPT_Control_2_Patients.parquet",
                                      "control",
                                      "control")

  control3_df <- load_parquet_dataset(cont,
                                      "IAPT_Control_3_Patients.parquet",
                                      "control",
                                      "control")

  # -----------------------------
  # GP Practice Mapping
  # -----------------------------

  gp_raw <- storage_download(cont,
                             "UKHD_Demography_Reg_Patients_GP_Practice_Map_SCD_00000.parquet",
                             dest = NULL)

  gp_data <- read_parquet(gp_raw, scientific = FALSE) %>%
    filter(Is_Latest == 1) %>%
    select(Practice_Code, Comm_Region_Code)

  # -----------------------------
  # IAPT LTC Service Indicator
  # -----------------------------

  tmpfile <- tempfile(fileext = ".xlsx")

  storage_download(cont, "IAPT_LTCService_list.xlsx", dest = tmpfile)

  LTC_ServiceInd_df <- read_excel(tmpfile)

  LTC_ServiceInd_df <- LTC_ServiceInd_df %>%
    mutate(
      Organisation_name_updated = case_when(
        Organisation_code == "RKE" ~ "THE WHITTINGTON HOSPITAL NHS TRUST",
        Organisation_code == "NQL" ~ "NAVIGO HEALTH AND SOCIAL CARE CIC",
        Organisation_code == "RRE" ~ "MIDLANDS PARTNERSHIP NHS FOUNDATION TRUST",
        TRUE ~ Organisation_name
      )
    ) %>%
    dplyr::select(-Organisation_name) %>%
    distinct()

  LTC_ServiceInd_df <- LTC_ServiceInd_df %>%
    filter(!(Organisation_code  == "NQL" &
               Date == "2018-01-08")) %>%
    filter(!(Organisation_code  == "RRE" & Date == "2018-01-08"))


  LTC_ServiceInd_df <- unique(LTC_ServiceInd_df)

  LTC_ServiceInd_df <- LTC_ServiceInd_df[!duplicated(
    LTC_ServiceInd_df$Organisation_code), ]


}

###############################################################################
# Basic Validation
###############################################################################

message("Intervention 1&2: ", nrow(intervention1N2_df))
message("Intervention 3: ", nrow(intervention3_df))
message("Control 1: ", nrow(control1_df))
message("Control 2: ", nrow(control2_df))
message("Control 3: ", nrow(control3_df))


################################################################################
# Step 2: Data Cleaning and Variable Derivation
#
# This section:
# - Attaches LTC service indicators to patient records
# - Calculates age at a fixed reference date (01-04-2016)
# - Applies cohort-specific cleaning rules via custom helper functions
################################################################################


# intervention1N2_df

intervention1N2_df <- intervention1N2_df %>%
  left_join(LTC_ServiceInd_df,
            by = c("OrgID_Provider" = "Organisation_code"))
intervention1N2_df$Date <- as.Date(intervention1N2_df$Date)

intervention1N2_df <- intervention1N2_df %>%
  mutate(IAPTLTCServiceInd = (ifelse(Date <= Baseline_date, 1, 0)))  %>%
  dplyr::select(!c("Organisation_name_updated", "Date"))

intervention1N2_df$IAPTLTCServiceInd <- ifelse(
  is.na(intervention1N2_df$IAPTLTCServiceInd),
  0,
  intervention1N2_df$IAPTLTCServiceInd
)


# intervention3_df
intervention3_df <- intervention3_df %>%
  left_join(LTC_ServiceInd_df,
            by = c("OrgID_Provider" = "Organisation_code"))
intervention3_df$Date <- as.Date(intervention3_df$Date)

intervention3_df <- intervention3_df %>%
  mutate(IAPTLTCServiceInd = (ifelse(Date <= Baseline_date, 1, 0)))  %>%
  dplyr::select(!c("Organisation_name_updated", "Date"))

intervention3_df$IAPTLTCServiceInd <- ifelse(is.na(
                                            intervention3_df$IAPTLTCServiceInd),
                                             0,
                                             intervention3_df$IAPTLTCServiceInd)


# Control3_df
control3_df <- control3_df %>%
  left_join(LTC_ServiceInd_df,
            by = c("OrgID_Provider" = "Organisation_code"))

control3_df$Date <- as.Date(control3_df$Date)
control3_df <- control3_df %>%
  mutate(IAPTLTCServiceInd = (ifelse(Date <= Baseline_date, 1, 0)))  %>%
  dplyr::select(!c("Organisation_name_updated", "Date"))

control3_df$IAPTLTCServiceInd <- ifelse(is.na(control3_df$IAPTLTCServiceInd),
                                        0,
                                        control3_df$IAPTLTCServiceInd)





################Add Referene Date and calculate the age at ref date##########
# Reference date : Start of Study
ref_date <- as.Date("2016-04-01")

# Calculate time difference in years
control1_df$age_at_ref_date <- round(control1_df$Age_Assessment_FirstDate +
                                       as.numeric(difftime(
                                                      ref_date,
                                                      control1_df$Baseline_date,
                                                      units = "days"
  )) / 365.25, 0)

control2_df$age_at_ref_date <- round(control2_df$Age_Assessment_FirstDate +
                                       as.numeric(difftime(
                                                      ref_date,
                                                      control2_df$Baseline_date,
                                                      units = "days"
  )) / 365.25, 0)


control3_df$age_at_ref_date <- round(control3_df$Age_Assessment_FirstDate +
                                       as.numeric(difftime(
                                                      ref_date,
                                                      control3_df$Baseline_date,
                                                      units = "days"
  )) / 365.25, 0)


intervention1N2_df$age_at_ref_date <- round(
                    intervention1N2_df$Age_Assessment_FirstDate +
                    as.numeric(difftime(
                                  ref_date,
                                  intervention1N2_df$Baseline_date,
                                  units = "days"
  )) / 365.25, 0)

intervention3_df$age_at_ref_date <- round(
                  intervention3_df$Age_Assessment_FirstDate +
                  as.numeric(difftime(
                                ref_date,
                                intervention3_df$Baseline_date,
                                units = "days")
) / 365.25, 0)

################################################################################
# Custom Functions
#
# All reusable data cleaning, matching, and analysis logic is implemented
# in external scripts stored in the /functions or R directory.
################################################################################



# Load custom functions
invisible(lapply(Sys.glob("R/*.R"), function(x)
                   source(x)))


# cleaning

df_intervention1N2 <- get_cleaning_intervention(intervention1N2_df)
df_intervention3 <- get_cleaning_intervention(intervention3_df)
df_control1 <- get_cleaning_control(control1_df)
df_control2 <- get_cleaning_control(control2_df)
df_control3 <- get_cleaning_intervention(control3_df)



# Baseline characteristics
### this script needs to be updated as per the requirements of the clinicians


################################################################################
# Step 3: Dataset Construction
#
# Intervention and control cohorts are reduced to a common set of analysis
# variables and combined into comparison-specific datasets.
################################################################################



# Combine the intervention and control df


### Select the necessary columns from the control df and intervention df
column_list <- c(
  "pseudo_nhs_number_ncdr",
  "Baseline_date",
  "LSOA",
  "Person_ID",
  "Gender",
  "Postcode_District",
  "ExBAFIndicator",
  "GMPCodeReg",
  "PHQ9_FirstScore",
  "PHQ9_LastScore",
  "GAD_FirstScore",
  "GAD_LastScore",
  "Validated_PresentingComplaint",
  "OnsetDate",
  "Assessment_FirstDate",
  "Assessment_LastDate",
  "TherapySession_FirstDate",
  "TherapySession_LastDate",
  "ReferralRequestReceivedDate",
  "Age_ReferralRequest_ReceivedDate",
  "EmployStatus",
  "CareContact_Count",
  "EndCode",
  "Age_Assessment_FirstDate",
  "Assessment_FirstDate_Quarter",
  "OrgID_Provider",
  "ConsMechanism_Calc",
  "TherapyType_Level",
  "DNA_Rate",
  "EmpSupport",
  "Reliable_Recovery",
  "GP_GMPC",
  "GP_Name",
  "GP_Postcode",
  "GP_LSOA_Code",
  "GP_LSOA_Name",
  "GP_Rural_Urban_Classification",
  "GP_Total_Size",
  "GP_FTE_Ratio",
  "Achieved_Points",
  "Revised_Max_Points_Total",
  "GP_QOF_Percentage_Achieved",
  "GP_QOF_Effective_Snapshot_Date",
  "AE_Attendance_Before_Baseline",
  "APC_Attendance_Before_Baseline",
  "OP_Attendance_Before_Baseline",
  "Baseline",
  "baseline_date_plus_1_year",
  "baseline_date_plus_5_years",
  "Baseline_1_year",
  "Baseline_5_years",
  "alcohol_dependence_Baseline",
  "atrial_fibrillation_Baseline",
  "bronchiectasis_Baseline",
  "cancer_Baseline",
  "cerebrovascular_disease_Baseline",
  "chronic_kidney_disease_Baseline",
  "chronic_liver_disease_Baseline",
  "chronic_pain_Baseline",
  "copd_Baseline",
  "coronary_heart_disease_Baseline",
  "cystic_fibrosis_Baseline",
  "diabetes_Baseline",
  "epilepsy_Baseline",
  "heart_failure_Baseline",
  "hypertension_Baseline",
  "inflammatory_bowel_disease_Baseline",
  "multiple_sclerosis_Baseline",
  "parkinsons_disease_Baseline",
  "peripheral_vascular_disease_Baseline",
  "rheumatoid_arthritis_Baseline",
  "sickle_cell_disease_Baseline",
  "Autism_Baseline",
  "learning_disability_Baseline",
  "dementia_Baseline",
  "frailty_risk_Baseline",
  "osteoarthritis_Baseline",
  "osteoporosis_Baseline",
  "Asthma_Baseline",
  "pulmonary_heart_disease_Baseline",
  "neurological_organ_failure_Baseline",
  "sarcoidosis_Baseline",
  "severe_interstitial_lung_disease_Baseline",
  "Physical_Disability_Baseline",
  "alcohol_dependence_Baseline_1_year",
  "atrial_fibrillation_Baseline_1_year",
  "bronchiectasis_Baseline_1_year",
  "cancer_Baseline_1_year",
  "cerebrovascular_disease_Baseline_1_year",
  "chronic_kidney_disease_Baseline_1_year",
  "chronic_liver_disease_Baseline_1_year",
  "chronic_pain_Baseline_1_year",
  "copd_Baseline_1_year",
  "coronary_heart_disease_Baseline_1_year",
  "cystic_fibrosis_Baseline_1_year",
  "diabetes_Baseline_1_year",
  "epilepsy_Baseline_1_year",
  "heart_failure_Baseline_1_year",
  "hypertension_Baseline_1_year",
  "inflammatory_bowel_disease_Baseline_1_year",
  "multiple_sclerosis_Baseline_1_year",
  "parkinsons_disease_Baseline_1_year",
  "peripheral_vascular_disease_Baseline_1_year",
  "rheumatoid_arthritis_Baseline_1_year",
  "sickle_cell_disease_Baseline_1_year",
  "Autism_Baseline_1_year",
  "learning_disability_Baseline_1_year",
  "dementia_Baseline_1_year",
  "frailty_risk_Baseline_1_year",
  "osteoarthritis_Baseline_1_year",
  "osteoporosis_Baseline_1_year",
  "Asthma_Baseline_1_year",
  "pulmonary_heart_disease_Baseline_1_year",
  "neurological_organ_failure_Baseline_1_year",
  "sarcoidosis_Baseline_1_year",
  "severe_interstitial_lung_disease_Baseline_1_year",
  "Physical_Disability_Baseline_1_year",
  "alcohol_dependence_Baseline_5_year",
  "atrial_fibrillation_Baseline_5_year",
  "bronchiectasis_Baseline_5_year",
  "cancer_Baseline_5_year",
  "cerebrovascular_disease_Baseline_5_year",
  "chronic_kidney_disease_Baseline_5_year",
  "chronic_liver_disease_Baseline_5_year",
  "chronic_pain_Baseline_5_year",
  "copd_Baseline_5_year",
  "coronary_heart_disease_Baseline_5_year",
  "cystic_fibrosis_Baseline_5_year",
  "diabetes_Baseline_5_year",
  "epilepsy_Baseline_5_year",
  "heart_failure_Baseline_5_year",
  "hypertension_Baseline_5_year",
  "inflammatory_bowel_disease_Baseline_5_year",
  "multiple_sclerosis_Baseline_5_year",
  "parkinsons_disease_Baseline_5_year",
  "peripheral_vascular_disease_Baseline_5_year",
  "rheumatoid_arthritis_Baseline_5_year",
  "sickle_cell_disease_Baseline_5_year",
  "Autism_Baseline_5_year",
  "learning_disability_Baseline_5_year",
  "dementia_Baseline_5_year",
  "frailty_risk_Baseline_5_year",
  "osteoarthritis_Baseline_5_year",
  "osteoporosis_Baseline_5_year",
  "Asthma_Baseline_5_year",
  "pulmonary_heart_disease_Baseline_5_year",
  "neurological_organ_failure_Baseline_5_year",
  "sarcoidosis_Baseline_5_year",
  "severe_interstitial_lung_disease_Baseline_5_year",
  "Physical_Disability_Baseline_5_year",
  "followup_total",
  "alcohol_dependence_first_occurance_date",
  "atrial_fibrillation_first_occurance_date",
  "bronchiectasis_first_occurance_date",
  "cancer_first_occurance_date",
  "cerebrovascular_disease_first_occurance_date",
  "chronic_kidney_disease_first_occurance_date",
  "chronic_liver_disease_first_occurance_date",
  "chronic_pain_first_occurance_date",
  "copd_first_occurance_date",
  "coronary_heart_disease_first_occurance_date",
  "cystic_fibrosis_first_occurance_date",
  "diabetes_first_occurance_date",
  "epilepsy_first_occurance_date",
  "heart_failure_first_occurance_date",
  "hypertension_first_occurance_date",
  "inflammatory_bowel_disease_first_occurance_date",
  "multiple_sclerosis_first_occurance_date",
  "parkinsons_disease_first_occurance_date",
  "peripheral_vascular_disease_first_occurance_date",
  "rheumatoid_arthritis_first_occurance_date",
  "sickle_cell_disease_first_occurance_date",
  "Autism_first_occurance_date",
  "learning_disability_first_occurance_date",
  "dementia_first_occurance_date",
  "frailty_risk_first_occurance_date",
  "osteoarthritis_first_occurance_date",
  "osteoporosis_first_occurance_date",
  "Asthma_first_occurance_date",
  "pulmonary_heart_disease_first_occurance_date",
  "neurological_organ_failure_first_occurance_date",
  "sarcoidosis_first_occurance_date",
  "severe_interstitial_lung_disease_first_occurance_date",
  "Physical_Disability_first_occurance_date",
  "date_of_death",
  "Group",
  "type",
  "IAPTLTCServiceInd",
  "Ethnic_Group",
  "Age_Group",
  "IMD_Quintile",
  "Baseline_LTC_Category",
  "Waiting_under_6weeks",
  "SourceOfReferralIAPT_Category",
  "Dosage",
  "GP_IMD_Quintile",
  "Reliable_Improvement"
)

df_intervention1N2 <- df_intervention1N2 %>%
  dplyr::select(column_list)


df_intervention3 <- df_intervention3 %>%
  dplyr::select(column_list)


df_control1 <- df_control1 %>%
  dplyr::select(column_list)


df_control2 <- df_control2 %>%
  dplyr::select(column_list)


df_control3 <- df_control3 %>%
  dplyr::select(column_list)


## rbind the intervention and control dfs



df_combined1 <- rbind(df_intervention1N2, df_control1)
df_combined2 <- rbind(df_intervention3, df_control2)
df_combined3 <- rbind(df_intervention1N2, df_control3)
df_combined4 <- rbind(df_intervention3, df_control3)



### Some more cleaning

library(dplyr)

df_combined1 <- get_cleaning_combined(df_combined1)
df_combined2 <- get_cleaning_combined(df_combined2)
df_combined3 <- get_cleaning_combined(df_combined3)
df_combined4 <- get_cleaning_combined(df_combined4)


# Join the GP Practice data

df_combined1 <-  df_combined1 %>%
  left_join(gp_data, by = c("GP_GMPC" = "Practice_Code")) %>%
  rename("GP_Region_Code" = "Comm_Region_Code")


df_combined2 <-  df_combined2 %>%
  left_join(gp_data, by = c("GP_GMPC" = "Practice_Code")) %>%
  rename("GP_Region_Code" = "Comm_Region_Code")



df_combined3 <-  df_combined3 %>%
  left_join(gp_data, by = c("GP_GMPC" = "Practice_Code")) %>%
  rename("GP_Region_Code" = "Comm_Region_Code")



df_combined4 <-  df_combined4 %>%
  left_join(gp_data, by = c("GP_GMPC" = "Practice_Code")) %>%
  rename("GP_Region_Code" = "Comm_Region_Code")

df_combined1_cleaned <- get_cleaning_full_dataset(df_combined1)

df_combined2_cleaned <- get_cleaning_full_dataset(df_combined2)

################################################################################
# Step 4: Propensity Score Matching
#
# Matching is performed separately for each predefined intervention–control
# comparison using a standardised matching function.
################################################################################


matched_data_1 <- get_matching(df_combined1, "Int1N2Cont1")
matched_data_2 <- get_matching(df_combined2, "Int3Cont2")
matched_data_3 <- get_matching(df_combined3, "Int1N2Cont3")
matched_data_4 <- get_matching(df_combined4, "Int3Cont3")


################################################################################
# Step 5: Analysis
#
# This section executes:
# - Primary outcome analyses
# - Secondary outcome analyses
# - Sensitivity analyses
#
# Each analysis returns structured outputs that are exported for reporting.
################################################################################

#Outcome 1 : The incidence of LTCs that are aetiologically linked to depression
#(LTC-LD) : Interventions 1 & 2 (Treated Patients before follow up) vs
#Control 3 (attend only assessment session before follow up)

primary_analysis_1 <- get_primary_analysis_LD(matched_data_3)

#Outcome 2 : The incidence of LTCs that are not known to be aetiologically
#linked to depression (LTC-PLD) : Interventions 1 & 2 (Treated Patients before
#follow up) vs Control 3 (attend only assessment session before follow up)

primary_analysis_2 <- get_primary_analysis_PLD(matched_data_3)

#Outcome 3 : The incidence of LTCs that are aetiologically linked to depression
#(LTC-LD) : Interventions 1 & 2 (Treated Patients before follow up) vs
#Control 1 (Treated Patients after follow-up)

sensitivity_analysis_1 <- get_primary_analysis_LD(matched_data_1)


#Outcome 4 : The incidence of LTCs that are possibly aetiologically linked to
#depression (LTC-PLD) : Interventions 1 & 2 (Treated Patients before follow up)
#vs Control 1 (Treated Patients after follow-up)

sensitivity_analysis_2 <- get_primary_analysis_PLD(matched_data_1)

#Outcome 5 : The incidence of LTCs that are aetiologically linked to depression
#(LTC-LD)(AT analysis) : Intervention 3 ‘As treated’ group (Patients with at
#least an assessment before follow up) vs Control 3 (attend only assessment
#session before follow up)

as_treated_analysis_1 <- get_primary_analysis_LD(matched_data_4)

#Outcome 6 : The incidence of LTCs that are aetiologically linked to depression
#(LTC-LD)(AT analysis) : Intervention 3 ‘As treated’ group (Patients with at
#least an assessment before follow up) vs Control 3 (attend only assessment
# session before follow up)

as_treated_analysis_1 <- get_primary_analysis_PLD(matched_data_4)


#Outcome 7 : The incidence of LTCs that are aetiologically linked to depression
#(LTC-LD). (full data set) : Interventions 1 (Treated &
#Reliably Recovered Patients) vs Intervention 2 (Treated &
#Not Reliably Recovered Patients)
#  And
#Outcome 8 : The incidence of LTCs that are aetiologically linked to depression
# (LTC-LD). (full data set) : Interventions 1 (Treated & Reliably
# Improved Patients) vs Intervention 2(Treated & Not Reliably Improved Patients)

secondary_analysis_full_6_13 <- get_sensitivity_analysis_LD(
                                                           df_combined1_cleaned)

#Outcome 9 : The incidence of LTCs that are possibly aetiologically linked to
# depression (LTL-PLD) (full data set) : Interventions 1
# (Treated & Reliably Recovered Patients) vs Intervention 2
# (Treated & Not Reliably Recovered Patients)
#  And
#Outcome 10 : The incidence of LTCs that are possibly aetiologically linked to
# depression (LTL-PLD) (full data set) : Interventions 1 (Treated & Reliably
# Improved Patients) vs Intervention 2(Treated & Not Reliably Improved Patients)

secondary_analysis_full_7_14 <- get_sensitivity_analysis_PLD(
                                                          df_combined1_cleaned)


## Output
# Get the list variable name automatically

get_save_lists_to_csv(
  primary_analysis_1,
  primary_analysis_2,
  sensitivity_analysis_1,
  sensitivity_analysis_2,
  as_treated_analysis_1,
  as_treated_analysis_1,
  secondary_analysis_full_6_13,
  secondary_analysis_full_7_14
)
