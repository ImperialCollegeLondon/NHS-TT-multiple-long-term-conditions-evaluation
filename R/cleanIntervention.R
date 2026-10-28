#' Clean and standardise intervention cohort data
#'
#' @description
#' Applies standard cleaning and variable derivation steps to intervention
#' cohort datasets. This includes variable renaming, demographic recoding,
#' derivation of grouped variables (e.g. ethnicity, age groups), and removal
#' of duplicate records.
#'
#' The function ensures consistency between intervention and control datasets
#' prior to matching and analysis.
#'
#' @param intervention_df A data.frame or tibble containing raw intervention
#'   cohort data extracted from Azure Data Lake Storage.
#'
#' @return
#' A cleaned data.frame with harmonised variable names and derived covariates,
#' suitable for downstream matching and analysis.
#'
#' @details
#' This function assumes IAPT-standard coding for ethnicity, employment status,
#' deprivation indices, and demographic variables.
#'
#' @examples
#' \dontrun{
#' cleaned_intervention <- get_cleaning_intervention(raw_intervention_df)
#' }
#'
#' @export



get_cleaning_intervention <- function(intervention_df){


# Rename the columns to match with other datasets
df_intervention = intervention_df %>%
  rename(	"EmployStatus"="EmploymentStatus_First",
          "IndicesOfDeprivationDecile"="IMD_Decile"	
  ) %>%
  distinct()

# Ethnic Categrory Grouping: (Ref: IAPT Metadata)
#  White
#  A	British
#  B	Irish
#  C	Any other White background
#  Mixed
#  D	White and Black Caribbean
#  E	White and Black African
#  F	White and Asian
#  G	Any other mixed background
#  Asian or Asian British
#  H	Indian
#  J	Pakistani
#  K	Bangladeshi
#  L	Any other Asian background
#  Black or Black British
#  M	Caribbean
#  N	African
#  P	Any other Black background
#  Other Ethnic Groups
#  R	Chinese
#  S	Any other ethnic group
#  Z	Not Stated
#  99	Not Known 



df_intervention <- df_intervention %>%
  mutate(Ethnic_Group = case_when(
    grepl("^[HJKL]", Validated_EthnicCategory) ~ "Asian",
    grepl("^[MNP]", Validated_EthnicCategory) ~ "Black",
    grepl("^[DEFG]", Validated_EthnicCategory) ~ "Mixed",
    grepl("^[SR]", Validated_EthnicCategory) ~ "Other",
    grepl("^[ABC]", Validated_EthnicCategory) ~ "White",
    grepl("^Z", Validated_EthnicCategory) ~ "Not Stated",
    TRUE ~ "Not Stated"
  ))%>%
  dplyr::select(-Validated_EthnicCategory)

# Convert Indices of deprivation decile to Quintile

df_intervention <- df_intervention %>%
  mutate(IMD_Quintile = case_when(
    IndicesOfDeprivationDecile %in% 1:2 ~ 1,
    IndicesOfDeprivationDecile %in% 3:4 ~ 2,
    IndicesOfDeprivationDecile %in% 5:6 ~ 3,
    IndicesOfDeprivationDecile %in% 7:8 ~ 4,
    IndicesOfDeprivationDecile %in% 9:10 ~ 5,
    TRUE ~ NA_real_  # or another fallback value
  ))%>%
  dplyr::select(-IndicesOfDeprivationDecile)



# Group the Age :

# Min Age at referral is 18, However Age at reference date can go below 18 , as 
# reference date is the start of study period
df_intervention$Age_Group <- cut(df_intervention$age_at_ref_date, 
                                 breaks = c(0 , 39,59,79,Inf), 
                                 labels = c("18-39",
                                            "40-59",
                                            "60-79",
                                            "80+"))
                                 # labels = c("0-18"
                                 #            ,"19-60"
                                 #            ,"60+"),
                                 #right = FALSE



df_intervention = df_intervention %>%
  dplyr::select(-age_at_ref_date)


# Ex-British Armed Airforce Indicator (Ref: IAPT Metadata)( Binary : Yes/No - 
#  There are multiple forms of numbers exist, hence included all forms of 2)
##  02	Ex-services member
##  03	Not an ex-services member or their dependant
##  05	Dependant of an ex-services member
##  UU	Unknown (Person asked and does not know or is not sure)
##  ZZ	Not stated (Person asked but declined to provide a response)

df_intervention$ExBAFIndicator <- ifelse(df_intervention$ExBAFIndicator %in% c("02", "2", 2), 1, 0)

# Baseline LTC Category :
## Number of LTC's at the Baseline
## 0  No LTC
## 1-3 Less than 3 LTC
## >3 More than 3 LTC

df_intervention <- df_intervention %>%
  mutate(
    Baseline_LTC_Category = case_when(
      baseline_total == 0 ~ "No LTC",
      baseline_total >=1 & baseline_total <= 3 ~ "Less than 3 LTC",
      baseline_total > 3 ~ "More than 3 LTC",
      TRUE ~ NA_character_
      
    )
  )%>%
  dplyr::select(-baseline_total)



## EmployStatus (Ref: IAPT Metadata)( Binary : Yes/No - Employed/UnEmplloyed

## 01	Employed 
## 02	Unemployed and Seeking Work 
## 03	Students who are undertaking full (at least 16 hours per week) or part-time (less than 16 hours per week) education or training and who are not working or actively seeking work
## 04	Long-term sick or disabled, those who are receiving Incapacity Benefit, Income Support or both; or Employment and Support Allowance
## 05	Homemaker looking after the family or home and who are not working or actively seeking work 
## 06	Not receiving benefits and who are not working or actively seeking work
## 07	Unpaid voluntary work who are not working or actively seeking work 
## 08	Retired
## ZZ	Not Stated (Person asked but declined to provide a response)
## 


df_intervention$EmployStatus <- ifelse(df_intervention$EmployStatus %in% c("01", "1", "03", "3"), 1, 0)


# Waiting_under_6weeks (Ref: NHS Digital)( Binary : Yes/No - Waited Less than 6 weeks or not)
## As per NHS Digital IAPT Waiting time targets are 75% - under 6 weeks and 95% under 18 weeks
## Since the main target is 6 weeks, and almost 99% got treatment under 18 weeks 
## makes the 3rd category almost equal to null, hence choosing binary.


df_intervention <- df_intervention %>%
  mutate(Waiting_under_6weeks = ifelse(Time_Referral_to_CareContact <= 42, 1, 0))%>%
  dplyr::select(-Time_Referral_to_CareContact)

# SourceOfReferralIAPT_Category (Ref: IAPT Metadata)


## Primary Health Care
##   A1	General Medical Practitioner
##   A2	Health Visitor
##   A3	Other Primary Health Care
## Self Referral
##   B1	Self
##   B2	Carer
## Local Authority Services
##   C1	Social Services
##   C2	Education Service
## Employer
##   D1	Employer
## Justice System
##   E1	Police
##   E2	Courts
##   E3	Probation Service
##   E4	Prison
##   E5	Court Liaison and Diversion Service
## Child Health
##   F1	School Nurse
##   F2	Hospital-based Paediatrics
##   F3	Community-based Paediatrics
## Independent/Voluntary Sector
##   G1	Independent sector - Medium Secure Inpatients
##   G2	Independent Sector - Low Secure Inpatients
##   G3	Other Independent Sector Mental Health Services
##   G4	Voluntary Sector
## Acute Secondary Care
##   H1	Accident And Emergency Department
##   H2	Other secondary care specialty
## Other Mental Health NHS Trust
##   I1	Temporary transfer from another Mental Health NHS Trust
##   I2	Permanent transfer from another Mental Health NHS Trust
## Internal referrals  from Community Mental Health Team (within own NHS Trust)
##   J1	Community Mental Health Team (Adult Mental Health)
##   J2	Community Mental Health Team (Older People)
##   J3	Community Mental Health Team (Learning Disabilities)
##   J4	Community Mental Health Team (Child and Adolescent Mental Health)
## Internal referrals from Inpatient Service (within own NHS Trust)
##   K1	Inpatient Service (Adult Mental Health)
##   K2	Inpatient Service (Older People)
##   K3	Inpatient Service (Forensics)
##   K4	Inpatient Service (Child and Adolescent Mental Health)
##   K5	Inpatient Service (Learning Disabilities)
## Transfer by graduation (within own NHS Trust)
##   L1	Transfer by graduation from Child and Adolescent Mental Health Services to Adult Mental Health Services
##   L2	Transfer by graduation from Adult Mental Health Services to Older Peoples Mental Health Services
## Other
##   M1	Asylum Services
##   M2	NHS DirectTelephone or electronic Access Service
##   M3	Out of Area Agency
##   M4	Drug Action Team / Drug Misuse Agency
##   M5	Jobcentre plus**
##   M6	Other service or agency
## IAPT
##   N1	Stepped up from low intensity IAPT service *
##   N2	Stepped down from high intensity IAPT service *
##   

df_intervention <- df_intervention %>%
  mutate(
    SourceOfReferralIAPT_Category = case_when(
      SourceOfReferralMH %in% c("A1","A2","A3") ~ "Primary_Health_Care" ,
      SourceOfReferralMH %in% c("B1","B2") ~ "Self_Referral" ,
      SourceOfReferralMH %in% c("C1","C2") ~ "Local_Authority_Services" ,
      SourceOfReferralMH %in% c("D1") ~ "Employer" ,
      SourceOfReferralMH %in% c("E1","E2","E3","E4","E5") ~ "Justice_System" ,
      SourceOfReferralMH %in% c("F1","F2","F3") ~ "Child_Health" ,
      SourceOfReferralMH %in% c("G1","G2","G3","G4") ~ "Independent/Voluntary_Sector" ,
      SourceOfReferralMH %in% c("H1","H2") ~ "Acute_Secondary_Care" ,
      SourceOfReferralMH %in% c("I1","I2") ~ "Other_Mental_Health_NHS_Trust" ,
      SourceOfReferralMH %in% c("J1","J2","J3", "J4") ~ "Community_Mental_Health_Team" ,
      SourceOfReferralMH %in% c("K1","K2","K3", "K4", "K5") ~ "Inpatient_Services" ,
      SourceOfReferralMH %in% c("L1","L2") ~ "Transfer_by_graduation" ,
      SourceOfReferralMH %in% c("M1","M2","M3", "M4", "M5" , "M6") ~ "Other" ,
      SourceOfReferralMH %in% c("N1","N2") ~ "IAPT" ,
      is.na(SourceOfReferralMH) ~ NA_character_,
      TRUE ~ NA_character_
    )
  )%>%
  dplyr::select(-SourceOfReferralMH)


# Dosage (Ref: Research Proposal)

##  •	Dosage – Patients that completed ≥ 4 treatment sessions 
## vs patients that completed 2-4 treatment sessions (Clark, 2018) 

df_intervention$Dosage <- ifelse(df_intervention$TreatmentCareContact_Count > 4, 1, 0)
df_intervention <-  df_intervention %>%
  dplyr::select(-TreatmentCareContact_Count)


# Convert Indices of deprivation decile to Quintile

df_intervention <- df_intervention %>%
  mutate(GP_IMD_Quintile = case_when(
    GP_IMD_Decile %in% 1:2 ~ 1,
    GP_IMD_Decile %in% 3:4 ~ 2,
    GP_IMD_Decile %in% 5:6 ~ 3,
    GP_IMD_Decile %in% 7:8 ~ 4,
    GP_IMD_Decile %in% 9:10 ~ 5,
    TRUE ~ NA_real_  # or another fallback value
  ))%>%
  dplyr::select(-GP_IMD_Decile)


## Convert NA to 0 for the AE, APC and OP Appointment counts before baseline

df_intervention$AE_Attendance_Before_Baseline[is.na(df_intervention$AE_Attendance_Before_Baseline)] <- 0
df_intervention$APC_Attendance_Before_Baseline[is.na(df_intervention$APC_Attendance_Before_Baseline)] <- 0
df_intervention$OP_Attendance_Before_Baseline[is.na(df_intervention$OP_Attendance_Before_Baseline)] <- 0


## GP Rural Urban Classification ( Rural/Urban )

df_intervention <- df_intervention %>%
  mutate(GP_Rural_Urban_Classification = case_when(
    grepl("^Rural", GP_Rural_Urban_Classification, ignore.case = TRUE) ~ "Rural",
    grepl("^Urban", GP_Rural_Urban_Classification, ignore.case = TRUE) ~ "Urban",
    TRUE ~ NA_character_
  ))


# Reliable Improvement (Ref: IAPT Research proposal and 
## https://pmc.ncbi.nlm.nih.gov/articles/PMC10163979/pdf/ehad188.pdf )
## In line with IAPT definitions reliable improvement in depression symptoms was 
## (i) a reliable improvement (≥6 points) in depressive symptoms [as measured by the Patient Health Questionnaire 9-item (PHQ-9) score] and
## (ii) no reliable deterioration in anxiety [≥4 points, as measured by the Generalized Anxiety Disorder Scale 7-item (GAD-7)]) symptoms, between the start and end of treatment.




df_intervention <- df_intervention %>%
  mutate(Reliable_Improvement = ifelse((df_intervention$PHQ9_FirstScore - df_intervention$PHQ9_LastScore >= 6 ) &
                                         (df_intervention$GAD_LastScore - df_intervention$GAD_FirstScore < 4), 1, 0))


## removal of Ethnic_Group Not Stated due to very low numbers and missing from the Control groups

df_intervention = df_intervention %>%
  filter(Ethnic_Group != "Not Stated")

return(df_intervention)
}