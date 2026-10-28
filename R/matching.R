#' Perform propensity score matching
#'
#' @description
#' Conducts propensity score matching between intervention and control cohorts
#' using predefined covariates. Matching is performed separately for each
#' comparison scenario.
#'
#' @param df A cleaned combined dataset containing both intervention and control
#'   observations.
#' @param comparison_name A character string identifying the comparison
#'   (used for labelling and outputs).
#'
#' @return
#' A matched dataset containing only matched intervention and control records.
#'
#' @details
#' Matching methodology and covariate selection are defined internally to
#' ensure consistency across analyses.
#'
#' @export

get_matching <- function(df_combined, varname) {

vars <- c("PHQ9_FirstScore",
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
          "DNA_Rate",
          "SourceOfReferralIAPT_Category",
          "GP_Region_Code",
          "IAPTLTCServiceInd") 

factorvars <- c("Gender",
                "ExBAFIndicator",
                "TherapyType_Level",
                "EmployStatus",
                "GP_Rural_Urban_Classification",
                "Ethnic_Group",
                "IMD_Quintile",
                "Age_Group",
                "Waiting_under_6weeks",
                "Dosage",
                "GP_IMD_Quintile",
                "ConsMechanism_Calc",
                "EmpSupport",
                "Baseline_LTC_Category",
                "SourceOfReferralIAPT_Category",
                "GP_Region_Code",
                "IAPTLTCServiceInd"
)
# Remove "Dosage" if varname == "Int1N2Cont3"
if (varname == "Int1N2Cont3") {
  vars <- setdiff(vars, "Dosage")
  factorvars <- setdiff(factorvars, "Dosage")
}

df_1 <- df_combined



# Releveling factors
relevel_vars <- list(
  Gender = "2", ExBAFIndicator = "0", TherapyType_Level = "Mixed",
  EmployStatus = "1", GP_Rural_Urban_Classification = "Urban",
  Ethnic_Group = "White", IMD_Quintile = "5", Age_Group = "18-39",
  Baseline_LTC_Category = "No LTC", Waiting_under_6weeks = "1",
  Dosage = "1", ConsMechanism_Calc = "F2F", EmpSupport = "0",
  SourceOfReferralIAPT_Category = "1", IAPTLTCServiceInd = "0"
)


for (var in names(relevel_vars)) {
  if (var %in% names(df_1)) {
    df_1[[var]] <- relevel(factor(df_1[[var]]), ref = relevel_vars[[var]])
  }
}

# Remove outliers from the Hospital Activity count as most of the distribution is less than 10
df_1 <- df_1[df_1$AE_Attendance_Before_Baseline <= 10, ]
df_1 <- df_1[df_1$APC_Attendance_Before_Baseline <= 10, ]
df_1 <- df_1[df_1$OP_Attendance_Before_Baseline <= 10, ]


# Remove null GP name to avoid null in the 6 GP related covariates
df_1 <- df_1[!is.na(df_1$GP_Name), ]
df_1 <- df_1[!is.na(df_1$GP_Region_Code), ]  

#  Remove outliers from the GP FTE Ratio as most of the distribution is less than 10
df_1 <- df_1[df_1$GP_FTE_Ratio <= 20, ]


required_cols <- c(vars, "type", "pseudo_nhs_number_ncdr", "Baseline_date")
required_cols <- unique(required_cols[required_cols %in% names(df_1)])

df_1_cleaned <- df_1[complete.cases(df_1[,  required_cols]), ]

df_1_cleaned$Baseline_date <- as.Date(df_1_cleaned$Baseline_date )  # Ensure it's a Date object

# Option 1: Extract year
# df_1_cleaned$entry_year <- format(df_1_cleaned$Baseline_date, "%Y")

# Option 2: Extract quarter (e.g., "2016 Q2")

# df_1_cleaned$entry_quarter <- paste0(year(df_1_cleaned$Baseline_date), " Q", quarter(df_1_cleaned$Baseline_date))

df_1_cleaned$ConsMechanism_Calc <- ifelse(df_1_cleaned$ConsMechanism_Calc == "Remote", 1, 0)

###### duplicates

df_1_cleaned <- df_1_cleaned %>%
  arrange(pseudo_nhs_number_ncdr, desc(Baseline_date)) %>%
  distinct(pseudo_nhs_number_ncdr, .keep_all = TRUE)



tabUnmatched <- CreateTableOne(vars = vars , strata = "type" , factorVars = factorvars, data = df_1_cleaned , test = FALSE )
#print(tabUnmatched , smd = TRUE)


#addmargins(table(ExtractSmd(tabUnmatched) > 0.1))





###Prpensity Score Estimation

ps_formula <- as.formula(paste("type ~", paste(vars, collapse = "+")))
psModel <- glm(ps_formula, family = binomial(link = "logit"), data = df_1_cleaned)


# summary(psModel)





# Perform propensity score matching
match_model <- matchit(ps_formula ,
                       
                       data = df_1_cleaned, 
                       method = "nearest",
                       ratio = 1,
                       caliper = 0.11,
                      #  caliper =1.5,
                       replace= FALSE
                      
)

# Get matched data
matched_data <- match.data(match_model)
model_summary <- summary(match_model)


## Construct a table
tabMatched <- CreateTableOne(vars = vars, strata = "type", factorVars = factorvars, data = matched_data, test = FALSE)
## Show table with SMD
#print(tabMatched, smd = TRUE)

###Assessing balance before and after matching /weigh


## Construct a data frame containing variable name and SMD from all methods
dataPlot <- data.frame(variable   = rownames(ExtractSmd(tabUnmatched)),
                       Unmatched  = as.numeric(ExtractSmd(tabUnmatched)),
                       Matched    = as.numeric(ExtractSmd(tabMatched))
)

## Create long-format data for ggplot2
dataPlotMelt <- reshape2::melt(data          = dataPlot,
                               id.vars       = c("variable"),
                               variable.name = "Method",
                               value.name    = "SMD")

## Order variable names by magnitude of SMD
varNames <- as.character(dataPlot$variable)[order(dataPlot$Unmatched)]

## Order factor levels in the same order
dataPlotMelt$variable <- factor(dataPlotMelt$variable,
                                levels = varNames)

## Plot using ggplot2
p<- ggplot(data = dataPlotMelt,
       mapping = aes(x = variable, y = SMD, group = Method, color = Method)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.1, color = "black", size = 0.1) +
  coord_flip() +
  theme_bw() +
  theme(legend.key = element_blank())

ggsave(paste0("outputs/plots/",varname,"_smd_plot.png"), plot = p, width = 8, height = 6, dpi = 300)


## Column bind tables
resCombo <- cbind(print(tabUnmatched,  printToggle = FALSE),
                  print(tabMatched,    printToggle = FALSE)
)

## Add group name row, and rewrite column names
resCombo <- rbind(Group = rep(c("Control","Intervention"), 2), resCombo)
colnames(resCombo) <- c("Unmatched","","Matched","")

print("1 finished")
# Save summary to text file
file_path <- file.path("outputs", paste0(varname, "_model_Summary.txt"))
sink(file_path)
print(model_summary)
sink()

print("2 finished")
file_path <- file.path("outputs", paste0(varname, "_rescombo.csv"))

sink(file_path)
print(resCombo)
sink()


print("3 finished")
saveRDS(matched_data, paste0(varname, "matched_data.RDS"))


return(matched_data)

}

