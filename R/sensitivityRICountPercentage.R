
#' Count and Percentage by Reliable Improvement
#'
#' @description
#' Computes counts and proportions of binary variables stratified by
#' Reliable Improvement status.
#'
#' @param df Data.frame containing `Reliable_Improvement` column.
#' @param cols Character vector of binary variable names.
#'
#' @return
#' Data.frame with one row per variable and count/percentage columns
#' for each Reliable Improvement group.
#'
#' @details
#' Percentages are calculated within Reliable Improvement strata.
#'
#' @export
library(dplyr)
library(tidyr)
library(rlang)

get_ReliableImprovementcountPercent_fn <- function(df, cols) {
  count_percent_table <- lapply(cols, function(col) {
    # Count by Reliable_Improvement and value (0 or 1)
    counts <- df %>%
      group_by(Reliable_Improvement, value = .data[[col]]) %>%
      summarise(n = n(), .groups = "drop") %>%
      complete(Reliable_Improvement, value = c(0, 1), fill = list(n = NA)) %>%
      group_by(Reliable_Improvement) %>%
      mutate(percent = n / sum(n, na.rm = TRUE)) %>%
      ungroup()
    
    # Filter to keep only rows where value == 1
    counts_1 <- counts %>%
      filter(value == 1) %>%
      select(Reliable_Improvement, count_1 = n, percent_1 = percent)
    
    # Pivot to one row per variable
    result <- counts_1 %>%
      pivot_wider(
        names_from = Reliable_Improvement,
        values_from = c(count_1, percent_1),
        names_glue = "Reliable_Improvement{Reliable_Improvement}_{.value}"
      ) %>%
      mutate(Variable = col) %>%
      select(Variable, everything())
    
    return(result)
  })
  
  final_df <- bind_rows(count_percent_table)
  return(final_df)
}


