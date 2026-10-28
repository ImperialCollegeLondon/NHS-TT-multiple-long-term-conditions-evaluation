#' Count and Percentage Table by Group
#'
#' @description
#' Calculates counts and within-group proportions for binary variables,
#' stratified by a specified grouping variable.
#'
#' @param df Data.frame containing grouping and binary variables.
#' @param group_column Name of grouping variable (character).
#' @param cols Character vector of binary variable names.
#'
#' @return
#' Data.frame summarising count and proportion (value == 1) per group.
#'
#' @details
#' Designed for descriptive summaries in cohort comparisons.
#'
#' @export

countPercent_fn <- function(df, group_column, cols) {
  count_percent_table <- lapply(cols, function(col) {
    # Dynamically refer to the group column
    group_sym <- rlang::sym(group_column)
    
    counts <- df %>%
      group_by(group = .data[[group_column]], value = .data[[col]]) %>%
      summarise(n = n(), .groups = "drop") %>%
      complete(group, value = c(0, 1), fill = list(n = NA)) %>%
      group_by(group) %>%
      mutate(percent = n / sum(n, na.rm = TRUE)) %>%
      ungroup()
    
    # Keep only value == 1
    counts_1 <- counts %>%
      filter(value == 1) %>%
      dplyr::select(group, count_1 = n, percent_1 = percent)
    
    # Pivot wider using group values (e.g., typeA, typeB) into column names
    result <- counts_1 %>%
      pivot_wider(
        names_from = group,
        values_from = c(count_1, percent_1),
        names_glue = paste0(group_column, "{group}_{.value}")
      ) %>%
      mutate(Variable = col) %>%
      dplyr::select(Variable, everything())
    
    return(result)
  })
  
  final_df <- bind_rows(count_percent_table)
  return(final_df)
}
