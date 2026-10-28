#' Summarise Reliable Recovery Counts and Percentages for Binary Variables
#'
#' @description
#' Calculates counts and proportions of observations with value == 1 for
#' one or more binary variables, stratified by `Reliable_Recovery` status.
#'
#' For each variable supplied in `cols`, the function:
#' \itemize{
#'   \item Counts observations by `Reliable_Recovery` and variable value (0/1)
#'   \item Computes within-group percentages
#'   \item Extracts results for value == 1 only
#'   \item Reshapes output to one row per variable
#' }
#'
#' This function is typically used in descriptive or outcome reporting to
#' compare binary indicators across Reliable Recovery groups.
#'
#' @param df A data.frame or tibble containing:
#'   \itemize{
#'     \item A column named `Reliable_Recovery`
#'     \item One or more binary (0/1) variables listed in `cols`
#'   }
#'
#' @param cols A character vector of column names representing binary
#'   indicator variables to summarise.
#'
#' @return
#' A data.frame with one row per variable in `cols`. Columns include:
#' \itemize{
#'   \item `Variable` – variable name
#'   \item `Reliable_Recovery_{group}_count_1` – count of value == 1
#'   \item `Reliable_Recovery_{group}_percent_1` – proportion within group
#' }
#'
#' @details
#' Assumes:
#' \itemize{
#'   \item `Reliable_Recovery` is coded consistently across observations
#'   \item Variables in `cols` are binary (0/1)
#'   \item Missing values are preserved and excluded from percentage denominators
#' }
#'
#'
#' @export


get_ReliableRecoverycountPercent_fn <- function(df, cols) {
  count_percent_table <- lapply(cols, function(col) {
    # Count by Reliable_Recovery and value (0 or 1)
    counts <- df %>%
      group_by(Reliable_Recovery, value = .data[[col]]) %>%
      summarise(n = n(), .groups = "drop") %>%
      complete(Reliable_Recovery, value = c(0, 1), fill = list(n = NA)) %>%
      group_by(Reliable_Recovery) %>%
      mutate(percent = n / sum(n, na.rm = TRUE)) %>%
      ungroup()

    # Filter to keep only rows where value == 1
    counts_1 <- counts %>%
      filter(value == 1) %>%
      select(Reliable_Recovery, count_1 = n, percent_1 = percent)

    # Pivot to one row per variable
    result <- counts_1 %>%
      pivot_wider(
        names_from = Reliable_Recovery,
        values_from = c(count_1, percent_1),
        names_glue = "Reliable_Recovery_{Reliable_Recovery}_{.value}"
      ) %>%
      mutate(Variable = col) %>%
      select(Variable, everything())

    return(result)
  })

  final_df <- bind_rows(count_percent_table)
  return(final_df)
}
