#' Primary Analysis: Physical Long-Term Conditions (PLD)
#'
#' @description
#' Conducts the primary analysis of physical long-term conditions (PLD)
#' at baseline, 1-year, and 5-year follow-up within the matched cohort.
#'
#' The function:
#' \itemize{
#'   \item Cleans and filters condition indicators
#'   \item Constructs LTC count variables
#'   \item Generates ≥N LTC binary outcomes (1–24)
#'   \item Runs adjusted and unadjusted Poisson models
#'   \item Computes count and percentage summaries
#' }
#'
#' @param matched_data Matched cohort dataset containing LTC indicators
#'   and covariates.
#'
#' @return
#' A named list containing:
#' \itemize{
#'   \item Adjusted IRRs for ≥N LTCs
#'   \item Unadjusted IRRs
#'   \item Count and percentage summaries
#'   \item Individual LTC model results
#' }
#'
#' @details
#' Robust (sandwich) standard errors are used for all Poisson models.
#' Designed for secondary PLD outcome analysis.
#'
#' @export