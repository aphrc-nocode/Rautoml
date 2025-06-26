#' Create a cohort based on concept codes
#'
#' This function creates a cohort from specified concept codes and returns
#' information about the cohort settings, counts, and attrition.
#'
#' @param cdm A CDM reference object
#' @param keywords A vector of keywords to identify concept codes
#' @param cohortname Name for the new cohort
#' @param event_start_date The exit date for the cohort
#'
#' @return A list containing:
#' \itemize{
#'   \item set_df - Cohort settings data frame
#'   \item cc_df - Cohort counts data frame
#'   \item attr_df - Attrition data frame
#'   \item tc - Total counts by cohort definition
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' result <- cohortcreation(cdm, c("asthma", "copd"), "respiratory_cohort", "2020-01-01")
#' }
cohortcreation <- function(cdm, keywords, cohortname, event_start_date) {
  
  codes_list <- list()
  for (kw in keywords) {
    codes_list[[paste0(kw, "_codes")]] <- getCandidateCodes(cdm, kw)
  }
  
  codes_list_id <- list()
  
  for (kw in names(codes_list)) {
    codes_list_id[[stringr::str_sub(kw, start = stringr::str_length(kw)-5, end = stringr::str_length(kw))]] <-
      codes_list[[kw]]$concept_id
  }
  
  study_codes <- newCodelist(codes_list_id)
  
  cdm[[cohortname]] <- cdm |>
    conceptCohort(
      conceptSet = study_codes,
      exit = event_start_date,
      name = cohortname
    )
  
  set_df <- settings(cdm[[cohortname]])
  cc_df <- cohortCount(cdm[[cohortname]])
  attr_df <- attrition(cdm[[cohortname]])
  
  tc <- cdm[[cohortname]] %>%
    dplyr::group_by(cohort_definition_id) %>%
    dplyr::tally() %>%
    dplyr::collect()
  
  return(list(
    set_df = set_df,
    cc_df = cc_df,
    attr_df = attr_df,
    tc = tc
  ))
}
