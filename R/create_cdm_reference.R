#' Create a CDM reference
#'
#' This function creates a CDM reference using CDMConnector.
#'
#' @param con A database connection object
#' @param cdmName The name of the CDM
#' @param cdmSchema The schema containing the CDM tables
#' @param writeSchema The schema where results can be written
#' @param achillesSchema Optional schema for Achilles results
#'
#' @return A CDM reference object
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- create_cdm_reference(con, "my_cdm", "cdm_schema", "results_schema")
#' }
create_cdm_reference <- function(con, cdmName, cdmSchema, writeSchema, achillesSchema = NULL) {
  cdm <- CDMConnector::cdmFromCon(
    con = con,
    cdmName = cdmName,
    cdmSchema = cdmSchema,
    writeSchema = writeSchema,
    achillesSchema = achillesSchema
  )
  message("CDM reference created.")
  return(cdm)
}
