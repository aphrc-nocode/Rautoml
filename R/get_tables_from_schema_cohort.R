#' Get Tables from a Specific Schema
#'
#' This function queries the database for all base table names in a given schema.
#'
#' @param conn A database connection object created by `DatabaseConnector::connect()` or `DBI::dbConnect()`.
#' @param schema_name A character string specifying the schema name.
#'
#' @return A data frame of table names in the specified schema.
#' @export
#'
#' @examples
#' \dontrun{
#'   conn <- DatabaseConnector::connect(connectionDetails)
#'   tables <- get_tables_from_schema_cohort(conn, schema_name)
#' }
get_tables_from_schema_cohort <- function(conn, schema_name) {
  query <- sprintf("SELECT table_name FROM information_schema.tables 
                    WHERE table_schema = '%s' AND table_type = 'BASE TABLE'", schema_name)
  tables <- DBI::dbGetQuery(conn, query)
  return(tables)
}
