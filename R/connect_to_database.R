#' Connect to a database
#'
#' This function establishes a connection to a database using the specified parameters.
#'
#' @param dbms The type of DBMS running on the server
#' @param server The name of the server
#' @param user The user name used to access the server
#' @param password The password for that user
#' @param port The port number on the server
#' @param pathToDriver Path to the JDBC driver (optional)
#'
#' @return A list containing the connection details and connection object
#' @export
#' @import DatabaseConnector
#'
#' @examples
#' \dontrun{
#' connect_to_database(
#'   dbms = "postgresql",
#'   server = "localhost/db",
#'   user = "user",
#'   password = "password",
#'   port = 5432
#' )
#' }
connect_to_database <- function(dbms, server, user, password, port, pathToDriver) {
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms,
    server = server,
    user = user,
    password = password,
    port = port,
    pathToDriver = pathToDriver
  )
  
  conn <- DatabaseConnector::connect(connectionDetails)
  message("Database connection established.")
  query <- "SELECT schema_name FROM information_schema.schemata"
  schema_names <- DBI::dbGetQuery(conn, query)
  schema_names <- schema_names[["schema_name"]]
  
  
  return(list(connectionDetails = connectionDetails, conn = conn,schema_names=schema_names))
}


# conn_detail_temp <- connect_to_database(
#   dbms = "postgresql",
#   server = "atlas.inspirenetwork.net/postgres",
#   user = "postgres",
#   password = "mypass",
#   port = "5432",
#   pathToDriver = "C:/jdbcDrivers")
# 
# 
# # Get tables from a specific schema
# get_tables_from_schema_cohort <- function(conn, schema_name) {
#   # Query to get table names from the specified schema
#   query <- sprintf("SELECT table_name FROM information_schema.tables 
#                     WHERE table_schema = '%s' AND table_type = 'BASE TABLE'", schema_name)}
#   