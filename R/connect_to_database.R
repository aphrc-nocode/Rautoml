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
  connectionDetails <- createConnectionDetails(
    dbms = dbms,
    server = server,
    user = user,
    password = password,
    port = port,
    pathToDriver = pathToDriver
  )
  
  conn <- connect(connectionDetails)
  message("Database connection established.")
  return(list(connectionDetails = connectionDetails, conn = conn))
}

