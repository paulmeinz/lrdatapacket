#'
#'
#'
#'
#'
#'
#'
#'
#'
#'

query_data <- function(connect_obj, query) {

  # Make query lowercase
  query <- tolower(query)

  # Connect to the db and query
  conn <- RODBC::odbcConnect(connect_obj)
  data <- RODBC::sqlQuery(conn, query)


}
