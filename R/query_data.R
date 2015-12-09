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

query_data <- function(connect_obj, query,
                       demo_names = c('id','term','age','course_number',
                                      'matr_goal','ed_level','ethnicity',
                                      'enroll_status','inst_mode','language',
                                      'gender', 'grade')) {

  # Make query lowercase
  query <- tolower(query)

  # Connect to the db and query
  conn <- RODBC::odbcConnect(connect_obj)
  data <- RODBC::sqlQuery(conn, query)
  close(conn)

  # Assign column names in order
  for (i in 1:length(names(data))) {
    names(data)[i] <- demo_names[i]
  }

  lrdata <- data.frame(id = data[,'id'], term = data[,'term'])

  # Process data based on column names
  for (i in names(data)) {

    # Recode age and add it to the lrdata frame
    if (i == 'age') {lrdata <- data.frame(lrdata, recode_age(data[,i]))}

    # Recode ethnicity ...
    if (i == 'ethnicity') {lrdata <- data.frame(lrdata,
                                                ethnicity =
                                                recode_ethnic_group(data[,i]))}

    # Recode term into year
    if (i == 'term') {lrdata <- data.frame(lrdata,
                                           acad_year =
                                           recode_acad_year(data[,i]))}

    # Recode grade into success
    if (i == 'grade') {lrdata <- data.frame(lrdata,
                                            success =
                                            calculate_success(data[,i]))}


  }

  lrdata
}
