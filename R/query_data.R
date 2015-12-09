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
                       demo_names = c('id','term','subject','subject_long',
                                      'course_number','age','matr_goal',
                                      'ed_level','ethnicity','enroll_status',
                                      'inst_mode','language','gender',
                                      'grade')) {

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
    if (i == 'ethnicity') {
      lrdata <- data.frame(lrdata, ethnicity = recode_ethnic_group(data[,i]))}

    # Recode term into year
    if (i == 'term') {
      lrdata <- data.frame(lrdata, acad_year = recode_acad_year(data[,i]))
    }

    # Recode grade into success
    if (i == 'grade') {
      lrdata <- data.frame(lrdata, success = calculate_success(data[,i]))
    }

    # Recode course number into course level
    if (i == 'course_number') {
      lrdata <- data.frame(lrdata, course_number = recode_course_level(data[,i]))
    }

    # Recode matriculation goal
    if (i == 'matr_goal') {
      lrdata <- data.frame(lrdata, matr_goal = recode_ed_goal(data[,i]))
    }

    # Recode educational level
    if (i == 'ed_level') {
      lrdata <- data.frame(lrdata, ed_level = recode_ed_level(data[,i]))
    }

    # Recode freshman status
    if (i == 'enroll_status') {
      lrdata <- data.frame(lrdata, freshman_status =
                           recode_freshman_status(data[,i]))
    }

    # Recode inst mode
    if (i == 'inst_mode') {
      lrdata <- data.frame(lrdata, inst_mode = recode_inst_mode(data[,i]))
    }

    # Recode language status
    if (i == 'language') {
      lrdata <- data.frame(lrdata, language = recode_lang_status(data[,i]))
    }

    # Recode gender
    if (i == 'gender') {
      lrdata <- data.frame(lrdata, gender = recode_gender(data[,i]))
    }
  }

  lrdata
}
