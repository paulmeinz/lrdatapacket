#' Query data for standard data packet
#'
#' A function for querying demographic data for standard college
#' data packets. Returns an object with class data.frame and lrdataset
#'
#' @param dsn A data source name for the archive side of the LRDB. See the
#' readme on how to set up a data source name.
#' @param query A sql query in the form of a character vector or length one
#' the query must pull student id, term (e.g, 1139), term description,
#' subject (e.g., 'PSYC'),long subject ('Psychology'), and official grade.
#' @param demo_names A character vector of column names corresponding with
#' the order of columns in your sql query. The column names must be from
#' a standard list - each column name corresponding with currently supported
#' data (see details) and must contain 'id','term','subject','subject_long',
#' 'grade', and 'term_desc'
#'
#' @details
#' In order for this function to yield the necessary data, the user
#' must pass a sql query (param query) for the archive LRDB and a vector of
#' column names (param demo_names) corresponding with the columns of the query.
#' For example, if the first column of the query selected student id, then the
#' first column name provided would be 'id'. The function will only support
#' certain types of query data (e.g., student id, age, etc.), and each of these
#' data types has a standard column name that must be supplied in the same order
#' as the columns in the query. Each column name is listed below with associated
#' query data.
#'
#' 'id' - student id
#'
#' 'term' - 4-digit term identifier
#'
#' 'subject' - 2-5 letter course prefix (e.g., 'PSYC')
#'
#' 'long_subject' - full subject description (e.g, 'Psychology')
#'
#' 'course_number' - course number, up to 3 digits (e.g., 300)
#'
#' 'age' - student age during a given term
#'
#' 'matr_goal' - matriculation goal description (e.g.'Transfer to a 4-year')
#'
#' 'ed_level' - education level description (e.g., 'Completed GED')
#'
#' 'ethnicity' - student race
#'
#' 'inst_mode' - instructional mode description (e.g., 'lecture')
#'
#' 'language' - student's primary language
#'
#' 'gender' - student gender (m,f,u)
#'
#' 'grade' - official course grade (e.g., 'A','B','C', etc.)
#'
#'
#' Do not modify or recode the data in any way (e.g., with a case when). The
#' column names within the LRDB have not been provided here to keep the
#' structure of the database secure. A standard query can be provided upon
#' request. Contact \email{meinzp@@crc.losrios.edu}.
#'
#' @examples
#' \dontrun{
#' query_data(dsn = 'datasourcename',
#'            query = 'select id, term, subject, subject_descr...',
#'            demo_names = c('id','term','subject','subject_long'))
#'            }

query_data <- function(dsn, query,
                       demo_names = c('id', 'term', 'term_desc','subject',
                                      'subject_long', 'course_number', 'age',
                                      'matr_goal', 'ed_level', 'ethnicity',
                                      'enroll_status', 'inst_mode', 'language',
                                      'gender', 'grade')) {

  # Input validation
  supported <- c('id','term', 'subject', 'subject_long', 'course_number', 'age',
                 'matr_goal', 'ed_level','ethnicity', 'enroll_status',
                 'inst_mode', 'language', 'gender', 'grade', 'term_desc')
  required <- c('id','term','subject','subject_long', 'grade', 'term_desc')
  test <- required %in% demo_names

  # Note didn't use stopifnot because I wanted a custom error message
  if (FALSE %in% test) {
    stop(paste("demo_names must contain", required[!test], ";", sep = ' '))
  }

  if (FALSE %in% (demo_names %in% supported)) {
    stop("non-standard column name provided")
  }

  # Make query lowercase
  query <- tolower(query)

  # Connect to the db and query
  conn <- RODBC::odbcConnect(dsn)
  data <- RODBC::sqlQuery(conn, query)
  close(conn)

  # Print any sql errors
  if (length(grep('ERROR', data)) > 0) {
    print(data)
    stop('There was an error in your query :(')
  }

  # Assign column names in order
  for (i in 1:length(names(data))) {
    names(data)[i] <- demo_names[i]
  }

  # Additional input validation
  if (length(names(data)) != length(demo_names)){
    stop("Columns in dataset are not the same length as names provided")
  }

  lrdata <- data.frame(id = data[,'id'], term = data[,'term'],
                       term_desc = data[,'term_desc'])

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

  # Make the data have a lrdataset class for later validation
  class(lrdata) <- append(class(lrdata), "lrdataset")

  lrdata
}
