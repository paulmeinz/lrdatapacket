
# Function for recoding academic year (will be modified to be more self-sustaining in the future)

recode_acad_year <- function(data){
  newyear <- car::recode(data$STRM,"
                         c(1086, 1089, 1093) = '2008-2009';
                         c(1096, 1099, 1103) = '2009-2010';
                         c(1106, 1109, 1113) = '2010-2011';
                         c(1116, 1119, 1123) = '2011-2012';
                         c(1126, 1129, 1133) = '2012-2013';
                         c(1136, 1139, 1143) = '2013-2014';
                         c(1146, 1149, 1153) = '2014-2015'")
  newyear <- as.factor(newyear)
  newyear
}


# Function for creating a collapsed and expanded age group factor
# Returns a list of factor vectors (expanded age, collapsed age)

recode_age <- function(data) {
  newage <- data$TERM_AGE
  newage[data$TERM_AGE < 21] = '20 or under'
  newage[data$TERM_AGE > 20 & data$TERM_AGE < 25] = '21-24'
  newage[data$TERM_AGE > 24 & data$TERM_AGE < 30] = '25-29'
  newage[data$TERM_AGE > 29 & data$TERM_AGE < 40] = '30-39'
  newage[data$TERM_AGE > 39] = '40 or over'

  newage2 <- data$TERM_AGE
  newage2[data$TERM_AGE < 25] = 'Under 25'
  newage2[data$TERM_AGE > 24] = '25 or over'

  newage <- factor(newage)
  newage2 <- factor(newage2, levels = c('Under 25', '25 or over'), ordered = TRUE)

  ages <- list(newage, newage2)

  ages
}


# Function for turning letter grades into numeric 100/0 success values

calculate_success <- function(data) {
  newgrade <- rep(0, times = length(data$OFFICIAL_GRADE))
  newgrade[data$OFFICIAL_GRADE == 'A'
           | data$OFFICIAL_GRADE == 'B'
           | data$OFFICIAL_GRADE == 'C'
           | data$OFFICIAL_GRADE == 'CR'
           | data$OFFICIAL_GRADE == 'P'] <- 100

  newgrade
}


#
