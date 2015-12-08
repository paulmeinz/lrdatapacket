
# Function for recoding academic year (will be modified to be more self-sustaining in the future)

recode_acad_year <- function(strm) {
  newyear <- c()
  newyear <- car::recode(strm,"
                         c(1086, 1089, 1093) = '2008-2009';
                         c(1096, 1099, 1103) = '2009-2010';
                         c(1106, 1109, 1113) = '2010-2011';
                         c(1116, 1119, 1123) = '2011-2012';
                         c(1126, 1129, 1133) = '2012-2013';
                         c(1136, 1139, 1143) = '2013-2014';
                         c(1146, 1149, 1153) = '2014-2015';
                         else = NA")

  if (TRUE %in% is.na(newyear)) {
    warning('Term to year conversion detected idiosyncratic input. Ensure that
            demographic names were passed in the correct order and/or that the
            query pulled currently supported data.')
  }

  newyear <- as.factor(newyear)
  newyear
}


# Function for creating a collapsed and expanded age group factor
# Returns a list of factor vectors (expanded age, collapsed age)

recode_age <- function(term_age) {

  if (!is.numeric(term_age) || TRUE %in% term_age > 100) {
    warning('Age conversion detected idiosyncratic input. Ensure that demographic
            names were passed in the correct order and/or that the query pulled
            currently supported data.')
  }

  newage <- c()
  newage[term_age < 21] = '20 or under'
  newage[term_age > 20 & term_age < 25] = '21-24'
  newage[term_age > 24 & term_age < 30] = '25-29'
  newage[term_age > 29 & term_age < 40] = '30-39'
  newage[term_age > 39] = '40 or over'
  newage2 <- c()
  newage2[term_age < 25] = 'Under 25'
  newage2[term_age > 24] = '25 or over'
  newage <- factor(newage)
  newage2 <- factor(newage2, levels = c('Under 25', '25 or over'),
                    ordered = TRUE)
  ages <- list(newage, newage2)
  ages
}


# Function for turning letter grades into numeric 100/0 success values

calculate_success <- function(grades) {
  newgrade <- rep(0, times = length(grades))
  newgrade[grades == 'A'
           | grades == 'B'
           | grades == 'C'
           | grades == 'CR'
           | grades == 'P'] <- 100
  newgrade
}


# Function for recoding course numbers in to course level variable (basic skills,
# college level, transfer level)

recode_course_level <- function(crse_num) {
  newLevel <- c(1:length(crse_num))
  newLevel[crse_num < 100] <- 'Basic Skills'
  newLevel[crse_num > 99 & crse_num < 300] <- 'College-Level*'
  newLevel[crse_num > 299] <- 'Transfer*'
  newLevel <- as.factor(newLevel)
  newLevel
}

# Function to recode student educational goal into a simpler factor

recode_ed_goal <- function(matr_goal) {
  newgoal <- as.character(c(1:length(matr_goal)))
  newgoal[matr_goal == 'Not Applicable'] <- 'Undecided/Unknown'
  newgoal[matr_goal == 'Uncollected/Unreported'] <- 'Undecided/Unknown'
  newgoal[matr_goal == 'Undecided on goal'] <- 'Undecided/Unknown'
  newgoal[matr_goal == 'Undecided on Goal'] <- 'Undecided/Unknown'
  newgoal[matr_goal == 'Transfer to 4-Year after AA/AS'] <- 'Transfer'
  newgoal[matr_goal == 'Transfer to 4-Year w/o AA/AS'] <- 'Transfer'
  newgoal[matr_goal == 'Transfer to 4-Year- no AA/AS'] <- 'Transfer'
  newgoal[matr_goal == 'Acquire Job Skills Only'] <- 'Job Skills Development'
  newgoal[matr_goal == 'Acquire New Job Skills, Only'] <- 'Job Skills Development'
  newgoal[matr_goal == 'Maintain Certificate/License'] <- 'Job Skills Development'
  newgoal[matr_goal == 'Update Job Skills, Only'] <- 'Job Skills Development'
  newgoal[matr_goal == 'Update Job Skills Only'] <- 'Job Skills Development'
  newgoal[matr_goal == 'Upgrade Job Skills Only'] <- 'Job Skills Development'
  newgoal[matr_goal == 'Earn A  Vocational Certificate'] <- 'Degree/Certificate Obtainment'
  newgoal[matr_goal == 'Earn a Certificate' ] <- 'Degree/Certificate Obtainment'
  newgoal[matr_goal == 'Earn A Voc Degree w/o Transfer'] <- 'Degree/Certificate Obtainment'
  newgoal[matr_goal == 'Earn a Voc Degree- no Transfer'] <- 'Degree/Certificate Obtainment'
  newgoal[matr_goal == 'Earn AA/AS Degree w/o Transfer'] <- 'Degree/Certificate Obtainment'
  newgoal[matr_goal == 'Earn AA/AS Degree- no Transfer'] <- 'Degree/Certificate Obtainment'
  newgoal[substr(matr_goal,1,7) == 'Four-yr'] <- 'Univ. Student taking CC classes'
  newgoal[matr_goal == '`Four-yr student mtg 4-yr reqs'] <- 'Univ. Student taking CC classes'
  newgoal[matr_goal == 'Educational Development'] <- 'Personal/Basic Skills Development'
  newgoal[matr_goal == 'Improve Basic Skills'] <- 'Personal/Basic Skills Development'
  newgoal[matr_goal == 'Complete High School/GED'] <- 'Personal/Basic Skills Development'
  newgoal[matr_goal == 'Discover Career Interests'] <- 'Personal/Basic Skills Development'
  newgoal[matr_goal == 'Move from noncred to credit'] <- 'Personal/Basic Skills Development'
  newgoal <- as.factor(newgoal)
  newgoal
}


# Function to recode previously attained educational level of a student

recode_ed_level <- function(ed_level) {
  newed <- as.character(c(1:length(ed_level)))
  newed[ed_level == 'Not Applicable'] <- 'Non-traditional HS Proficiency'
  newed[ed_level == 'Received CA HS Proficiency'] <- 'Non-traditional HS Proficiency'
  newed[ed_level == 'GED/Cert. of Equiv/Completn'] <- 'Non-traditional HS Proficiency'
  newed[ed_level == 'Adult School'] <- 'Below HS-level Education'
  newed[ed_level == 'Have not received HS diploma'] <- 'Below HS-level Education'
  newed[ed_level == 'Foreign Secondary Schl Dipl'] <- 'High School Diploma'
  newed[ed_level == 'Received HS Diploma'] <- 'High School Diploma'
  newed[ed_level == 'Received Associate Degree'] <- 'AA Degree or higher'
  newed[ed_level == 'Bachelor Degree or higher'] <- 'AA Degree or higher'
  newed[ed_level == 'Special Admit/Advanced Ed'] <- 'Special Admit'
  newed[ed_level == 'Unknown'] <- 'Unknown'
  newed[ed_level == 'UNKNOWN'] <- 'Unknown'
  newed[ed_level == 'Uncollected/Unreported'] <- 'Unknown'
  newed[is.na(ed_level)] <- 'Unknown'
  newed <- as.factor(newed)
  newed
}


# Function for recoding collapsing ethnic groups

recode_ethnic_group <- function(ethnicity) {
  neweth <- car::recode(ethnicity, "'Asian' = 'Asian/Pac. Isl.';
                   'Filipino' = 'Asian/Pac. Isl.';
                   'Pacific Islander' = 'Asian/Pac. Isl.';
                   'African American' = 'African American';
                   'Hispanic/Latino' = 'Hispanic/Latino';
                   'Native American' = 'Native American';
                   'White' = 'White';
                    else = 'Other/Mixed/Unknown'
                   ")
  neweth <- as.factor(neweth)
  neweth
}


# Function for recoding freshman status

recode_freshman_status <- function(status) {
  newstatus <- as.character(c(1:length(status)))
  newstatus[status == 'First Time Student (New)'] <- 'First Time Freshman'
  newstatus[status != 'First Time Student (New)'] <- 'Other Student'
  newstatus <- as.factor(newstatus)
  newstatus
}


# Function for recoding instructional mode

recode_inst_mode <- function(mode) {
  newInst <- car::recode(mode, "
                      c(2,4) = 'Lecture/Lab';
                      c(71,72) = 'Internet';
                      c(20,40,50,51,52,63,90,98) = 'Other';
                      11 = NA")
  newInst <- as.factor(newInst)
  newInst
}


# Function for recoding language status

recode_lang_status <- function(lang) {
  newlang <- as.character(c(1:length(lang)))
  newlang[lang == 'English'] <- 'English Primary Language'
  newlang[lang != 'English'] <- 'English NOT Primary Language'
  newlang <- as.factor(newlang)
  newlang
}


# Function for recoding gender into full names

recode_gender <- function(gender) {
  newgender <- c(1:length(gender))
  newgender[gender == 'F'] <- 'Female'
  newgender[gender == 'M'] <- 'Male'
  newgender[gender == 'U'] <- 'Unknown'

  newgender <- as.factor(newgender)
  newgender
}


