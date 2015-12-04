
# Function for recoding academic year (will be modified to be more self-sustaining in the future)

recode_acad_year <- function(strm){
  newyear <- c()
  newyear <- car::recode(strm,"
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

recode_age <- function(term_age) {
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
  newage2 <- factor(newage2, levels = c('Under 25', '25 or over'), ordered = TRUE)
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

recode_course_level <- function(crse_num){
  newLevel <- c(1:length(crse_num))
  newLevel[crse_num < 100] <- 'Basic Skills'
  newLevel[crse_num > 99 & crse_num < 300] <- 'College-Level*'
  newLevel[crse_num > 299] <- 'Transfer*'
  newLevel <- as.factor(newLevel)
  newLevel
}

# Function to recode student educational goal into a simpler factor

recode_ed_goal <- function(data) {
  newgoal <- as.character(c(1:length(data$MATR_GOAL_1_DESCR)))
  newgoal[data$MATR_GOAL_1_DESCR == 'Not Applicable'] <- 'Undecided/Unknown'
  newgoal[data$MATR_GOAL_1_DESCR == 'Uncollected/Unreported'] <- 'Undecided/Unknown'
  newgoal[data$MATR_GOAL_1_DESCR == 'Undecided on goal'] <- 'Undecided/Unknown'
  newgoal[data$MATR_GOAL_1_DESCR == 'Undecided on Goal'] <- 'Undecided/Unknown'
  newgoal[data$MATR_GOAL_1_DESCR == 'Transfer to 4-Year after AA/AS'] <- 'Transfer'
  newgoal[data$MATR_GOAL_1_DESCR == 'Transfer to 4-Year w/o AA/AS'] <- 'Transfer'
  newgoal[data$MATR_GOAL_1_DESCR == 'Transfer to 4-Year- no AA/AS'] <- 'Transfer'
  newgoal[data$MATR_GOAL_1_DESCR == 'Acquire Job Skills Only'] <- 'Job Skills Development'
  newgoal[data$MATR_GOAL_1_DESCR == 'Acquire New Job Skills, Only'] <- 'Job Skills Development'
  newgoal[data$MATR_GOAL_1_DESCR == 'Maintain Certificate/License'] <- 'Job Skills Development'
  newgoal[data$MATR_GOAL_1_DESCR == 'Update Job Skills, Only'] <- 'Job Skills Development'
  newgoal[data$MATR_GOAL_1_DESCR == 'Update Job Skills Only'] <- 'Job Skills Development'
  newgoal[data$MATR_GOAL_1_DESCR == 'Upgrade Job Skills Only'] <- 'Job Skills Development'
  newgoal[data$MATR_GOAL_1_DESCR == 'Earn A  Vocational Certificate'] <- 'Degree/Certificate Obtainment'
  newgoal[data$MATR_GOAL_1_DESCR == 'Earn a Certificate' ] <- 'Degree/Certificate Obtainment'
  newgoal[data$MATR_GOAL_1_DESCR == 'Earn A Voc Degree w/o Transfer'] <- 'Degree/Certificate Obtainment'
  newgoal[data$MATR_GOAL_1_DESCR == 'Earn a Voc Degree- no Transfer'] <- 'Degree/Certificate Obtainment'
  newgoal[data$MATR_GOAL_1_DESCR == 'Earn AA/AS Degree w/o Transfer'] <- 'Degree/Certificate Obtainment'
  newgoal[data$MATR_GOAL_1_DESCR == 'Earn AA/AS Degree- no Transfer'] <- 'Degree/Certificate Obtainment'
  newgoal[substr(data$MATR_GOAL_1_DESCR,1,7) == 'Four-yr'] <- 'Univ. Student taking CC classes'
  newgoal[data$MATR_GOAL_1_DESCR == '`Four-yr student mtg 4-yr reqs'] <- 'Univ. Student taking CC classes'
  newgoal[data$MATR_GOAL_1_DESCR == 'Educational Development'] <- 'Personal/Basic Skills Development'
  newgoal[data$MATR_GOAL_1_DESCR == 'Improve Basic Skills'] <- 'Personal/Basic Skills Development'
  newgoal[data$MATR_GOAL_1_DESCR == 'Complete High School/GED'] <- 'Personal/Basic Skills Development'
  newgoal[data$MATR_GOAL_1_DESCR == 'Discover Career Interests'] <- 'Personal/Basic Skills Development'
  newgoal[data$MATR_GOAL_1_DESCR == 'Move from noncred to credit'] <- 'Personal/Basic Skills Development'
  newgoal <- as.factor(newgoal)
  newgoal
}


# Function to recode previously attained educational level of a student

recode_ed_level <- function(data) {
  newed <- as.character(c(1:length(data$ED_LEVEL_DESCR)))
  newed[data$ED_LEVEL_DESCR == 'Not Applicable'] <- 'Non-traditional HS Proficiency'
  newed[data$ED_LEVEL_DESCR == 'Received CA HS Proficiency'] <- 'Non-traditional HS Proficiency'
  newed[data$ED_LEVEL_DESCR == 'GED/Cert. of Equiv/Completn'] <- 'Non-traditional HS Proficiency'
  newed[data$ED_LEVEL_DESCR == 'Adult School'] <- 'Below HS-level Education'
  newed[data$ED_LEVEL_DESCR == 'Have not received HS diploma'] <- 'Below HS-level Education'
  newed[data$ED_LEVEL_DESCR == 'Foreign Secondary Schl Dipl'] <- 'High School Diploma'
  newed[data$ED_LEVEL_DESCR == 'Received HS Diploma'] <- 'High School Diploma'
  newed[data$ED_LEVEL_DESCR == 'Received Associate Degree'] <- 'AA Degree or higher'
  newed[data$ED_LEVEL_DESCR == 'Bachelor Degree or higher'] <- 'AA Degree or higher'
  newed[data$ED_LEVEL_DESCR == 'Special Admit/Advanced Ed'] <- 'Special Admit'
  newed[data$ED_LEVEL_DESCR == 'Unknown'] <- 'Unknown'
  newed[data$ED_LEVEL_DESCR == 'UNKNOWN'] <- 'Unknown'
  newed[data$ED_LEVEL_DESCR == 'Uncollected/Unreported'] <- 'Unknown'
  newed[is.na(data$ED_LEVEL_DESCR)] <- 'Unknown'
  newed <- as.factor(newed)
  newed
}


# Function for recoding collapsing ethnic groups

recode_ethnic_group <- function(data) {
  neweth <- recode(data$RACE, "'Asian' = 'Asian/Pac. Isl.';
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

recode_freshman_status <- function(data){
  newstatus <- as.character(c(1:length(data$ENRL_STATUS_DESCR)))
  newstatus[data$ENRL_STATUS_DESCR == 'First Time Student (New)'] <- 'First Time Freshman'
  newstatus[data$ENRL_STATUS_DESCR != 'First Time Student (New)'] <- 'Other Student'
  newstatus <- as.factor(newstatus)
  newstatus
}


# Function for recoding instructional mode

recode_inst_mode <- function(data){
  newInst <- recode(data$INSTRUCTION_MODE, "
                      c(2,4) = 'Lecture/Lab';
                      c(71,72) = 'Internet';
                      c(20,40,50,51,52,63,90,98) = 'Other';
                      11 = NA")
  newInst <- as.factor(newInst)
  newInst
}


# Function for recoding language status

recode_lang_status <- function(data) {
  newlang <- as.character(c(1:length(data$PRIMARY_LANG)))
  newlang[data$PRIMARY_LANG == 'English'] <- 'English Primary Language'
  newlang[data$PRIMARY_LANG != 'English'] <- 'English NOT Primary Language'
  newlang <- as.factor(newlang)
  newlang
}


# Function for recoding gender into full names

gender <- function(data) {
  newgender <- c(1:length(data$GENDER))
  newgender[data$GENDER == 'F'] <- 'Female'
  newgender[data$GENDER == 'M'] <- 'Male'
  newgender[data$GENDER == 'U'] <- 'Unknown'

  newgender <- as.factor(newgender)
  newgender
}


