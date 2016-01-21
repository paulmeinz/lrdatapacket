

generate_packets <- function(data,
                             path,
                             use_subject = 'TRUE',
                             program = '',
                             program_short = '') {

  subjects <- unique(data$subject_long)

  if (use_subject) {
    for (i in subjects) {

      plot_data <- data[data$subject_long == i,]
      subject <- plot_data[1, 'subject']

      # Headcount Plots (duplicated and unduplicated)
      title <- paste(i,':','\n', 'Duplicated Headcount by Academic Term',
                     sep = '')
      save <- paste(path, subject,'.jpg', sep = '')
      plot_headcounts(plot_data, save, title, undup = FALSE)
      title <- paste(i,':','\n', 'Unuplicated Headcount by Academic Term',
                     sep = '')
      save <- paste(path, subject, '1.jpg', sep = '')
      plot_headcounts(plot_data, save, title)

      # Disaggregated Headcount Plots....Age Range
      title <- paste(i,':','\n', 'Headcount by Age Range', sep = '')
      save <- paste(path, subject, '2.jpg', sep = '')
      disag_hc_plot(plot_data, 'age', save, 'Age Range', title)

      # Collapsed Age Range
      title <- paste(i,':','\n', 'Headcount by Age Range (Collapsed)', sep = '')
      save <- paste(path, subject, '3.jpg', sep = '')
      disag_hc_plot(plot_data, 'agecol', save, 'Age Range', title)

      # Gender
      title <- paste(i,':','\n', 'Headcount by Gender', sep = '')
      save <- paste(path, subject, '4.jpg', sep = '')
      disag_hc_plot(plot_data, 'gender', save, 'Gender', title)

      # Ethnicity
      title <- paste(i,':','\n', 'Headcount by Ethnicity', sep = '')
      save <- paste(path, subject, '5.jpg', sep = '')
      disag_hc_plot(plot_data, 'ethnicity', save, 'Ethnicity', title)

      # Educational Goal
      title <- paste(i,':','\n', 'Headcount by Ed Goal', sep = '')
      save <- paste(path, subject, '6.jpg', sep = '')
      disag_hc_plot(plot_data, 'matr_goal', save, 'Educational Goal', title)

      # Educational Level
      title <- paste(i,':','\n', 'Headcount by Ed Level', sep = '')
      save <- paste(path, subject, '7.jpg', sep = '')
      disag_hc_plot(plot_data, 'matr_goal', save, 'Educational Level', title)

      # Instructional Mode
      title <- paste(i,':','\n', 'Headcount by Instructional Mode', sep = '')
      save <- paste(path, subject, '8.jpg', sep = '')
      disag_hc_plot(plot_data, 'inst_mode', save, 'Instructional Mode', title)

      # Course Level
      title <- paste(i,':','\n', 'Headcount by Course Level', sep = '')
      save <- paste(path, subject, '9.jpg', sep = '')
      disag_hc_plot(plot_data, 'course_number', save, 'Course Level', title)

      # Freshman Status
      title <- paste(i,':','\n', 'Headcount by Freshman Status', sep = '')
      save <- paste(path, subject, '10.jpg', sep = '')
      disag_hc_plot(plot_data, 'enroll_status', save, 'Freshman Status', title)

      # Primary Language
      title <- paste(i,':','\n', 'Headcount by Primary Language', sep = '')
      save <- paste(path, subject, '11.jpg', sep = '')
      disag_hc_plot(plot_data, 'language', save, 'Primary Language', title)


      # Success Rate Plots...Age Range
      title <- paste(i,':','\n', 'Success Rate by Age Range', sep = '')
      save <- paste(path, subject, '12.jpg', sep = '')
      disag_scs_plot(plot_data, 'age', save, 'Age Range', title)

      # Age Range Collapsed
      title <- paste(i,':','\n', 'Success Rate by Age Range (Collapsed)',
                     sep = '')
      save <- paste(path, subject, '13.jpg', sep = '')
      disag_scs_plot(plot_data, 'agecol', save, 'Age Range', title)

      # Gender
      title <- paste(i,':','\n', 'Success Rate by Gender', sep = '')
      save <- paste(path, subject, '14.jpg', sep = '')
      disag_scs_plot(plot_data, 'gender', save, 'Gender', title)

      # Ethnicity
      title <- paste(i,':','\n', 'Success Rate by Ethnicity', sep = '')
      save <- paste(path, subject, '15.jpg', sep = '')
      disag_scs_plot(plot_data, 'ethnicity', save, 'Ethnicity', title)

      # Educational Goal
      title <- paste(i,':','\n', 'Success Rate by Ed Goal', sep = '')
      save <- paste(path, subject, '16.jpg', sep = '')
      disag_scs_plot(plot_data, 'matr_goal', save, 'Educational Goal', title)

      # Educational Level
      title <- paste(i,':','\n', 'Success Rate by Ed Level', sep = '')
      save <- paste(path, subject, '17.jpg', sep = '')
      disag_scs_plot(plot_data, 'matr_goal', save, 'Educational Level', title)

      # Instructional Mode
      title <- paste(i,':','\n', 'Success Rate by Instructional Mode', sep = '')
      save <- paste(path, subject, '18.jpg', sep = '')
      disag_scs_plot(plot_data, 'inst_mode', save, 'Instructional Mode', title)

      # Course Level
      title <- paste(i,':','\n', 'Success Rate by Course Level', sep = '')
      save <- paste(path, subject, '19.jpg', sep = '')
      disag_scs_plot(plot_data, 'course_number', save, 'Course Level', title)

      # Freshman Status
      title <- paste(i,':','\n', 'Success Rate by Freshman Status', sep = '')
      save <- paste(path, subject, '20.jpg', sep = '')
      disag_scs_plot(plot_data, 'enroll_status', save, 'Freshman Status', title)

      # Primary Language
      title <- paste(i,':','\n', 'Success Rate by Primary Language', sep = '')
      save <- paste(path, subject, '21.jpg', sep = '')
      disag_scs_plot(plot_data, 'language', save, 'Primary Language', title)
    }
  }

  if (use_subject) {
    program <- 'Collegewide'
    program_short <- 'CRCC'
  }

  # I don't like how this code repeats...
  # Headcount Plots (duplicated and unduplicated)
  title <- paste(program,':','\n', 'Duplicated Headcount by Academic Term',
                 sep = '')
  save <- paste(path, program_short,'.jpg', sep = '')
  plot_headcounts(data, save, title, undup = FALSE)
  title <- paste(program,':','\n', 'Unuplicated Headcount by Academic Term',
                 sep = '')
  save <- paste(path, program_short, '1.jpg', sep = '')
  plot_headcounts(data, save, title)

  # Disaggregated Headcount Plots....Age Range
  title <- paste(program,':','\n', 'Headcount by Age Range', sep = '')
  save <- paste(path, program_short, '2.jpg', sep = '')
  disag_hc_plot(data, 'age', save, 'Age Range', title)

  # Collapsed Age Range
  title <- paste(program,':','\n', 'Headcount by Age Range (Collapsed)', sep = '')
  save <- paste(path, program_short, '3.jpg', sep = '')
  disag_hc_plot(data, 'agecol', save, 'Age Range', title)

  # Gender
  title <- paste(program,':','\n', 'Headcount by Gender', sep = '')
  save <- paste(path, program_short, '4.jpg', sep = '')
  disag_hc_plot(data, 'gender', save, 'Gender', title)

  # Ethnicity
  title <- paste(program,':','\n', 'Headcount by Ethnicity', sep = '')
  save <- paste(path, program_short, '5.jpg', sep = '')
  disag_hc_plot(data, 'ethnicity', save, 'Ethnicity', title)

  # Educational Goal
  title <- paste(program,':','\n', 'Headcount by Ed Goal', sep = '')
  save <- paste(path, program_short, '6.jpg', sep = '')
  disag_hc_plot(data, 'matr_goal', save, 'Educational Goal', title)

  # Educational Level
  title <- paste(program,':','\n', 'Headcount by Ed Level', sep = '')
  save <- paste(path, program_short, '7.jpg', sep = '')
  disag_hc_plot(data, 'matr_goal', save, 'Educational Level', title)

  # Instructional Mode
  title <- paste(program,':','\n', 'Headcount by Instructional Mode', sep = '')
  save <- paste(path, program_short, '8.jpg', sep = '')
  disag_hc_plot(data, 'inst_mode', save, 'Instructional Mode', title)

  # Course Level
  title <- paste(program,':','\n', 'Headcount by Course Level', sep = '')
  save <- paste(path, program_short, '9.jpg', sep = '')
  disag_hc_plot(data, 'course_number', save, 'Course Level', title)

  # Freshman Status
  title <- paste(program,':','\n', 'Headcount by Freshman Status', sep = '')
  save <- paste(path, program_short, '10.jpg', sep = '')
  disag_hc_plot(data, 'enroll_status', save, 'Freshman Status', title)

  # Primary Language
  title <- paste(program,':','\n', 'Headcount by Primary Language', sep = '')
  save <- paste(path, program_short, '11.jpg', sep = '')
  disag_hc_plot(data, 'language', save, 'Primary Language', title)


  # Success Rate Plots...Age Range
  title <- paste(program,':','\n', 'Success Rate by Age Range', sep = '')
  save <- paste(path, program_short, '12.jpg', sep = '')
  disag_scs_plot(data, 'age', save, 'Age Range', title)

  # Age Range Collapsed
  title <- paste(program,':','\n', 'Success Rate by Age Range (Collapsed)',
                 sep = '')
  save <- paste(path, program_short, '13.jpg', sep = '')
  disag_scs_plot(data, 'agecol', save, 'Age Range', title)

  # Gender
  title <- paste(program,':','\n', 'Success Rate by Gender', sep = '')
  save <- paste(path, program_short, '14.jpg', sep = '')
  disag_scs_plot(data, 'gender', save, 'Gender', title)

  # Ethnicity
  title <- paste(program,':','\n', 'Success Rate by Ethnicity', sep = '')
  save <- paste(path, program_short, '15.jpg', sep = '')
  disag_scs_plot(data, 'ethnicity', save, 'Ethnicity', title)

  # Educational Goal
  title <- paste(program,':','\n', 'Success Rate by Ed Goal', sep = '')
  save <- paste(path, program_short, '16.jpg', sep = '')
  disag_scs_plot(data, 'matr_goal', save, 'Educational Goal', title)

  # Educational Level
  title <- paste(program,':','\n', 'Success Rate by Ed Level', sep = '')
  save <- paste(path, program_short, '17.jpg', sep = '')
  disag_scs_plot(data, 'matr_goal', save, 'Educational Level', title)

  # Instructional Mode
  title <- paste(program,':','\n', 'Success Rate by Instructional Mode',
                 sep = '')
  save <- paste(path, program_short, '18.jpg', sep = '')
  disag_scs_plot(data, 'inst_mode', save, 'Instructional Mode', title)

  # Course Level
  title <- paste(program,':','\n', 'Success Rate by Course Level', sep = '')
  save <- paste(path, program_short, '19.jpg', sep = '')
  disag_scs_plot(data, 'course_number', save, 'Course Level', title)

  # Freshman Status
  title <- paste(program,':','\n', 'Success Rate by Freshman Status', sep = '')
  save <- paste(path, program_short, '20.jpg', sep = '')
  disag_scs_plot(data, 'enroll_status', save, 'Freshman Status', title)

  # Primary Language
  title <- paste(program,':','\n', 'Success Rate by Primary Language', sep = '')
  save <- paste(path, program_short, '21.jpg', sep = '')
  disag_scs_plot(data, 'language', save, 'Primary Language', title)

}
