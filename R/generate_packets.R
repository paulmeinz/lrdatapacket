#'
#'
#'
#'
#'

generate_packets <- function(data,
                             path,
                             use_subject = 'TRUE',
                             program = '') {

  subjects <- unique(data$subject_long)

  if (use_subject) {
    for (i in subjects) {
      plot_data <- data[data$subject_long == i,]
      subject <- plot_data[1, 'subject']
      title <- paste(i,':','\n', 'Duplicated Headcount by Academic Term', sep = '')
      save <- paste(path, subject,'.jpg', sep = '')
      plot_headcounts(plot_data, save, title, undup = FALSE)
      title <- paste(i,':','\n', 'Unuplicated Headcount by Academic Term', sep = '')
      save <- paste(path, subject, '1.jpg', sep = '')
      plot_headcounts(plot_data, save, title)
      title <- paste(i,':','\n', 'Headcount by Age Range', sep = '')
      save <- paste(path, subject, '2.jpg', sep = '')
      disag_hc_plot(data, 'age', save, 'Age Range', title)
      title <- paste(i,':','\n', 'Headcount by Age Range (Collapsed)', sep = '')
      save <- paste(path, subject, '3.jpg', sep = '')
      disag_hc_plot(data, 'agecol', save, 'Age Range', title)
      title <- paste(i,':','\n', 'Headcount by Gender', sep = '')
      save <- paste(path, subject, '4.jpg', sep = '')
      disag_hc_plot(data, 'gender', save, 'Gender', title)
      title <- paste(i,':','\n', 'Headcount by Ethnicity', sep = '')
      save <- paste(path, subject, '5.jpg', sep = '')
      disag_hc_plot(data, 'ethnicity', save, 'Ethnicity', title)
      title <- paste(i,':','\n', 'Headcount by Ed Goal', sep = '')
      save <- paste(path, subject, '6.jpg', sep = '')
      disag_hc_plot(data, 'matr_goal', save, 'Educational Goal', title)
      title <- paste(i,':','\n', 'Headcount by Ed Level', sep = '')
      save <- paste(path, subject, '7.jpg', sep = '')
      disag_hc_plot(data, 'matr_goal', save, 'Educational Level', title)
      title <- paste(i,':','\n', 'Headcount by Instructional Mode', sep = '')
      save <- paste(path, subject, '8.jpg', sep = '')
      disag_hc_plot(data, 'inst_mode', save, 'Instructional Mode', title)
      title <- paste(i,':','\n', 'Headcount by Course Level', sep = '')
      save <- paste(path, subject, '9.jpg', sep = '')
      disag_hc_plot(data, 'course_number', save, 'Course Level', title)
      title <- paste(i,':','\n', 'Headcount by Freshman Status', sep = '')
      save <- paste(path, subject, '10.jpg', sep = '')
      disag_hc_plot(data, 'enroll_status', save, 'Freshman Status', title)
      title <- paste(i,':','\n', 'Headcount by Primary Language', sep = '')
      save <- paste(path, subject, '11.jpg', sep = '')
      disag_hc_plot(data, 'language', save, 'Primary Language', title)
    }
  }
}
