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
    }
  }
}
