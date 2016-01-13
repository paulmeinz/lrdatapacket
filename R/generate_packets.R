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
      plot_headcounts(plot_data, paste(path, subject, '.jpg'), undup = FALSE)
      plot_headcounts(plot_data, paste(path, subject, '1.jpg'), undup = TRUE)
    }
  }
}
