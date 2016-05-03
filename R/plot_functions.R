#' Generate a summary demographic plot
#'
#' Takes a lrdataset object (generated from the query_data function) and
#' produces a plot for the past five years disaggregated by a
#' demographic variable. disag_hc_plot produces a disaggregated demographic
#' plot, disag_scs_plot produces a disaggregated success plot, and
#' plot_headcounts produces a headcount (duplicated or unduplicated) plot.
#'
#' @param data A lrdataset object generated from the query_gen function.
#' @param demo_col The name of a demographic variable in the lrdataset object.
#' This demographic variable will be used to disaggregate.
#' @param path A file path to where you would like to save your plot. The 
#' filepath must end in the filename (e.g. 'path/filename.jpg')
#' @param x_axis_lab The x-axis label.
#' @param title The plot title
#'
#' @details
#' You must use a lrdataset object with this function. The purpose of this
#' restriction is to ensure that the dataset has only standard data and column
#' names
#'
#' @examples
#' \dontrun{disag_hc_plot(data, 'gender', 'User/documents/ex.jpg','Gender',
#' 'xaxislabel')}
#'
#' @export


disag_hc_plot <- function(data,
                          demo_col,
                          path,
                          x_axis_lab = '',
                          title = '') {

  # Determine if data object of correct class
  if (class(data)[2] != 'lrdataset') {
    stop('This function only supports lrdataset data objects')
  }

  # Change the demo_column name to generic name
  demo <- names(data) == demo_col
  names(data)[demo] <- 'demo_col'

  # Make each emplid unique for term and year.
  unique_data <- unique(data[, c('id', 'term', 'acad_year', 'demo_col')])

  # Get total headcount by demo/year
  headcount <- plyr::ddply(unique_data,
                           c('demo_col', 'acad_year'),
                           summarise,
                           headcount = length(id), .drop = F)

  # Get total headcount by year
  total <- plyr::ddply(unique_data,c('acad_year'), summarise,
                       total = length(id), .drop = F)


  # Now merge the two
  pt_data <- merge(headcount, total, by.x = 'acad_year', by.y = 'acad_year')

  # Use utils to calculate plot features (data level location, color, xaxis
  # label angle
  label_loc <- set_label_loc(pt_data[, 'demo_col'],
                             pt_data$headcount/pt_data$total)
  color_scheme <- set_colors(pt_data[, 'acad_year'])
  xaxis_loc <- set_xaxis_label_loc(pt_data[, 'demo_col'])

  # Generate a plot
  plot <- ggplot(data = pt_data,
                 aes(x = demo_col,
                 y = headcount/total,
                 fill = acad_year)) +

          # Add barchart type
          geom_bar(stat = 'identity', position = 'dodge') +

          # Modify test aesthetics
          geom_text(aes(x = demo_col,
                        y = headcount/total,
                        ymax = headcount/total,
                        label = paste(round(headcount/total*100
                                            , digit = 1),
                        '%', sep = '')),
                    position=position_dodge(width=.9), vjust = .4, size = 4.5,
                    angle = 90, hjust = label_loc) +

          # Add color blind friendly scheme
          scale_fill_manual(values = color_scheme) +

          # Title and some additional themes
          ggtitle(title)+
          xlab(x_axis_lab) +
          ylab("Percentage") +
          guides(fill=guide_legend(title="Academic Year")) +
          scale_y_continuous(limits = c(0,1))+
          theme(legend.position='bottom',
                axis.text = element_text(size = 12),
                axis.title = element_text (size = 12, face = 'bold'),
                plot.title = element_text(size = 12, face = 'bold'),
                legend.text = element_text(size = 9),
                legend.title = element_text(size = 12),
                axis.text.x = element_text(angle = xaxis_loc[1],
                                           hjust = xaxis_loc[2]
                                           , colour='black'),
                axis.text.y=element_text(colour='black'))


  # Display and save the plot
  print(plot)
  ggsave(path, width=8.694, height=6.9583, dpi=72)
}

#' @describeIn disag_hc_plot Produce a Success Rate Plot
#' @export

disag_scs_plot <- function(data,
                           demo_col,
                           path,
                           x_axis_lab = '',
                           title = '') {

  # Determine if data object of correct class
  if (class(data)[2] != 'lrdataset') {
    stop('This function only supports lrdataset data objects')
  }

  # Change the demo_column name to generic name
  demo <- names(data) == demo_col
  names(data)[demo] <- 'demo_col'

  # Get average succes by year and demo
  pt_data <- plyr::ddply(data, c('demo_col', 'acad_year'), summarise,
                         success = mean(success), .drop = F)

  # Use utils to calculate plot features (data level location, color, xaxis
  # label angle
  label_loc <- set_label_loc(pt_data[, 'demo_col'],
                             pt_data$success/100)
  color_scheme <- set_colors(pt_data[, 'acad_year'])
  xaxis_loc <- set_xaxis_label_loc(pt_data[, 'demo_col'])

  # Additional manipulation of data labels such that no data doesnt have a
  # label
  labels <- paste(round(pt_data$success, digit = 1), "%", sep = '')
  labels[labels == "NaN%"] <- ''
  pt_data$success[is.nan(pt_data$success)] <- 0.0
  pt_data <- data.frame(pt_data, labels)

  # Generate a plot
  plot <- ggplot(data = pt_data,
                 aes(x = demo_col,
                     y = success/100,
                     fill = acad_year)) +

    # Set chart type
    geom_bar(stat = 'identity', position = 'dodge') +

    # Set text aesthetics
    geom_text(aes(x = demo_col,
                  y = success/100,
                  ymax = success/100,
                  label = labels),
              position=position_dodge(width=.9), vjust = .4, size = 4.5,
              angle = 90, hjust = label_loc) +

    # Set color blind color scheme
    scale_fill_manual(values = color_scheme) +

    # Set axis titles and more aesthetics
    ggtitle(title)+
    xlab(x_axis_lab) +
    ylab("Percentage") +
    guides(fill=guide_legend(title="Academic Year")) +
    scale_y_continuous(limits = c(0,1))+
    theme(legend.position='bottom',
          axis.text = element_text(size = 12),
          axis.title = element_text (size = 12, face = 'bold'),
          plot.title = element_text(size = 12, face = 'bold'),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 12),
          axis.text.x = element_text(angle = xaxis_loc[1],
                                     hjust = xaxis_loc[2]
                                     , colour='black'),
          axis.text.y=element_text(colour='black'))

  # Display and export
  print(plot)
  ggsave(path, width=8.694, height=6.9583, dpi=72)

}

#' @describeIn disag_hc_plot Produce a Headcount Plot
#' @export

plot_headcounts <- function(data, path, title = '', undup = TRUE) {

  # If undup is true then deduplify by term
  if (undup) {
    headcount <- unique(data[,c('id','term_desc','term')])
    pt_data <- aggregate(id ~ term_desc + term, data = headcount, length)
    y_title <- 'Unduplicated'
  }

  # Otherwise don't deuplify
  else {
    pt_data <- aggregate(id ~ term_desc + term, data = data, length)
    y_title <- 'Duplicated'
  }

  # Order term description by term for plotting, save as an ordered factor
  pt_data$term_desc <- factor(pt_data$term_desc,
                              levels = pt_data$term_desc[order(pt_data$term)],
                              ordered = T)

  # Get plot min max values
  low <- min(pt_data$id)
  high <- max(pt_data$id)
  low <- low - low/2
  high <- high + high/2

  # Make a line chart
  plot <- ggplot(data = pt_data, aes(x = term_desc, y = id)) +

    # Set plot type as line/point
    geom_line(aes(group = 1)) +
    geom_point() +

    # Set titles and other aesthetics
    xlab("Academic Term") +
    ylab(paste(y_title, "Headcount", sep = ' ')) +
    ggtitle(title) +
    geom_text(aes(x = term_desc, y = id, ymax = id, label = id),
                  position=position_dodge(width=.9), vjust = -.3, size = 4.5,
                  hjust = 0, angle = 35) +
    scale_y_continuous(limit = c(low, high)) +
    theme(axis.text = element_text(size = 10),
                                   axis.title = element_text(size = 12,
                                                             face = 'bold'),
          plot.title = element_text(size = 14, face = 'bold'),
          axis.text.x = element_text(angle = 35, hjust = 1, colour='black'),
          axis.text.y = element_text(colour='black'))

  # Display and save the plot
  print(plot)
  ggsave(path, width=8.694, height=6.9583, dpi=72)
}
