#'
#'
#'
#'
#'
#'
#'
#'


disag_hc_plot <- function(data, demo_col) {

  # Determine if data object of correct class
  if (class(data)[2] != 'lrdataset') {
    stop('This function only supports lrdataset data objects')
  }

  # Change the demo_column name to generic name
  demo <- names(data)  == demo_col
  names(data)[demo] <- 'demo_col'

  # Make each emplid unique for term and year.
  unique_data <- unique(data[, c('id', 'term', 'acad_year', 'demo_col')])

  print(unique_data)
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
  print(pt_data)

  label_loc <- set_label_loc(pt_data[, 'demo_col'],
                             pt_data$headcount/pt_data$total)
  color_scheme <- set_colors(pt_data[, 'acad_year'])
  xaxis_loc <- set_xaxis_label_loc(pt_data[, 'demo_col'])

  # Set title
  title <- paste('default plot title')

  # Generate a plot
  plot <- ggplot(data = pt_data,
                 aes(x = demo_col,
                 y = headcount/total,
                 fill = acad_year)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(x = demo_col,
                        y = headcount/total,
                        ymax = headcount/total,
                        label = paste(round(headcount/total*100
                                            , digit = 1),
                        '%', sep = '')),
                    position=position_dodge(width=.9), vjust = .4, size = 4.5,
                    angle = 90, hjust = label_loc) +
          scale_fill_manual(values = color_scheme) +
          ggtitle(title)+
          xlab("default label") +
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


  print(plot)
  ggsave(paste('C:/Users/W1563070/Documents/testing.jpg'),
         width=8.694, height=6.9583, dpi=72)
}
