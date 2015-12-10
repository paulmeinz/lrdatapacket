#'
#'
#'
#'
#'
#'
#'
#'


disag_hc_plot <- function(data, emplid_col = 1, demo_col = 2, term_col = 3
                                , year_col = 4) {

  # Make each emplid unique for term and year.
  unique_data <- unique(data[,c(emplid_col,demo_col,term_col,year_col)])

  # Get total headcount by demo/year
  headcount <- plyr::ddply(unique_data,
                    c(names(data)[demo_col],names(data)[year_col]),
                    summarise, headcount = length(data[,1]), .drop = F)

  # Get total headcount by year
  total <- plyr::ddply(unique_data,c(names(data)[year_col]), summarise,
                 total = length(data[,emplid_col]), .drop = F)

  # Now merge the two
  plot_data <- merge(headcount, total, by.x = names(data)[year_col],
                     by.y = names(data)[year_col])

  # Use utils to calculate plot features (data level location, color, xaxis
  # label angle)
  label_loc <- set_label_loc(plot_data[,names(data)[demo_col]], headcount$headcount/total$total)
  color_scheme <- set_colors(plot_data[, names(data)[year_col]])
  xaxis_loc <- set_xaxis_label_loc(plot_data[, names(data)[demo_col]])

  # Set title
  title <- paste('default plot title')

  # Generate a plot
  plot <- ggplot(data = plot_data,
                 aes(x = names(data)[demo_col],
                 y = headcount/total,
                 fill = names(data)[year_col])) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_text(aes(x = names(data)[demo_col], y = headcount/total,
                    ymax = headcount/total,
                    label = paste(round(headcount/total*100, digit = 1),
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
