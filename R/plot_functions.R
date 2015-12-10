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
  headcount <- ddply(unique_data,
                    c(names(data)[demo_col],names(data)[year_col]),
                    summarise, headcount = length(data,1), .drop = F)

  # Get total headcount by year
  total <- ddply(uniquedata,c(names(data)[year_col]), summarise,
                 total = length(EMPLID), .drop = F)

  # Now merge the two
  plot_data <- merge(headcount, total, by.x = 'acadyear', by.y = 'acadyear')


  myhj <- location(pltdata2$ages..1..,pltdata2$EMPLID.x/pltdata2$EMPLID.y)
  title <- paste('Headcount by Age Range:','\n',deptlong,year, sep = ' ')

  cbpallet <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00")[(6-length(levels(data$acadyear))):5]


  CRC_WIDE_A_AgeRange <- ggplot(data = pltdata2, aes(x = ages..1.., y = EMPLID.x/EMPLID.y, fill = acadyear)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    geom_text(aes(x = ages..1.., y = EMPLID.x/EMPLID.y, ymax = EMPLID.x/EMPLID.y,
                  label = paste(round(EMPLID.x/EMPLID.y*100, digit = 1),'%', sep = '')
    ),
    position=position_dodge(width=.9), vjust = .4, size = 4.5, angle = 90, hjust = myhj) +
    scale_fill_manual(values = cbpallet) +
    ggtitle(title)+
    xlab("Age Range") +
    ylab("Percentage") +
    guides(fill=guide_legend(title="Academic Year")) +
    scale_y_continuous(limits = c(0,1),label = percent)+
    theme(legend.position='bottom',
          axis.text = element_text(size = 12), axis.title = element_text (size = 12, face = 'bold'),
          plot.title = element_text(size = 12, face = 'bold'),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 12),
          axis.text.x = element_text(colour='black'),
          axis.text.y=element_text(colour='black'))

  print(CRC_WIDE_A_AgeRange)
  ggsave(paste(savedirectory, program,'2','.jpg',sep = ''), width=8.694, height=6.9583, dpi=72)
}
