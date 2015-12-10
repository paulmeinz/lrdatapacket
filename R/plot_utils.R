# Function for determining whether data labels should sit above or within
# the relevant bar.

set_label_loc <- function(xaxis, data) {

  hj <- c(1:length(data))

  # If bar value is above or below 15% assign a 1 or 0 respectively
  hj[data > .15] <- 1
  hj[data <=.15] <- 0

  # Now order the location values by the xaxis variable
  hj <- data.frame(xaxis, hj)
  hj <- hj[order(xaxis),]

  hj[,2]

}


# Determine bar color scheme

set_colors <- function(acad_year) {

  # Use color blind friendly colors
  colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00")
  color_scheme <- colors[(6-length(levels(acad_year))):5]

  color_scheme
}


# Determine x-axis label angles

set_xaxis_label_loc <- function(xaxis) {

  max_char <- max(nchar(levels(xaxis)))
  n_levels <- length(levels(xaxis))

  # If an xaxis factor has more than 2 levels and has a level that is more than
  # 15 characters
  if (max_char >= 15 & n_levels > 2) {
    label_loc <- c(35, 1)
    return(label_loc)
  }

  label_loc <- c(45, 0)
  label_loc
}
