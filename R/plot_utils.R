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
