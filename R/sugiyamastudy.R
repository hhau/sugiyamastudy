#' @importFrom densityratiosugiyama clsif
#' @importFrom densityratiosugiyama ulsif
#' @importFrom densityratiosugiyama eval.basis
#' @import ggplot2
#' @import dplyr

NULL

# some globals that I might adjust

# how many standard deviations to go beyond the min/max of a sample
# (for plotting) - this really does not work well for cauchy distributions (duh)
SD_FACTOR = 0.5
PLOT_HEIGHT = 6
PLOT_WIDTH = 9