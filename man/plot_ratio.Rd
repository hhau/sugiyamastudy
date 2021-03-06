% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-ratio.R
\name{plot_ratio}
\alias{plot_ratio}
\title{Plot the estimated density ratio}
\usage{
plot_ratio(ratio_obj, output_directory = NULL,
  plot_height = PLOT_HEIGHT, plot_width = PLOT_WIDTH)
}
\arguments{
\item{ratio_obj}{a ratio object produced by \code{\link{make_ratio_obj}}.}

\item{output_directory}{defaults to NULL, if not null, should be a file.path
that points to a directory where the pdf of the plot will be saved.}

\item{plot_height}{output pdf plot height in inches(?)}

\item{plot_width}{output pdf plot width in inches(?)}

\item{...}{other arguments to be passed to \code{\link[ggplot2]{ggsave}}.}
}
\value{
a ggplot2 object
}
\description{
Plots the estimated density ratio, the true density ratio, and the true
component distributions, on both the original (probability density & ratio)
scale as well as the log scale (log density and log ratio).
}
