## very much assumes a one dimensional ratio
#' Plot the estimated density ratio
#'
#' Plots the estimated density ratio, the true density ratio, and the true
#' component distributions, on both the original (probability density & ratio)
#' scale as well as the log scale (log density and log ratio).
#'
#' @param ratio_obj a ratio object produced by \code{\link{make_ratio_obj}}.
#' @param output_directory defaults to NULL, if not null, should be a file.path
#' that points to a directory where the pdf of the plot will be saved.
#' @param ... other arguments to be passed to \code{\link[ggplot2]{ggsave}}.
#'
#' @export
#' @return a ggplot2 object
plot_ratio <- function(
  ratio_obj,
  output_directory = NULL,
  plot_height = PLOT_HEIGHT,
  plot_width = PLOT_WIDTH
) {
  n_points <- 500
  x <- seq(
    from = ratio_obj$plot_limits$min,
    to = ratio_obj$plot_limits$max,
    length.out = n_points
  )

  # I should think about functionalising this string making stuff
  title_string <- paste0(
    "Numerator: ",
    ratio_obj$numerator_density$base,
    ", Denominator: ",
    ratio_obj$denominator_density$base
  )

  subtitle_string <- paste0(
    "Sample sizes: ",
    "Numerator: ",
    ratio_obj$numerator_density$sample_size,
    ", Denominator: ",
    ratio_obj$denominator_density$sample_size,
    ".",
    "   Lambda: ",
    ratio_obj$basis_estimate$lambda,
    "   Method: ",
    ratio_obj$ratio_method
  )

  y <- ratio_obj$ratio(x)

  true_ratio <- exp(
    log(ratio_obj$numerator_density$density(x)) -
    log(ratio_obj$denominator_density$density(x))
  )

  plot_df <- suppressWarnings(bind_rows(
    data.frame(
      x = x,
      y = y,
      Object = "Ratio estimate"
    ),
    data.frame(
      x = x,
      y = true_ratio,
      Object = "True ratio"
    ),
    data.frame(
      x = x,
      y = ratio_obj$numerator_density$density(x),
      Object = "Numerator density"
    ),
    data.frame(
      x = x,
      y = ratio_obj$denominator_density$density(x),
      Object = "Denominator density"
    )
  ) %>%
    mutate(Object = as.factor(Object)))

  plot_df <- suppressWarnings(bind_rows(
    plot_df %>% cbind(scale = "orig"),
    plot_df %>% mutate(y = log(y)) %>% cbind(scale = "log")
  ) %>%
    mutate(scale = as.factor(scale)))

  plot_res <- ggplot(plot_df, aes(x = x, y = y, group = Object, colour = Object)) +
    geom_line() +
    theme_bw() +
    labs(title = title_string, subtitle = subtitle_string) +
    facet_wrap(~ scale, nrow = 1, scales = "free") +
    scale_color_viridis_d()

  if (!is.null(output_directory)) {
    plot_file_name <- paste0(
      make_file_name(ratio_obj),
      ".pdf"
    )

    ggsave(
      filename = file.path(output_directory, plot_file_name),
      plot = plot_res,
      device = "pdf",
      width = PLOT_WIDTH,
      height = PLOT_HEIGHT
    )
  }

  return(plot_res)
}
