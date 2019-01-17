# SD_FACTOR is a global defined in sugiyama-study.R
get_plot_limits <- function(numerator_samples, denominator_samples) {
  nu_min <- min(numerator_samples) - SD_FACTOR * stats::sd(numerator_samples)
  de_min <- min(denominator_samples) - SD_FACTOR * stats::sd(denominator_samples)
  plot_min <- min(nu_min, de_min)

  nu_max <- max(numerator_samples) + SD_FACTOR * stats::sd(numerator_samples)
  de_max <- max(denominator_samples) + SD_FACTOR * stats::sd(denominator_samples)
  plot_max <- max(nu_max, de_max)  

  return(list(
    min = plot_min,
    max = plot_max
  ))
}

make_par_string <- function(par_list) {
  n_pars <- length(par_list)
  par_string <- rep("", n_pars)
  
  for (ii in 1:n_pars) {
    par_string[ii] <- paste(
      names(par_list)[ii],
      par_list[ii],
      sep = "="
    )
  }
  
  par_string <- paste(par_string, collapse = "-")
  return(par_string)
}

make_file_name <- function(ratio_obj) {
  file_name_string <- paste(
    "nu",
    ratio_obj$numerator_density$base,
    ratio_obj$numerator_density$sample_size,
    make_par_string(ratio_obj$numerator_density$params),
    "de",
    ratio_obj$denominator_density$base,
    ratio_obj$denominator_density$sample_size,
    make_par_string(ratio_obj$denominator_density$params),
    ratio_obj$ratio_method,
    ratio_obj$basis_estimate$lambda,
  sep = "-")
  return(file_name_string)
}

# function is written to be called like:
#
# test_func <- function(alpha, beta) {
#   alpha^2 + beta
# }
#
# factorial_parallel_apply(test_func, alpha = 1:3, beta = 1:10)
#
# and this will compute all the output whilst parallelising over alpha
  
#' Apply a function to all combinations of other inputs 
#' 
#' Uses \code{\link[parallel]{mclapply}}, so works where fork() can be called.
#'
#' @param f A function to apply.
#' @param ... Named vector-like inputs.
#'
#' @return A data frame with column names equal to \code{names(list(...))} and
#' the name of \code{f}.
#' @export
#'
#' @examples
#' test_func <- function(alpha, beta) {
#'   alpha^2 + beta
#' }
#' 
#' factorial_parallel_apply(test_func, alpha = 1:3, beta = 1:10)
factorial_parallel_apply <- function(f, ...) {
  f_name <- deparse(substitute(f))
  full_inputs <- expand.grid(list(...))
  
  res <- do.call(c, parallel::mclapply(seq_along(full_inputs), function(i){
    do.call(f, as.list(full_inputs[i, ]))
  }, mc.cores = parallel::detectCores()))
  
  full_inputs[[f_name]] <- res
  return(full_inputs)
}
