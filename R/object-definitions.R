#' make "density" object
#'
#' @param base_density a base string for a density with d\* and r\* functions,
#' for example "norm", as it has "dnorm" and "rnorm".
#' @param params a named list of parameters shared between the d\* and r\*
#' functions
#' @param sample_size number of samples to produce from this distirbution.
#'
#' @return an S3 object of type "density", with components:
#' \itemize{
#'  \item{\emph{base}}{  string: base name of distribution.}
#'  \item{\emph{density}}{  function: density function of distribution, takes
#'    one argument, its location.}
#'  \item{\emph{generator}}{  function: generator function of distribution.}
#' }
#' @export
make_density_obj <- function(
  base_density = "norm",
  params = list(mean = 0, sd = 1),
  sample_size = 500
) {
  density_function <- function(x) {
    args <- params
    args[["x"]] <- x

    do.call(
      paste0("d", base_density),
      args = args
    )
  }

  generator_function <- function(n_samples) {
    args <- params
    args[["n"]] <- n_samples
    
    do.call(
      paste0("r", base_density),
      args = args
    )
  }

  res <- list(
    base = base_density,
    density = density_function,
    generator = generator_function,
    sample_size = sample_size,
    params = params
  )

  class(res) <- "density_sugi"
  return(res)
}

#' make "ratio" object 
#' 
#' estimates the density ratio for two density objects.
#'
#' @param numerator_density a "density_sugi" object representing the density of 
#' the numerator.
#' @param denominator_density a "density_sugi" object representing the density 
#' of the denominator.
#' @param ratio_method string: either "clisf" for CLISF fitting (L1 penalty, 
#' no positivity constraint), or "ulsif" for ULSIF fitting (L2 penalty, 
#' with positivity constraint).
#' @param ratio_lambda positive numeric: value of regularisation parameter.
#' @param keep_samples boolean: keep the samples used for estimating the density
#' ratio or not.
#' @param ... named list of other options passed to either 
#' \code{\link[densityratiosugiyama]{clsif}} or 
#' \code{\link[densityratiosugiyama]{ulsif}}
#'
#' @return an S3 object of type "ratio_sugi", with components:
#' \itemize{
#'  \item{\emph{numerator_density}}{  S3 object of type "density" for the numerator.}
#'  \item{\emph{denominator_density}}{  S3 object of type "density" for the 
#'    denominator.}
#'  \item{\emph{ratio_method}}{  see: ratio_method.}
#'  \item{\emph{ratio}}{  function: evaluates the estimated density at the requested
#'    location.}
#'  \item{\emph{basis_estimate}}{  list, basis estimate. see 
#'    \code{\link[densityratiosugiyama]{clsif}} and 
#'    \code{\link[densityratiosugiyama]{eval.basis}}.}
#'  \item{\emph{plot_limits}}{  list, named elements are some reasonable(?) bounds for
#'    plots}
#' }
#' @export
make_ratio_obj <- function(
  numerator_density,
  denominator_density,
  ratio_method,
  ratio_lambda,
  keep_samples = TRUE,
  ...
) {
  stopifnot(
    identical(
      x = (class(numerator_density) == class(denominator_density)),
      y = (class(numerator_density) == "density_sugi")
    )
  )

  # produce the samples
  numerator_density[["samples"]] <- 
    with(numerator_density, generator(sample_size))

  denominator_density[["samples"]] <- 
    with(denominator_density, generator(sample_size))

  plot_limits <- get_plot_limits(
    numerator_density$samples, denominator_density$samples
  ) 

  # estimate the density ratio
  args <- list(...)
  args[["x.de"]] <- denominator_density$samples 
  args[["x.nu"]] <- numerator_density$samples
  args[["lambda"]] <- ratio_lambda

  ratio_est <- do.call(
    ratio_method,
    args
  ) 

  ratio_est[["lambda"]] <- args$lambda

  # see if we want to keep the samples
  if (!keep_samples) {
    numerator_density[["samples"]] <- NULL
    denominator_density[["samples"]] <- NULL
  }

  # closure the density ratio evaluator
  ratio <- function(x) {
    eval.basis(
      ratio_est,
      x
    )
  }

  res <- list(
    numerator_density = numerator_density,
    denominator_density = denominator_density,
    ratio_method = ratio_method,
    ratio = ratio,
    basis_estimate = ratio_est,
    plot_limits = plot_limits
  )
  class(res) <- "ratio_sugi"
  return(res)
}
