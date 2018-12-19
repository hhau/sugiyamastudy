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
#'  \item {base} {string: base name of distribution}
#'  \item {density} {function: density function of distribution, takes
#'    one argument for its location.}
#'  \item {generator} {function: generator function of distribution.}
#' }
.make_density_obj <- function(
  base_density = "norm",
  params = list(mean = 0, sd = 1),
  sample_size = 500
) {

  density_function <- function(x) {
    do.call(
      paste0("d", base_density),
      args = params
    )
  }

  generator_function <- function(n_samples) {
    do.call(
      paste0("r", base_density),
      args = params
    )
  }

  res <- list(
    base = base_density,
    density = density_function,
    generator = density_function,
  )

  class(res) <- "density"
  return(res)
 
}

.make_ratio_obj <- function(

) {

}
