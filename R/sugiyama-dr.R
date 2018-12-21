#' Estimate a density ratio and save the outputs
#' 
#' Major scaffolding function for my simulation study. The default arguments
#' demonstrate the expected formats for this function (and all functions within
#' this package). I've tried to be consistent with the naming, but there are 
#' some issues. 
#' 
#' Important assumptions:
#' - The "base_dist" component of the numerator/denominator objects has to have
#' a corresponding dbase_dist and rbase_dist methods, which a functions of
#' x and n_samples respectively (and the named parameters in the args 
#' component). 
#' 
#' @param numerator List with named components "base_dist" and "args", see 
#' defaults for example. 
#' @param denominator Same as \code{numerator}.
#' @param numerator_sample_size Number of samples to draw from the numerator.
#' @param denominator_sample_size Number of samples to draw from the 
#' denominator.
#' @param keep_samples boolean, should the ratio object keep the samples used to
#' estimate it? FALSE saves memory, but it can be useful to keep them if the 
#' ratio estimator breaks in a non-obvious way.
#' @param ratio_method string, either "clsif" or "ulsif".
#' @param ratio_lambda regularisation parameter for the \code{ratio_method} 
#' objectives.
#' @param ... other arugments that are passed to either 
#' \code{\link[densityratiosugiyama]{clsif}} or 
#' \code{\link[densityratiosugiyama]{ulsif}}
#' @param rds_directory directory to save the ratio object to as an RDS file.
#' @param pdf_diectory directory to save the ratio plot to.
#'
#' @return I have no idea what to return here 
#' @export
suigyama_dr <- function(
  numerator = list(base_dist = "norm", params = list(mean = 1, sd = 0.2)),
  denominator = list(base_dist = "t", params = list(df = 3, ncp = 0)),
  numerator_sample_size = 500,
  denominator_sample_size = 500,
  keep_samples = TRUE,
  ratio_method = "clsif",
  ratio_lambda = 0.1,
  ...,
  rds_directory = NULL,
  pdf_directory = NULL
) {
  # I should validate the inputs. 
  numerator_obj <- make_density_obj(
    base_density = numerator$base_dist,
    params = numerator$params,
    sample_size = numerator_sample_size
  )

  denominator_obj <- make_density_obj(
    base_density = denominator$base_dist,
    params = denominator$params,
    sample_size = denominator_sample_size
  )

  ratio_obj <- make_ratio_obj(
    numerator_density = numerator_obj,
    denominator_density = denominator_obj,
    ratio_method = ratio_method,
    keep_samples = keep_samples,
    ratio_lambda,
    ...
  )

  # uses globals from sugiyamastudy.R
  plot_ratio(
    ratio_obj = ratio_obj,
    output_directory = pdf_directory,
    PLOT_HEIGHT,
    PLOT_WIDTH
  )

  save_ratio(
    ratio_obj = ratio_obj,
    rds_directory = rds_directory
  )

  invisible(make_file_name(ratio_obj))

}
