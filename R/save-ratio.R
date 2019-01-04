#' Save the density ratio object
#'
#' 
#' @param ratio_obj "ratio_sugi" object to be saved
#' @param rds_directory directory to keep rds object in
#'
#' @return whatever \code{\link[base]{saveRDS}} returns
#' @export
save_ratio <- function(ratio_obj, rds_directory) {
  # build the file name - has to encode quite a bit of information.
  # string should look like:
  # nu-(BASE_DIST)-(NU_SAMPLE-SIZE)-(NU_PARS)\
  # de-(BASE_DIST)-(DE_SAMPLE-SIZE)-(DE_PARS)\
  # (METHOD)-(LAMDBA).rds
  # ex: "nu-norm-500-mean=1-sd=0.1-de-t-350-df=3-ncp=1-clisf-0.1.pdf"
  file_name_string <- paste0(
    make_file_name(ratio_obj),
    ".rds"
  )

  # build the file path
  output_path <- file.path(
    rds_directory,
    file_name_string
  )

  # write the rds file
  # implicitly return whatever saveRDS returns? do i need a return statement for
  # consistency?
  saveRDS(
    ratio_obj,
    file = output_path
  )
}
