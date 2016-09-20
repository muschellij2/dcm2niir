#' @title dcm2nii version
#' @description Finds the version of the dcm2nii function called 
#'
#' @param ... Arguments passed to \code{\link{dcm2nii_bin}}.
#' @export
#' 
#' @return A character string of the version
#' @examples \dontrun{
#' dcm2nii_version()
#' }
dcm2nii_version = function(
  ...
) {
  cmd = dcm2nii_bin(...)
  cmd = paste0(cmd, " -h")
  suppressWarnings({
    res = system(cmd, intern = TRUE)
  })
  res = trimws(res)
  res = grep("^Chris", res, value = TRUE)
  return(res)
}