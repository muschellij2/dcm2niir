#' @title Check Output from \code{dcm2nii}
#' @description This function checks whether the output from \code{\link{dcm2nii}}
#' runs correctly in that only one \code{nifti} file is returned if only one exists
#'
#' @param dcm2nii_output Output from \code{\link{dcm2nii}} command, must have
#' \code{nii_after} element
#'
#' @return Character vector of unique nifti filenames
#' @importFrom dplyr arrange group_by desc
#' @export
#' @importFrom utils head
check_dcm2nii = function(dcm2nii_output){
#   print(path)
#   niis = dcm2nii(path)
  stub = NULL
  rm(list = "stub");
  niis = dcm2nii_output$nii_after
  if (length(niis) > 1) {
    bn = basename(niis)
    bn = gsub("[.]gz$", "", bn)
    bn = gsub("[.]nii$", "", bn)
    ss = strsplit(bn, "_")
    nc = nchar(bn)
    l = min(sapply(ss, length))
    ss = sapply(ss, function(x){
      paste(x[seq(l)], collapse = "_")
    })
    df = data.frame(
      name = niis,
      stub = ss,
      nc = nc,
      stringsAsFactors = FALSE)
    df = dplyr::arrange(df, stub, desc(nc))
    df = head(dplyr::group_by(df, stub), 1)
    niis = df$name
  }
  niis
}
