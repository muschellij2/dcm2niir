#' @title Check Output from \code{dcm2nii}
#' @description This function checks whether the output from \code{\link{dcm2nii}}
#' runs correctly in that only one \code{nifti} file is returned if only one exists
#'
#' @param dcm2nii_output Output from \code{\link{dcm2nii}} command, must have
#' \code{nii_after} element
#' @param include_json Include JSON file if in the output?
#'
#' @return Character vector of unique nifti filenames
#' @importFrom dplyr arrange group_by desc slice
#' @export
#' @importFrom utils head
check_dcm2nii = function(dcm2nii_output,
                         include_json = TRUE){
  #   print(path)
  #   niis = dcm2nii(path)
  stub = NULL
  rm(list = "stub");
  niis = dcm2nii_output$nii_after
  if (include_json) {
    json = dcm2nii_output$json_after
  }
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
    if (include_json) {
      json = dcm2nii_output$json_after
      if (length(json) > 0) {
        bn = basename(json)
        bn = gsub("[.]json$", "", bn)
        ss = strsplit(bn, "_")
        ss = sapply(ss, function(x){
          paste(x[seq(l)], collapse = "_")
        }) 
      }
      j_df = data.frame(
        json = json,
        stub = ss,
        stringsAsFactors = FALSE)
      df = dplyr::left_join(df, j_df, by = "stub")
    }
    df = dplyr::arrange(df, stub, desc(nc))
    df = dplyr::group_by(df, stub)
    df = dplyr::slice(df, 1)
    niis = df$name
    if (include_json) {    
      json = df$json
    }
  }
  if (include_json) {
    attr(niis, "json_file") = json
  }
  niis
}
