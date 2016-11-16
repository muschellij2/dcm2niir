#' @title Install dcm2nii tools
#' @description Install dcm2nii to dcm2niir for conversion tools
#' @return NULL
#' @export
#' @param lib.loc a character vector with path names of R libraries.
#' Passed to \code{\link{system.file}}
#' @examples
#' install_dcm2nii()
#' @importFrom utils download.file unzip
install_dcm2nii = function(lib.loc = NULL){
  dcm2nii_files = system.file("dcm2nii", package = "dcm2niir",
                              lib.loc = lib.loc)

  if (!file.exists(dcm2nii_files)) {
    url = "http://muschellij2.github.io/cttools/dcm2nii_files.zip"
    urlfile <- file.path(
      system.file(package = "dcm2niir",
                  lib.loc = lib.loc),
      "dcm2nii_files.zip")
    download.file(url, urlfile)
    files = unzip(urlfile,
                  exdir = system.file(
                    package = "dcm2niir",
                    lib.loc = lib.loc))
    for (ifile in files) system(sprintf("chmod +x %s", ifile))
    x = file.remove(urlfile)
    rm(x)
  }
  dcm2nii_files = system.file("dcm2nii",
                              package = "dcm2niir",
                              lib.loc = lib.loc)
  return(file.exists(dcm2nii_files))
}