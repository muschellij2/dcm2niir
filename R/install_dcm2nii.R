#' @title Install dcm2nii tools
#' @description Install dcm2nii to dcm2niir for conversion tools 
#' @return NULL
#' @export
#' @examples 
#' install_dcm2nii()
install_dcm2nii = function(){
  dcm2nii_files = system.file("dcm2nii", package= "dcm2niir")
  
  if (!file.exists(dcm2nii_files)){
    url = "http://muschellij2.github.io/cttools/dcm2nii_files.zip"
    urlfile <- file.path(system.file(package="dcm2niir"), "dcm2nii_files.zip")
    download.file(url, urlfile, quiet=TRUE)
    files = unzip(urlfile, exdir = system.file(package="dcm2niir"))
    for (ifile in files) system(sprintf("chmod +x %s", ifile))
    x = file.remove(urlfile)
  }
  dcm2nii_files = system.file("dcm2nii", package= "dcm2niir")  
  return(file.exists(dcm2nii_files))
}