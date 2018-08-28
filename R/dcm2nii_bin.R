#' @title Make path for dcm2nii binary
#' @description Wrapper for checking if dcm2nii binaries are installed and 
#' creating a file path to the binary requested.
#'
#' @param progdir (character) directory of bash scripts, no user input needed unless
#' binaries were installed elsewhere
#' @param dcm2niicmd (character) either "dcm2niix", "dcm2nii", or "dcm2nii_2009", which 
#' are different versions of dcm2nii. 
#' @return Character vector 
#' @export
#'
#' @examples \dontrun{
#' dcm2nii_bin()
#' }
dcm2nii_bin = function(      
  progdir = system.file(package = "dcm2niir"), 
  dcm2niicmd = c("dcm2niix", "dcm2nii_2009", "dcm2nii")
  ) {
  sysname = tolower(Sys.info()["sysname"])
  app = switch(sysname,
               linux = "_linux",
               darwin = "")
  dcm2niicmd = match.arg(dcm2niicmd)
  dcm2niicmd = paste0(dcm2niicmd, app)
  dcm2niicmd = file.path(progdir, dcm2niicmd)
  
  if (!file.exists(dcm2niicmd)) {
    install_dcm2nii(progdir = progdir)
  } 
  stopifnot(file.exists(dcm2niicmd))
  return(dcm2niicmd)
}