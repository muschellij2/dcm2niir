#' @title DICOM 2 NIFTI Converter
#' @description Uses Chris Rorden's dcm2nii from 
#' http://www.mccauslandcenter.sc.edu/mricro/mricron/dcm2nii.html to convert
#' DICOM files to NIfTI format.  Need
#' to run \code{\link{install_dcm2nii}} before running.
#' @param basedir (character) directory to run \code{dcm2nii}
#' @param progdir (character) directory of bash scripts, no user input needed
#' unless necessary
#' @param verbose (logical) print diagnostic printouts
#' @param dcm2niicmd (character) either "dcm2nii" or "dcm2nii_2009", which 
#' are different versions of dcm2nii.  Can also be "dcm2niix" - experimental
#' and only Mac OSX
#' @return List of result of \code{system} run and indication of an error
#' @export
#' @examples \dontrun{
#' ### This code will install the dcm2nii functions
#' 
#' dcm2nii_files = system.file("dcm2nii", package= "dcm2niir")
#' 
#' if (!file.exists(dcm2nii_files)){
#'   install_dcm2nii()
#' }
#' }
dcm2nii <- function(basedir, 
                    progdir = system.file(package="dcm2niir"), 
                    verbose=TRUE, 
                    dcm2niicmd = c("dcm2nii_2009", "dcm2nii", "dcm2niix"), 
                    ...){  
  sysname = tolower(Sys.info()["sysname"])
  app = switch(sysname,
               linux = "_linux",
               darwin = "")
  dcm2niicmd = dcm2niicmd[1]
  dcm2niicmd = match.arg(dcm2niicmd, 
                         c("dcm2nii_2009", "dcm2nii", "dcm2niix"))

  dcm2niicmd = paste0(dcm2niicmd, app)
  if (!file.exists(file.path(progdir, dcm2niicmd))){
    install_dcm2nii()
  } 
  stopifnot(file.exists(file.path(progdir, dcm2niicmd)))
  basedir = normalizePath(basedir)
  
  intern=TRUE
  if (!grepl("dcm2niix", dcm2niicmd)){
    cmd = sprintf('%s/%s -b "%s"/CT_dcm2nii.ini "%s"', progdir,
                  dcm2niicmd, progdir, basedir)
  } else {
    cmd1 = sprintf('%s', file.path(progdir, dcm2niicmd))
    cmd2 = sprintf("%s", basedir)
    cmd = paste(cmd1, " -z -f %p_%t_%s ", cmd2)
    if (verbose) print(cmd)
  }
  res <- system(cmd, intern=intern)      
  if (intern) {
    errs <- any(grepl("Error|use MRIcro|Unsupported Transfer Syntax", res))
  } else {
    errs <- res != 0
  }
  stopifnot(length(errs) == 1)
  
  return(list(result=res, error = errs))
} ## end dcm2nii
