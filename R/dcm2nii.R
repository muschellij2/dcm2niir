#' @title Simple run of DICOM 2 NIFTI Converter
#' @description Uses Chris Rorden's dcm2nii from 
#' http://www.mccauslandcenter.sc.edu/mricro/mricron/dcm2nii.html to convert
#' DICOM files to NIfTI format.  Should Need run \code{\link{install_dcm2nii}} before running.
#' @param basedir (character) directory to get files
#' @param copy_files (logical) Should files be copied to a temporary directory?
#' @param progdir (character) directory of bash scripts, no user input needed
#' unless necessary
#' @param verbose (logical) print diagnostic printouts
#' @param dcm2niicmd (character) either "dcm2niix", "dcm2nii", or "dcm2nii_2009", which 
#' are different versions of dcm2nii.  
#' @param ... arguments to be passed to \code{\link{system}}
#' @return Result of \code{system} command.
#' @export
dcm2nii <- function(basedir, 
                    copy_files = TRUE,
                        progdir = system.file(package = "dcm2niir"), 
                        verbose=TRUE, 
                        dcm2niicmd = c("dcm2niix", "dcm2nii_2009", "dcm2nii"), 
                        ...){  
  sysname = tolower(Sys.info()["sysname"])
  app = switch(sysname,
               linux = "_linux",
               darwin = "")
  dcm2niicmd = match.arg(dcm2niicmd)
  dcm2niicmd = paste0(dcm2niicmd, app)
  if (!file.exists(file.path(progdir, dcm2niicmd))){
    install_dcm2nii()
  } 
  stopifnot(file.exists(file.path(progdir, dcm2niicmd)))
  basedir = path.expand(basedir)
  if (copy_files){
    if (verbose){
      message("#Copying Files\n")
    }
    tdir = tempfile()
    dir.create(tdir)
    l = list.files(path = basedir, recursive = TRUE, all.files = TRUE,
                   full.names = TRUE)
    file.copy(from = l, to = tdir)
    basedir = tdir
  }
  if (verbose) cat("Converting to nii \n")
  cmd1 = sprintf('%s/%s', shQuote(progdir), dcm2niicmd)
  cmd2 = sprintf("%s", shQuote(basedir))
  cmd = paste(cmd1, " -z -f %p_%t_%s ", cmd2)
  if (verbose) print(cmd)
  res <- system(cmd, ...)      
  return(res)
} ## end dcm2nii
