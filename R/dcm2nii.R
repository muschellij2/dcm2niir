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
#' @param opts list of arguments to pass to \code{dcm2nii}.
#' @param ... arguments to be passed to \code{\link{system}}
#' @return List of result of \code{system} command, names of files before and after
#' conversion
#' @export
dcm2nii <- function(basedir, 
                    copy_files = TRUE,
                    progdir = system.file(package = "dcm2niir"), 
                    verbose=TRUE, 
                    dcm2niicmd = c("dcm2niix", "dcm2nii_2009", "dcm2nii"), 
                    opts = "-z y -f %p_%t_%s",
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
  l_before = list.files(pattern = "[.]nii", path = basedir, 
                        recursive = TRUE, all.files = TRUE,
                        full.names = TRUE)
  if (verbose) {
    message("# Converting to nii \n")
  }
  cmd = file.path(progdir, dcm2niicmd)
  cmd1 = sprintf("%s", shQuote(cmd))
  cmd2 = sprintf("%s", shQuote(basedir))
  cmd = paste0(cmd1, " ", opts, " ", cmd2)
  if (verbose) {
    message(cmd)
  }
  res <- system(cmd, ...)
  l_after = list.files(pattern = "[.]nii", 
                       path = basedir, 
                       recursive = TRUE, all.files = TRUE,
                       full.names = TRUE)
  
  return(list(result = res, 
              nii_before = l_before,
              nii_after = l_after,
              cmd = cmd
  )
  )
} ## end dcm2nii
