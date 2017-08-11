#' @title Simple run of DICOM 2 NIfTI Converter
#' @description Uses Chris Rorden's dcm2nii from 
#' http://www.mccauslandcenter.sc.edu/mricro/mricron/dcm2nii.html to convert
#' DICOM files to NIfTI format.  Should Need run \code{\link{install_dcm2nii}} before running.
#' @param basedir (character) directory to get files
#' @param copy_files (logical) Should files be copied to a temporary directory?
#' @param progdir (character) directory of bash scripts, no user input needed unless
#' binaries were installed elsewhere. Passed to \code{\link{dcm2nii_bin}}.
#' @param verbose (logical) print diagnostic printouts
#' @param dcm2niicmd (character) either "dcm2niix", "dcm2nii", or "dcm2nii_2009", which 
#' are different versions of dcm2nii. Passed to \code{\link{dcm2nii_bin}}.
#' @param opts list of arguments to pass to \code{dcm2nii}.
#' @param ... arguments to be passed to \code{\link{system}}
#' @return List of result of \code{system} command, names of files before and after
#' conversion
#' @export
#' @examples 
#' library(utils)
#' dcm_file = paste0("ftp://medical.nema.org/medical/Dicom/", 
#' "DataSets/WG30/MGH/MR/MouseBrainSiemens15T_20150410/", 
#' "Converted/DICOM/mghmousetoenhancedmr_T1w_pre.dcm")
#' tdir = tempfile()
#' dir.create(tdir)
#' destfile = tempfile(fileext = ".dcm", tmpdir = tdir)
#' ci = Sys.getenv("CI")
#' method = ifelse(ci == "", "auto", "curl")
#' dl = download.file(url = dcm_file, method = method, destfile = destfile)
#' dl == 0
#' stopifnot(file.exists(destfile))
#' dcm2niir::install_dcm2nii()
#' res = dcm2niir::dcm2nii(basedir = tdir)
#' stopifnot(res$result == 0)
dcm2nii <- function(basedir = ".", 
                    copy_files = TRUE,
                    progdir = system.file(package = "dcm2niir"), 
                    verbose = TRUE, 
                    dcm2niicmd = c("dcm2niix", "dcm2nii_2009", "dcm2nii"), 
                    opts = "-z y -f %p_%t_%s",
                    ...){  
  dcm2niicmd = dcm2nii_bin(
    progdir = progdir,
    dcm2niicmd = dcm2niicmd)
  basedir = path.expand(basedir)
  if (copy_files) {
    if (verbose) {
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
  cmd = dcm2niicmd
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
  if (res != 0) {
    warning("Result indicated an error!  Please check resutls.")
  }
  return(list(result = res, 
              nii_before = l_before,
              nii_after = l_after,
              cmd = cmd
  )
  )
} ## end dcm2nii
