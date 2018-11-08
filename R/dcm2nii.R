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
#' @param merge_files Should files be merged, 
#' passed do \code{dcm2nii} options
#' @param ignore_derived Should derived images be ignored,
#' passed do \code{dcm2nii} options
#' @export
#' @examples 
#' library(utils)
#' install_dir = tempdir()
#' install_dcm2nii(progdir = install_dir)
#' ## dcm_file = paste0("ftp://medical.nema.org/medical/Dicom/", 
#' ## "DataSets/WG30/MGH/MR/MouseBrainSiemens15T_20150410/", 
#' ## "Converted/DICOM/mghmousetoenhancedmr_T1w_pre.dcm")
#' dcm_file = paste0("http://johnmuschelli.com/dcm2niir/", 
#' "mghmousetoenhancedmr_T1w_pre.dcm")
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
                    merge_files = FALSE,
                    ignore_derived = FALSE,
                    opts = paste0(
                      "-9 ",
                      ifelse(ignore_derived, "-i y ", ""),
                      ifelse(merge_files, " -m y ", ""),
                      "-z y -f %p_%t_%s"),
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
  j_before = list.files(pattern = "[.]json$", 
                        path = basedir, 
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
  j_after = list.files(pattern = "[.]json$", 
                       path = basedir, 
                       recursive = TRUE, all.files = TRUE,
                       full.names = TRUE)  
  if (res != 0) {
    warning("Result indicated an error!  Please check resutls.")
  }
  return(list(result = res, 
              nii_before = l_before,
              nii_after = l_after,
              json_after = j_after,
              json_before = j_before,
              cmd = cmd
  )
  )
} ## end dcm2nii

#' @param file A Par/REC file
#' @rdname dcm2nii
#' @export
dcm2nii_par_rec <- function(
  file = list.files(pattern = "[.](par|PAR)"), 
  copy_files = TRUE,
  verbose = TRUE,
  ...){  
  file = c(file, 
           sub("[.](par|PAR)$", ".rec", file),
           sub("[.](par|PAR)$", ".REC", file),
           sub("[.](rec|REC)$", ".par", file),
           sub("[.](rec|REC)$", ".PAR", file))
  file = unique(file)
  file = file[ file.exists(file)]
  file = normalizePath(file, mustWork = TRUE)
  if (copy_files) {
    if (verbose) {
      message("#Copying Files\n")
    }
    tdir = tempfile()
    dir.create(tdir)
    file.copy(from = file, to = tdir)
    file = file.path(tdir, basename(file))
  }
  basedir = unique(dirname(file))
  stopifnot(length(basedir) == 1)
  
  res = dcm2nii(basedir = basedir, 
                copy_files = FALSE, 
                verbose = verbose, ...)
  return(res)
} ## end dcm2nii


#' @rdname dcm2nii
#' @export
dcm2nii_bids_sidecar = function(
  basedir,
  progdir = system.file(package = "dcm2niir"), 
  dcm2niicmd = c("dcm2niix", "dcm2nii_2009", "dcm2nii"),
  ...
) {
  args = list(...)
  if (is.null(args$ignore_derived)) {
    args$ignore_derived = FALSE
  }
  if (is.null(args$merge_files)) {
    args$merge_files = FALSE
  }
  opts = c(
    "-b o",
    ifelse(args$ignore_derived, "-i y ", ""),
    ifelse(args$merge_files, " -m y ", "")
  )
  opts = paste(opts, collapse =  " ")
  out = dcm2nii(basedir, copy_files = FALSE,
                progdir = progdir, 
                opts = opts,
                verbose = FALSE, 
                dcm2niicmd = dcm2niicmd,
                ...)
  files = out$json_after
  if (length(files) > 0) {
    files = sapply(files, fix_sidecar)
  }
  return(files)
}

#' @rdname dcm2nii
#' @export
fix_sidecar = function(file) {
  xx = readLines(file)
  bad = !grepl("^\t", xx) & !grepl("^(\\{|\\})", xx)
  if (any(bad)) {
    xx = strsplit(paste(xx, collapse = " "), "\t")[[1]]
    xx = paste(xx, collapse = " ")
    writeLines(xx, con = file)
  }
  return(file)
}