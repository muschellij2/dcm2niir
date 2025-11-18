#' @title Simple run of DICOM 2 NIfTI Converter
#' @description Uses Chris Rorden's dcm2nii from 
#' http://www.mccauslandcenter.sc.edu/mricro/mricron/dcm2nii.html to convert
#' DICOM files to NIfTI format.  Should Need run \code{\link{install_dcm2nii}} before running.
#'
#' @param basedir (character) directory to get files
#' @param files (character) vector of files to segment.  Will override
#' \code{basedir} if used
#' @param copy_files (logical) Should files be copied to a temporary directory?
#' @param progdir (character) directory of bash scripts, no user input needed unless
#' binaries were installed elsewhere. Passed to \code{\link{dcm2nii_bin}}.
#' @param verbose (logical) print diagnostic printouts
#' @param dcm2niicmd (character) either "dcm2niix", "dcm2nii", or "dcm2nii_2009", which 
#' are different versions of dcm2nii. Passed to \code{\link{dcm2nii_bin}}.  
#' The default is `dcm2niix`, but `dcm2niix_feb2024` has updated features.
#' @param opts list of arguments to pass to \code{dcm2nii}.
#' @param ... arguments to be passed to \code{\link{system}}
#' @return List of result of \code{system} command, names of files before and after
#' conversion
#' @param merge_files Should files be merged, 
#' passed do \code{dcm2nii} options
#' @param unzip_files if \code{TRUE}, any file with \code{.gz} extension
#' will be unzipped
#' @param ignore_derived Should derived images be ignored,
#' passed do \code{dcm2nii} options
#' @param cleanup if \code{copy_files} is \code{TRUE}, should temporary files be deleted?
#' @export
#' @examples 
#' library(utils)
#' install_dir = tempdir()
#' sysname = tolower(Sys.info()["sysname"])
#' cmake = Sys.which("cmake")
#' if (file.exists(cmake) && sysname == "windows" && 
#' nzchar(Sys.getenv("APPVEYOR")) && FALSE) {
#' source_clone_dir = fs::path(Sys.getenv("APPVEYOR_BUILD_FOLDER"),
#' "inst", "dcm2niix_clone")
#' install_dcm2nii(
#' progdir = install_dir, 
#' overwrite = TRUE,
#' from_source = TRUE, 
#' verbose = 2,
#' source_clone_dir = source_clone_dir)
#' } else {
#' install_dcm2nii(progdir = install_dir)
#' }
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
#' dl = download.file(url = dcm_file, method = method, 
#' destfile = destfile, mode = "wb")
#' dl == 0
#' stopifnot(file.exists(destfile))
#' fs = file.size(destfile) 
#' fs
#' if (fs <= 2e5) {
#' dl = download.file(url = dcm_file, destfile = destfile, mode = "wb")
#' }
#' fs = file.size(destfile) 
#' fs
#' stopifnot(fs > 2e5)
#' list.files(tdir) 
#' dcm2niir::install_dcm2nii(progdir = install_dir)
#' res = dcm2niir::dcm2nii(basedir = tdir, verbose = 1)
#' stopifnot(res$result == 0)
#' res = dcm2niir::dcm2nii(files = destfile, verbose = 1)
#' destfile = R.utils::gzip(destfile)
#' res = dcm2niir::dcm2nii(files = destfile, verbose = 1, unzip_files = FALSE)
#' stopifnot(res$result != 0)
#' res = dcm2niir::dcm2nii(files = destfile, verbose = 1, unzip_files = TRUE)
#' stopifnot(res$result == 0)
dcm2nii <- function(
    basedir = ".", 
    files = NULL,
    copy_files = TRUE,
    progdir = system.file(package = "dcm2niir"), 
    verbose = TRUE, 
    dcm2niicmd = c("dcm2niix",  "dcm2niix_feb2024", "dcm2nii_2009", "dcm2nii"), 
    merge_files = FALSE,
    ignore_derived = FALSE,
    unzip_files = TRUE,
    opts = paste0(
      "-9 ",
      ifelse(ignore_derived, "-i y ", ""),
      ifelse(merge_files, " -m y ", ""),
      paste0(" -v ", as.numeric(verbose)),
      " -z y -f %p_%t_%s"),
    cleanup = FALSE,
    ...){  
  dcm2niicmd = dcm2nii_bin(
    progdir = progdir,
    dcm2niicmd = dcm2niicmd)
  dcm2niicmd = fs::fs_path(dcm2niicmd)
  if (!is.null(files) & !copy_files) {
    warning("copy_files is FALSE, but files are given, copy_files = TRUE")
    copy_files = TRUE
  }
  basedir = path.expand(basedir)
  dir_temp = NULL
  if (copy_files) {
    if (verbose) {
      message("#Copying Files\n")
    }
    dir_temp = tempfile()
    dir_temp = fs::fs_path(dir_temp)    
    dir.create(dir_temp)
    if (is.null(files)) {
      l = list.files(path = basedir, recursive = TRUE, all.files = TRUE,
                     full.names = TRUE)
      file.copy(from = l, to = dir_temp)
      if (unzip_files) {
        l = file.path(dir_temp, basename(l))
        l = l[ grepl(".gz$", tolower(l))]
        if (length(l) > 0) {
          sapply(l, R.utils::gunzip, remove = TRUE)
        }
      }      
    } else {
      outfiles = file.path(dir_temp, basename(files))
      file.copy(from = files, to = outfiles)
      files = outfiles
      if (unzip_files) {
        l = files[ grepl(".gz$", tolower(files))]
        if (length(l) > 0) {
          sapply(l, R.utils::gunzip, remove = TRUE)
        }
      }       
    }
    basedir = dir_temp
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
  args = list(...)
  if (is.character(res)) {
    if (attr(res, "status") != 1) {
      warning("Result indicated an error!  Please check results.")
    } else {
      if (res != 0) {
        warning("Result indicated an error!  Please check results.")
      }
    }
  }
  if (copy_files && as.logical(cleanup)) {
    if (verbose) {
      message("Cleaning up temporary files\n")
    }
    unlink(dir_temp, recursive = TRUE)
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
  dir_temp = NULL
  if (copy_files) {
    if (verbose) {
      message("#Copying Files\n")
    }
    dir_temp = tempfile()
    dir.create(dir_temp)
    file.copy(from = file, to = dir_temp)
    file = file.path(dir_temp, basename(file))
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