#' @title Install dcm2nii tools
#' @description Install dcm2nii to dcm2niir for conversion tools
#' @return NULL
#' @export
#' @param lib.loc a character vector with path names of R libraries.
#' Passed to \code{\link{system.file}}
#' @param overwrite Force reinstallation if one already present
#' @param from_source if this is TRUE, then \code{git} and \code{cmake}
#' are required and the current version is cloned from 
#' \url{https://github.com/rordenlab/dcm2niix}
#' @param jpeg install using JPEG utilities
#' @param progdir Installation directory for executable built
#' @param cmake_opts Additional options to pass to \code{cmake}, like 
#' \code{-DZLIB_IMPLEMENTATION=Cloudflare}
#' @param verbose print diagnostic messages
#' @param source_clone_dir experimental, where the file should be cloned if 
#' source install.  Do not use unless you know what you're doing.
#' @param git_url URL of \code{git} repository used for building from source
#' @examples
#' install_dir = tempdir()
#' install_dcm2nii(progdir = install_dir)
#' @importFrom utils download.file unzip
#' @importFrom fs path fs_path
#' @importFrom httr stop_for_status GET write_disk progress
install_dcm2nii = function(
  lib.loc = NULL,
  overwrite = FALSE,
  from_source = FALSE,
  jpeg = FALSE,
  progdir = NULL,
  cmake_opts = "", 
  verbose = TRUE,
  source_clone_dir = NULL,
  git_url = "https://github.com/rordenlab/dcm2niix"){
  
  sysname = tolower(Sys.info()["sysname"])
  app = switch(sysname, linux = "_linux", darwin = "")
  fname = paste0("dcm2niix", app)
  install_dir = progdir
  if (is.null(install_dir)) {
    install_dir = system.file(package = "dcm2niir",
                              lib.loc = lib.loc)
  } else {
    dir.create(install_dir, recursive = TRUE, showWarnings = FALSE)
  }
  dcm2nii_files = fs::path(install_dir, fname)
  
  
  if (!file.exists(dcm2nii_files) || overwrite) {
    if (from_source) {
      if (is.null(source_clone_dir)) {
        tdir = tempfile()
      } else {
        tdir = source_clone_dir
      }
      tdir = fs::fs_path(tdir)
      if (!dir.exists(tdir)) {
        dir.create(tdir)
      }
      sha = NULL
      if (requireNamespace("git2r", quietly = TRUE)) {
        repository = git2r::clone(git_url, local_path = tdir)
        sha = git2r::commits(repository)[[1]]$sha
      } else {
        git = Sys.which("git")
        cmd = paste0(
          git, 
          " clone ", 
          git_url,
          " ",
          tdir)
        system(cmd)
        cmd = paste0(
          git, 
          " -C ", tdir, " log -1")
        sha = system(cmd, intern = TRUE)
        sha = sub("commit", "", sha[[1]])
        sha = trimws(sha)
      }
      build_dir = fs::path(tdir, "build")
      if (!dir.exists(build_dir)) {
        dir.create(build_dir)
      }      
      cmake = Sys.which("cmake")
      make = Sys.which("make")
      # if (sysname == "windows") {
      #   make = Sys.which("mingw32-make")
      # }
      cmake = fs::fs_path(cmake)
      make = fs::fs_path(make)
      if (verbose) {
        message(paste0("cmake is ", cmake))
        message(paste0("make is ", make))
      }
      owd = getwd()
      on.exit({
        setwd(owd)
      })
      setwd(build_dir)
      cmd = paste0(
        # "cd ", build_dir, "; ", 
        cmake, 
        # -DUSE_JPEGLS=ON
        ifelse(jpeg, " -DUSE_OPENJPEG=ON ", ""), 
        " ", cmake_opts, 
        " ", tdir)
      if (verbose) {
        message(paste0("cmd is ", cmd))
      }
      if (verbose > 1) {
        message("listing current directory")
        print(list.files())
      }
      system(cmd)
      cmd = make
      if (verbose) {
        message(paste0("cmd is ", cmd))
      }         
      system(cmd)
      
      setwd(owd)
      binary = fs::path(build_dir, "bin", "dcm2niix")
      if (!file.exists(binary)) {
        stop("Build didn't result in a binary")
      }
      out_binary = fs::path(
        install_dir,
        "dcm2niix")
      out_binary = paste0(out_binary, app)
      file.copy(binary, to = out_binary, overwrite = overwrite)
      if (verbose) {
        if (!is.null(sha)) {
          message(paste0("sha is ", sha))
        }
      }      
    } else {
      # url = "http://muschellij2.github.io/cttools/dcm2nii_files.zip"
      # url = paste0("https://github.com/muschellij2/cttools/",
      #              "raw/gh-pages/dcm2nii_files.zip")
      url = paste0("https://github.com/muschellij2/dcm2niir/raw", 
                   "/gh-pages/dcm2nii_files.zip")
      urlfile <- fs::path(
        install_dir,
        "dcm2nii_files.zip")    
      # download.file(url, urlfile)
      req = httr::GET(
        url, 
        httr::write_disk(path = urlfile, overwrite = overwrite),
        httr::progress())  
      httr::stop_for_status(req)
      files = unzip(urlfile,
                    exdir = install_dir,
                    junkpaths = TRUE)
      for (ifile in files) system(sprintf("chmod +x %s", ifile))
      x = file.remove(urlfile)
      rm(x)
    }
  }
  dcm2nii_files = fs::path(install_dir, fname)
  if (verbose > 1) {
    message("dcm2nii_files is ", dcm2nii_files)  
  }
  
  return(file.exists(dcm2nii_files))
}


