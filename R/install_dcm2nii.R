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
#' @param progdir Installation directory for executable built
#' @examples
#' install_dir = tempdir()
#' install_dcm2nii(progdir = install_dir)
#' @importFrom utils download.file unzip
#' @importFrom httr stop_for_status GET write_disk progress
install_dcm2nii = function(lib.loc = NULL,
                           overwrite = FALSE,
                           from_source = FALSE,
                           progdir = NULL){
  
  sysname = tolower(Sys.info()["sysname"])
  app = switch(sysname, linux = "_linux", darwin = "")
  fname = paste0("dcm2niix", app)
  install_dir = progdir
  if (is.null(install_dir)) {
    install_dir = system.file(package = "dcm2niir",
                              lib.loc = lib.loc)
  }
  dcm2nii_files = file.path(install_dir, fname)
  
  
  if (!file.exists(dcm2nii_files) || overwrite) {
    if (from_source) {
      tdir = tempfile()
      if (!dir.exists(tdir)) {
        dir.create(tdir)
      }
      git = Sys.which("git")
      cmd = paste0(
        git, 
        " clone ", 
        "https://github.com/rordenlab/dcm2niix ",
        tdir)
      system(cmd)
      build_dir = file.path(tdir, "build")
      if (!dir.exists(build_dir)) {
        dir.create(build_dir)
      }      
      cmake = Sys.which("cmake")
      make = Sys.which("make")
      cmd = paste0(
        "cd ", build_dir, "; ", 
        cmake, " ..; ", 
        make)
      system(cmd)
      binary = file.path(build_dir, "bin", "dcm2niix")
      if (!file.exists(binary)) {
        stop("Build didn't result in a binary")
      }
      out_binary = file.path(
        install_dir,
        "dcm2niix")
      out_binary = paste0(out_binary, app)
      file.copy(binary, to = out_binary, overwrite = overwrite)
    } else {
      # url = "http://muschellij2.github.io/cttools/dcm2nii_files.zip"
      # url = paste0("https://github.com/muschellij2/cttools/",
      #              "raw/gh-pages/dcm2nii_files.zip")
      url = paste0("https://github.com/muschellij2/dcm2niir/raw", 
                   "/master/dcm2nii_files.zip")
      urlfile <- file.path(
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
  dcm2nii_files = file.path(install_dir, fname)
  
  return(file.exists(dcm2nii_files))
}


