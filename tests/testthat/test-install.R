context("installing from source")

test_that("install_dcm2nii", {
  install_dir = tempfile()
  dir.create(install_dir, showWarnings = FALSE)
  install_dcm2nii(progdir = install_dir)
  expect_true(install_dcm2nii(progdir = install_dir))
})


test_that("install_dcm2nii source", {
  testthat::skip_on_appveyor()
  install_dir = tempfile()
  dir.create(install_dir, showWarnings = FALSE)
  cmake = Sys.which("cmake")
  make = Sys.which("make")
  message("make is ", make)
  if (file.exists(cmake)) {
    install_dcm2nii(
      progdir = install_dir, 
      from_source = TRUE,
      overwrite = TRUE,
      verbose = 2)
    expect_true(install_dcm2nii(progdir = install_dir))
  }
})
