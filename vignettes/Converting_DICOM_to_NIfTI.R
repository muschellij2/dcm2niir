## ----data_dl-------------------------------------------------------------
library(utils)
# dcm_file = "ftp://medical.nema.org/medical/Dicom/DataSets/WG30/MGH/MR/MouseBrainSiemens15T_20150410/Converted/DICOM/mghmousetoenhancedmr_T1w_pre.dcm"
dcm_file = "http://johnmuschelli.com/dcm2niir/mghmousetoenhancedmr_T1w_pre.dcm"
tdir = tempfile()
dir.create(tdir)
destfile = tempfile(fileext = ".dcm", tmpdir = tdir)
ci = Sys.getenv("CI")
method = ifelse(ci == "", "auto", "curl")
dl = download.file(url = dcm_file, method = method, destfile = destfile)
dl == 0
file.exists(destfile)

## ----installer-----------------------------------------------------------
library(dcm2niir)
install_dcm2nii()

## ----data_convert--------------------------------------------------------
res = dcm2niir::dcm2nii(basedir = tdir)
res

## ----data_check----------------------------------------------------------
checked = check_dcm2nii(res)
checked
file.exists(checked)

