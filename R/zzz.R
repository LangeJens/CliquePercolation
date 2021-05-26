.onAttach <- function(libname, pkgname) {
  version <- read.dcf(file = system.file("DESCRIPTION", package = pkgname),
                      fields = "Version")
  packageStartupMessage("This is ", paste(pkgname, version))
  packageStartupMessage("See the Vignette for detailed information by using 'vignette(\"CliquePercolation\")'.")
  packageStartupMessage("For questions and issues, please see github.com/LangeJens/CliquePercolation/.")
}