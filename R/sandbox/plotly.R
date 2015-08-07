.pkgs = c("gdata", "reshape2", "RJSONIO", "RCurl", "devtools")

# Install required packages from CRAN (if not)
.inst <- .pkgs %in% installed.packages()
if(length(.pkgs[!.inst]) > 0) install.packages(.pkgs[!.inst])

# Install plotly package from Github
if(! "plotly" %in% installed.packages()) {
  require(devtools)
  devtools::install_github("plotly/R-api")
}

# Load package
library(plotly)
