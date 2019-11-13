## 00_install_necessary_packages.R ----
##
## Our Google Cloud Compute VM is based on a docker image with minimal
## R packages so we need to re-run this every time the VM restarts. This
## script just redownloads the latest version of all packages we don't already
## have.
##
## NOTE: This script is for your convenience but is *not* best practices. You
## should manually install each package (defined at the top of each R file
## that requires it) yourself to make sure you know what's going on. If using
## RStudio 1.2+. the code editor will bring up a warning to let you know
## when there is a package in the code that you do not have installed.

## To set up a GCP instance, you can use something like. Note that you should
## change the cpus and memory accordingly.
# library(googleComputeEngineR)
# vm <- googleComputeEngineR::gce_vm(
#   template = "rstudio",
#   name = "mvk-rstudio-new",
#   username = "mkiang",
#   password = "mkiangmkiang",
#   cpus = 24,
#   memory = 24 * 256 * 4 * 8,
#   disk_size_gb = 2048
# )

req_packages <-
  c(
    "tidyverse",
    "lubridate",
    "here",
    "fs",
    "readxl",
    "googleCloudStorageR",
    "foreach",
    "doParallel",
    "ineq",
    "janitor",
    "R.utils",
    "remotes",
    "DescTools",
    "ggalluvial",
    "kableExtra",
    "vroom",
    "remotes",
    "GGally", 
    "colorspaces",
    "shinyWidgets"
  )

for (p in req_packages) {
  if (!require(p, character.only = TRUE)) {
    print(p)
    utils::install.packages(p,
                     repos = 'https://cloud.r-project.org/',
                     INSTALL_opts = c('--no-lock'))
  }
}

if (!require("narcan")) {
  remotes::install_github("mkiang/narcan")
}

## Authenticate from inside a google cloud compute instance
googleAuthR::gar_gce_auth()

## These are used for plotting so only uncomment if you plan on running
## the plotting scripts. Because they are not from CRAN, you may need to
## debug some dependency issues.

# if (!require("patchwork")) {
#     remotes::install_github("thomasp85/patchwork")
# }

## First time around, you may also need to set up your git
  # git config --global user.email "youremail@email.com"
  # git config --global user.name "Your Name"
