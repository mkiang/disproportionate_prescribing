## Script to download the data from OSF, extract it, and then copy it
## into the correct locations for everything to work.

## Constant
osf_file <- "https://osf.io/mbcw7/download"
temp_f <- tempfile(fileext = ".zip")

## Download the file ----
download.file(
    url = osf_file, 
    destfile = temp_f
)

## Unzip the file ----
unzip(temp_f, 
      exdir = ".")

## Copy the data files to the Shiny apps ----
source("./code/99_copy_files_to_shiny_app.R")
