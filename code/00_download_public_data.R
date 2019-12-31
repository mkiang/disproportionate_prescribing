## Script to download the data from OSF, extract it, and then copy it
## into the correct locations for everything to work.
## 
## See: https://osf.io/8hzc3/ 

download_from_osf <- function(osf_file = "https://osf.io/mbcw7/download",
                              force_download = FALSE) {
    if (NROW(list.files("./data", recursive = TRUE)) < 13700 |
        force_download) {
        download.file(url = osf_file,
                      destfile = temp_f)
        
        ## Unzip the file ----
        unzip(temp_f,
              exdir = ".")
    } else {
        print(
            paste(
                "It appears data has already been downloaded.",
                "Use force_download = TRUE to overwrite existing,",
                "files in ./data"
            )
        )
    }
}

## Download data files and then copy ----
download_from_osf()
source("./code/99_copy_files_to_shiny_app.R")
