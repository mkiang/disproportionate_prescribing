## 02_count_prescriptions_in_full_rx.R ----
##
## This script just goes through all the full Rx files and gets the number of
## rows in each file (i.e., number of prescriptions). These files are very
## large and this seems to be the fastest way to do it outside of using
## bash directly. 

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(foreach)
library(doParallel)
library(parallel)
source(here::here("code", "utils.R"))

## Make dirs ----
fs::dir_create(here::here("data", "descriptives"))
fs::dir_create(here::here("result_objects", "logs"))
fs::dir_create(here::here("result_objects", "full_row_count"))

## Count the number of prescriptions in each FULL file ----
if (!fs::file_exists(
    here::here("data", "descriptives", "descriptives_num_rows_full_rx.csv")
    )) {
    ## Get file paths
    full_files <-
        sample(fs::dir_ls(here::here("data_private", "rx")))
    
    ## Count rows in each file and saves an intermediate file
    if (!fs::file_exists(here::here(
        "data",
        "descriptives",
        "descriptives_num_rows_full_rx.csv"
    ))) {
        ## Note: seems disk i/o is  limiting factor -- more cores is not helpful.
        doParallel::registerDoParallel(cores = 8)
        counts_holder <-
            foreach::foreach(f = full_files, .inorder = FALSE) %dopar% {
                ## Cheap logging
                sink(
                    sprintf(
                        "./result_objects/logs/log_%s_linecount.txt",
                        Sys.getpid()
                    ),
                    append = TRUE
                )
                print(sprintf("%s", f))
                print(Sys.time())
                
                ## Intermediate file
                f_new <- here::here("result_objects",
                                    "full_row_count",
                                    sprintf("rows_%s", basename(f)))
                
                if (!fs::file_exists(f_new)) {
                    res <- count_rows(f_path = f) %>%
                        extract_qtr_year()
                    readr::write_csv(res, f_new)
                    print(res)
                } else {
                    sprintf("Skipping: %s", f)
                }
                
                sink()
            }
        doParallel::stopImplicitCluster()
    }
    
    ## Collect intermediate files ----
    row_counts <-
        purrr::map_df(
            .x = fs::dir_ls(here::here("result_objects",
                                       "full_row_count"),
                            glob = "*.csv"),
            .f = ~ readr::read_csv(.x)
        )
    
    ## Save ----
    readr::write_csv(
        row_counts,
        here::here(
            "data",
            "descriptives",
            "descriptives_num_rows_full_rx.csv"
        )
    )
}
