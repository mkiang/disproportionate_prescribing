## 06_count_prescriptions_in_subset_rx.R ----
##
## This file goes through the subset data files and just counts the number
## of prescriptions in each subset. Uses the same process as the 02 code file.

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
fs::dir_create(here::here("result_objects", "mini_row_count"))

## Count the number of prescriptions in each subset file ----
## We're going to loop through each file and count the lines without importing.

## Get files
ndc_types <- return_drug_names()

## (1) Count rows in each file ----
if (!fs::file_exists(here::here("data", 
                                "descriptives",
                                "descriptives_num_rows_mini_rx.csv"))) {
    cl <- parallel::makeForkCluster(
        nnodes = parallel::detectCores() - 2,
        outfile = here::here("result_objects", "logs", "count_lines_mini_rx.txt")
    )
    doParallel::registerDoParallel(cl)
    
    row_counts <- NULL
    for (ndc in ndc_types) {
        mini_files <-
            sample(fs::dir_ls(here::here("data_private_mini", "rx", ndc)))
        
        row_counts_sub <-
            foreach::foreach(
                f = mini_files,
                .inorder = FALSE,
                .combine = dplyr::bind_rows
            ) %dopar% {
                count_rows(f_path = f) %>%
                    extract_qtr_year() %>%
                    dplyr::mutate(ndc_type = ndc) 
            }
        row_counts <- dplyr::bind_rows(row_counts, row_counts_sub)
    }
    parallel::stopCluster(cl)
    
    readr::write_csv(
        row_counts %>% 
            dplyr::arrange(ndc_type, year, quarter),
        here::here(
            "data",
            "descriptives",
            "descriptives_num_rows_mini_rx.csv"
        )
    )
}
