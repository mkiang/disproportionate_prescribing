## 19_tabulate_ntile_to_ntile_transactions.R ----
## 
## We want to understand the transactions between top prescribers and
## top-receiving patients. We group providers and patients in to centiles 1,
## 2, 3, 4, 5, and 6-100 and then map how the number of prescriptions and the
## total dosage flows btween these groups. 

## Imports ----
library(tidyverse)
library(here)
library(foreach)
library(doParallel)
library(parallel)
library(fs)
source(here::here("code", "utils.R"))

mme_convers <- load_mme_conversion()

## Loop through every drug type and calculate the ntile to ntile transactions
## across all years. Save intermediate files.
for (ndc_x in return_drug_names()) {
    print(ndc_x)
    if (!fs::dir_exists(here::here("result_objects", "ntile_transactions"))) {
        fs::dir_create(here::here("result_objects", "ntile_transactions"))
    }
    
    f_name <-
        here::here(
            "result_objects",
            "ntile_transactions",
            sprintf("%s_ntile_transactions.RDS", ndc_x)
        )
    
    if (fs::file_exists(f_name)) {
        next
    }
    
    doParallel::registerDoParallel(cores = 14)
    results <- foreach::foreach(
        year_x = 2017:2003,
        .inorder = FALSE,
        .combine = dplyr::bind_rows
    ) %dopar% {
        orig_df <- import_mini_rx_data(year_x, ndc_x)
        tabulate_ntile_transactions(orig_df, year_x, ndc_x)
    }
    doParallel::stopImplicitCluster()
    saveRDS(results, f_name, compress = "xz")
}

## Gather all intermediate files into a single file and save in data since
## this is not identifiable.
if (!fs::file_exists(here::here("data", "ntile_to_ntile_transactions.csv"))) {
    all_results <-
        purrr::map_df(.x = fs::dir_ls(here::here(
            "result_objects", "ntile_transactions"
        )),
        .f = ~ readRDS(.x))
    
    all_results <- all_results %>%
        dplyr::mutate(
            prescriptions = ifelse(prescriptions < 10, NA, prescriptions)
            )
    readr::write_csv(all_results,
                     here::here("data", "ntile_to_ntile_transactions.csv"))
}
