## 16_tabulate_ntile_cdc_guidelines_for_top_ntile_providers.R ----
##
## In 2016, the CDC released guidelines for the treatment of chronic pain and
## opioid prescriptions. Using those guidelines as a reference, we calculate
## the proportion of physicians in each centile that were above or below
## different thresholds. Note that the majority of our years were *before*
## the release of the guidelines and not all these prescriptions are chronic
## pain. We are only trying to describe provider prescribing patterns based
## on established recommendations and not establish clinical appropriateness.

## Imports ----
library(tidyverse)
library(here)
library(foreach)
library(doParallel)
library(parallel)
library(fs)
source(here::here("code", "utils.R"))

## Load MME table
mme_convers <- load_mme_conversion()

## For opioids and schedule ii opioids, see what proportion of each ntile
## is prescribing higher than the CDC guideline for chronic pain.
for (ndc_x in c("opioids", "schii_opioids")) {
    # print(ndc_x)
    if (!fs::dir_exists(here::here("result_objects", "ntile_guidelines"))) {
        fs::dir_create(here::here("result_objects", "ntile_guidelines"))
    }
    f_name <-
        here::here(
            "result_objects",
            "ntile_guidelines",
            sprintf("%s_ntile_guidelines.RDS", ndc_x)
        )
    
    if (!fs::file_exists(f_name)) {
        doParallel::registerDoParallel(cores = 8)
        results <- foreach::foreach(
            year_x = 2017:2003,
            .inorder = FALSE,
            .combine = dplyr::bind_rows
        ) %dopar% {
            orig_df <- import_mini_rx_data(year_x, ndc_x)
            tabulate_cdc_guidelines(orig_df, year_x, ndc_x)
        }
        doParallel::stopImplicitCluster()
        saveRDS(results, f_name, compress = "xz")
    }
}

## Gather all intermediate files into a single file and save in data since
## this is not identifiable.
if (!fs::file_exists(here::here("data", "ntile_cdc_guidelines.csv"))) {
    all_results <-
        purrr::map_df(.x = fs::dir_ls(here::here(
            "result_objects", "ntile_guidelines"
        )),
        .f = ~ readRDS(.x))
    readr::write_csv(
        all_results %>%
            dplyr::filter(n_patients > 10, n_deanpi > 10),
        here::here("data", "ntile_cdc_guidelines.csv")
    )
}
