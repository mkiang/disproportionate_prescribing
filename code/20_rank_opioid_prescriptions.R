## 20_rank_opioid_prescriptions.R ----
##
## In each year, among *all* providers and the top 1% of providers, which
## opioids are the most common? We collapse opioids based on their
## generic names (fixing typos and minor differences) and then rank them
## based on MMEs, number of providers, number of unique patients, number of
## prescriptions, etc.

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(foreach)
library(doParallel)
source(here::here("code", "utils.R"))

## Import data ----
mme_convers <- load_mme_conversion() %>%
    dplyr::mutate(generic_name = snakecase::to_title_case(generic_name,
                                                          parsing_option = 0))

## Loop through each year and rank the prescriptions ----
## We group prescriptions based on their generic name and ranked based on
## MMEs, number of unique providers, number of unique patients, number
## of prescriptions.
if (!fs::file_exists(here::here("data", "opioid_prescription_ranks.csv"))) {
    doParallel::registerDoParallel(cores = 14)
    ranking_results <-
        foreach::foreach(
            year_x = 2003:2017,
            .inorder = FALSE,
            .combine = dplyr::bind_rows
        ) %dopar% {
            temp_df <- import_mini_rx_data(year_x, "opioids")
            
            ## Calculate MMEs
            temp_df <- temp_df %>%
                calculate_mmes(
                    df = .,
                    mme_convers_df = mme_convers,
                    ndc_type_x = "opioids"
                ) %>%
                dplyr::filter(days_sup > 0,
                              mme > 0)
            
            top_ntile_provs <- temp_df %>%
                summarize_mmes() %>%
                dplyr::mutate(n_tile = 101 - dplyr::ntile(mme, 100)) %>%
                dplyr::filter(n_tile == 1) %>%
                dplyr::select(dea_npi, n_tile)
            
            temp_df <- temp_df %>%
                dplyr::left_join(top_ntile_provs,
                                 by = "dea_npi")
            
            dplyr::bind_rows(
                temp_df %>%
                    summarize_rank_by_drug(., year_x = year_x) %>%
                    dplyr::mutate(subset = "all_ntiles"),
                temp_df %>%
                    dplyr::filter(n_tile == 1) %>%
                    summarize_rank_by_drug(., year_x = year_x) %>%
                    dplyr::mutate(subset = "top_ntile")
            )
        }
    doParallel::stopImplicitCluster()
    
    ## Remove calculations based on fewer than 10 patients or providers in
    ## accordance with our DUA. Keep the relative ranks.
    ranking_results <- ranking_results %>%
        dplyr::mutate(flag = (n_deanpi < 10 | n_patid < 10)) %>%
        dplyr::mutate(
            n_prescriptions = ifelse(flag, NA, n_prescriptions),
            n_deanpi = ifelse(flag, NA, n_deanpi),
            n_patid = ifelse(flag, NA, n_patid),
            days_sup = ifelse(flag, NA, days_sup),
            quantity = ifelse(flag, NA, quantity),
            mme = ifelse(flag, NA, mme)
        ) %>%
        dplyr::select(-flag)
    
    ## Save it ----
    readr::write_csv(ranking_results,
                     here::here("data", "opioid_prescription_ranks.csv"))
}
