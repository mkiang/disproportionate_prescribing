## 07_tabulate_providers_patients_in_subset_rx.R ----
##
## This file goes through the subset data and tabulates different things of
## interest on the prescription data. Things we want are:
##      (1) number of prescriptions per file
##      (2) number of unique providers per year
##      (3) number of unique patients per year
##      (4) number of unique providers across all years
##      (5) number of unique patients across all years

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(foreach)
library(doParallel)
library(parallel)
source(here::here("code", "utils.R"))

## Constants ----
ndc_types <- return_drug_names()

## Make dirs ----
fs::dir_create(here::here("data", "descriptives"))
fs::dir_create(here::here("result_objects", "tabulations_mini", ndc_types))

## Extract unique DEA/NPI and PATIDs from each file ----
## In order to get the number of unique providers and patients per *year*,
## we need to import and save the unique ones in each *quarter*, then
## combine all the quarters in any given year and tally up the unique values
## again (since some will repeat across quarters).

## Make dirs
for (ndc in ndc_types) {
    tab_folder <- here::here("result_objects", "tabulations_mini", ndc)
    mini_files <- fs::dir_ls(here::here("data_private_mini", "rx", ndc))
    
    cl <- parallel::makeForkCluster(nnodes = 12)
    doParallel::registerDoParallel(cl)
    results <-
        foreach::foreach(f = sample(mini_files),
                .inorder = FALSE,
                .combine = c) %dopar% {
                    yr  <- extract_year(f)
                    qtr <- extract_quarter(f)
                    
                    f_new_pat <- sprintf("%s/patid_tabs_%iq%i.RDS",
                                         tab_folder, yr, qtr)
                    f_new_dea <- sprintf("%s/dea_tabs_%iq%i.RDS",
                                         tab_folder, yr, qtr)
                    
                    sink(sprintf("./result_objects/logs/log_%s_mini_tabs.txt",
                                 Sys.getpid()), 
                         append = TRUE)
                    print(sprintf("%s %s %s", yr, qtr, f_new_dea))
                    
                    if (any(!file.exists(f_new_pat),
                            !file.exists(f_new_dea))) {
                        temp_df <- readr::read_csv(
                            f,
                            col_types = readr::cols_only(
                                Patid = "c",
                                Dea = "c",
                                Npi = "c",
                                patid = "c",
                                dea = "c",
                                npi = "c"
                            ),
                            progress = FALSE
                        )
                        names(temp_df) <- tolower(names(temp_df))
                        
                        temp_df <- temp_df %>%
                            dplyr::mutate(
                                dea = ifelse(is.na(dea), "NoID", dea),
                                npi = ifelse(is.na(npi), "NoID", npi)
                            ) %>%
                            dplyr::transmute(patid = patid,
                                      dea_npi = paste0(dea, "_", npi))
                        
                        patid_tab <- table(temp_df$patid, useNA = "always")
                        dea_npi_tab <- table(temp_df$dea_npi, useNA = "always")
                        
                        rm(temp_df); gc2()
                        
                        saveRDS(patid_tab, f_new_pat, compress = "xz")
                        saveRDS(dea_npi_tab, f_new_dea, compress = "xz")
                        
                        rm(patid_tab, dea_npi_tab); gc2()
                    }
                    sprintf("%i Q%i", yr, qtr)
                    sink()
                }
    parallel::stopCluster(cl)
}

## Combine quarter tables into yearly tables ----
## Now go back through all the files and combine them by year and
## across all years for the full prescription data
if (!fs::file_exists(here::here(
    "data",
    "descriptives",
    "descriptives_deanpi_tabulations_mini_files.csv"
))) {
    for (ndc in ndc_types) {
        tab_folder <- here::here("result_objects", "tabulations_mini", ndc)
        
        if (!fs::file_exists(here::here(
            "result_objects",
            "tabulations_mini",
            ndc,
            "yearly_partials.RDS"
        ))) {
            ## Make the annual ones first
            cl <- parallel::makeForkCluster(nnodes = 12)
            doParallel::registerDoParallel(cl)
            
            results <-
                foreach::foreach(
                    y = 2003:2017,
                    .inorder = FALSE,
                    .combine = dplyr::bind_rows
                ) %dopar% {
                    ## Get DEA tables for each quarter and combine
                    dea_T <-
                        purrr::map(.x = paste0(
                            sprintf("%s/dea_tabs_%iq", tab_folder, y),
                            1:4,
                            ".RDS"
                        ),
                        .f = ~ readRDS(.x)) %>%
                        purrr::reduce(c) %>%
                        combine_tabs(.)
                    
                    ## Get Patid tables for each quarter and combine
                    pat_T <-
                        purrr::map(.x = paste0(
                            sprintf("%s/patid_tabs_%iq", tab_folder, y),
                            1:4,
                            ".RDS"
                        ),
                        .f = ~ readRDS(.x)) %>%
                        purrr::reduce(c) %>%
                        combine_tabs(.)
                    
                    ## Combine
                    dplyr::bind_rows(
                        summarize_tabulation(dea_T) %>%
                            dplyr::mutate(
                                ndc_type = ndc,
                                year = y,
                                type = "dea_npi"
                            ),
                        summarize_tabulation(pat_T) %>%
                            dplyr::mutate(
                                ndc_type = ndc,
                                year = y,
                                type = "patid"
                            )
                    )
                }
            parallel::stopCluster(cl)
            
            saveRDS(
                results %>% dplyr::arrange(ndc_type, type, year),
                here::here(
                    "result_objects",
                    "tabulations_mini",
                    ndc,
                    "yearly_partials.RDS"
                ),
                compress = "xz"
            )
        }
        
        ## (4-5) Now let's make one across *all* years for DEAs
        if (!fs::file_exists(here::here(
            "result_objects",
            "tabulations_mini",
            ndc,
            "dea_all_years.RDS"
        ))) {
            cl <- parallel::makeForkCluster(nnodes = 12)
            doParallel::registerDoParallel(cl)
            dea_T <-
                foreach::foreach(
                    f = fs::dir_ls(tab_folder, regexp = "dea_tabs_"),
                    .inorder = FALSE,
                    .combine = c
                ) %dopar% {
                    readRDS(f)
                }
            parallel::stopCluster(cl)
            
            dea_T <- combine_tabs(dea_T)
            
            results_dea_all_years <- summarize_tabulation(dea_T) %>%
                dplyr::mutate(ndc_type = ndc,
                       year = 9999,
                       type = "dea_npi")
            
            saveRDS(
                results_dea_all_years,
                here::here(
                    "result_objects",
                    "tabulations_mini",
                    ndc,
                    "dea_all_years.RDS"
                ),
                compress = "xz"
            )
            rm(dea_T); gc2()
        }
        
        ## Now let's make one across *all* years for patids
        if (!fs::file_exists(here::here(
            "result_objects",
            "tabulations_mini",
            ndc,
            "patid_all_years.RDS"
        ))) {
            cl <- parallel::makeForkCluster(nnodes = 12)
            doParallel::registerDoParallel(cl)
            pat_T <-
                foreach::foreach(
                    f = fs::dir_ls(tab_folder, regexp = "patid_tabs_"),
                    .inorder = FALSE,
                    .combine = c
                ) %dopar% {
                    readRDS(f)
                }
            parallel::stopCluster(cl)
            
            pat_T <- combine_tabs(pat_T)
            
            results_patid_all_years <-
                summarize_tabulation(pat_T) %>%
                dplyr::mutate(ndc_type = ndc,
                       year = 9999,
                       type = "patid")
            
            saveRDS(
                results_patid_all_years,
                here::here(
                    "result_objects",
                    "tabulations_mini",
                    ndc,
                    "patid_all_years.RDS"
                ),
                compress = "xz"
            )
        }
    }
    
    mini_tabs <- here::here("result_objects", "tabulations_mini")
    results_full <- dplyr::bind_rows(
        purrr::map(
            .x = fs::dir_ls(mini_tabs,
                        recurse = TRUE,
                        regexp = "yearly_partials.RDS"),
            .f = ~ readRDS(.x)
        ),
        purrr::map(
            .x = fs::dir_ls(mini_tabs,
                        recurse = TRUE,
                        regexp = "dea_all_years.RDS"),
            .f = ~ readRDS(.x)
        ),
        purrr::map(
            .x = fs::dir_ls(mini_tabs,
                        recurse = TRUE,
                        regexp = "patid_all_years.RDS"),
            .f = ~ readRDS(.x)
        ),
    )
    readr::write_csv(
        results_full %>% 
            dplyr::filter(n_rows > 10, 
                   n_unique > 10),
        here::here(
            "data",
            "descriptives",
            "descriptives_deanpi_tabulations_mini_files.csv"
        )
    )
}
