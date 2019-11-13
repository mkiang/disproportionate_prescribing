## 03_tabulate_providers_patients_in_full_rx.R ----
##
## We go through all the full Rx files to get the following quantities:
##      (1) number of unique providers per year
##      (2) number of unique patients per year
##      (3) number of unique providers across *all* years
##      (4) number of unique patients across *all* years
##
## Because files are in quarters, we need to extract and tabulate the patient
## and provider IDs by quarter, save the results. Then load up the results by
## year and combine the tabulations, saving the new aggregated result.

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(foreach)
library(doParallel)
source(here::here("code", "utils.R"))

## Make dirs ----
fs::dir_create(here::here("data", "descriptives"))
fs::dir_create(here::here("result_objects", "logs"))
tab_folder <- here::here("result_objects", "tabulations")
fs::dir_create(tab_folder)

## Helper ----
chunked_reader <- function(f) {
    ## Column selectors
    c_only <- readr::cols_only(Patid = "c",
                               Dea = "c",
                               Npi = "c")
    
    ## Chunked callback
    callback_f <- function(x, pos) {
        x %>% dplyr::select(dplyr::one_of("Patid", "Dea", "Npi"))
    }
    
    temp_df <- readr::read_csv_chunked(
        f,
        col_types = c_only,
        callback = readr::DataFrameCallback$new(callback_f),
        progress = FALSE
    )
    names(temp_df) <- tolower(names(temp_df))
    
    return(temp_df)
}


## Count the number of prescriptions in each full file ----
## Get file paths
full_files <-
    sample(fs::dir_ls(
        here::here("data_private", "rx"),
        recurse = TRUE,
        type = "file"
    ))

## Extract unique DEA/NPI and PATIDs from each file ----
## In order to get the number of unique providers and patients per *year*,
## we need to import and save the unique ones in each *quarter*, then
## combine all the quarters in any given year and tally up the unique values
## again (since some will repeat across quarters).
doParallel::registerDoParallel(cores = 12)
results <-
    foreach::foreach(f = full_files, .inorder = FALSE) %dopar% {
        ## Get year and quarter
        yr  <- extract_year(f)
        qtr <- extract_quarter(f)
        
        ## Make new file names for DEA and Patid
        f_new_pat <-
            sprintf("%s/patid_tabs_%iq%i.RDS", tab_folder, yr, qtr)
        f_new_dea <-
            sprintf("%s/dea_tabs_%iq%i.RDS", tab_folder, yr, qtr)
        
        ## Cheap logger
        sink(sprintf("./result_objects/logs/log_%s_tabs.txt", Sys.getpid()))
        print(sprintf("%s %s %s", yr, qtr, f_new_dea))
        
        if (any(!fs::file_exists(f_new_pat),
                !fs::file_exists(f_new_dea))) {
            ## Read it in
            temp_df <- chunked_reader(f)
            
            ## Clean it up
            temp_df <- temp_df %>%
                dplyr::mutate(dea = ifelse(is.na(dea), "NoID", dea),
                              npi = ifelse(is.na(npi), "NoID", npi)) %>%
                dplyr::transmute(patid = patid,
                                 dea_npi = paste0(dea, "_", npi))
            
            ## Tabulate unique counts
            patid_tab <- table(temp_df$patid, useNA = "always")
            dea_npi_tab <- table(temp_df$dea_npi, useNA = "always")
            
            rm(temp_df)
            gc2()
            
            ## Save
            saveRDS(patid_tab, f_new_pat, compress = "xz")
            saveRDS(dea_npi_tab, f_new_dea, compress = "xz")
            
            ## Clean up
            rm(patid_tab, dea_npi_tab)
            gc2()
        } else {
            sprintf("Skipping: %s", f)
        }
        
        sprintf("%i Q%i", yr, qtr)
        sink()
    }
doParallel::stopImplicitCluster()

## Combine quarter tables into yearly tables ----
## Now go back through all the files and combine them by year and
## across all years for the full prescription data
if (!fs::file_exists(here::here("result_objects",
                                "tabulations",
                                "yearly_partials.RDS")) &&
    !fs::file_exists(here::here(
        "data",
        "descriptives",
        "descriptives_deanpi_tabulations.csv"
    ))) {
    ## Make the annual ones first
    doParallel::registerDoParallel(cores = 8)
    
    results <-
        foreach::foreach(
            y = 2003:2017,
            .inorder = FALSE,
            .combine = dplyr::bind_rows
        ) %dopar% {
            ## Get DEA tables for each quarter and combine
            dea_T <-
                purrr::map(.x = paste0(sprintf("%s/dea_tabs_%iq", tab_folder, y),
                                       1:4,
                                       ".RDS"),
                           .f = ~ readRDS(.x)) %>%
                purrr::reduce(c) %>%
                combine_tabs(.)
            
            ## Get Patid tables for each quarter and combine
            pat_T <- purrr::map(.x = paste0(
                sprintf("%s/patid_tabs_%iq", tab_folder, y),
                1:4,
                ".RDS"
            ),
            .f = ~ readRDS(.x)) %>%
                purrr::reduce(c) %>%
                combine_tabs(.)
            
            ## Combine patients and providers into one
            dplyr::bind_rows(
                summarize_tabulation(dea_T) %>%
                    dplyr::mutate(year = y,
                                  type = "dea_npi"),
                summarize_tabulation(pat_T) %>%
                    dplyr::mutate(year = y,
                                  type = "patid")
            )
        }
    doParallel::stopImplicitCluster()
    
    ## Save it ----
    saveRDS(
        results,
        here::here("result_objects", "tabulations", "yearly_partials.RDS"),
        compress = "xz"
    )
}

## Now let's make one across *all* years for DEAs
if (!fs::file_exists(here::here("result_objects",
                                "tabulations",
                                "dea_all_years.RDS")) &&
    !fs::file_exists(here::here(
        "data",
        "descriptives",
        "descriptives_deanpi_tabulations.csv"
    ))) {
    doParallel::registerDoParallel(cores = 4)
    dea_T <-
        foreach::foreach(
            f = fs::dir_ls(tab_folder, regexp = "dea_tabs_"),
            .inorder = FALSE,
            .combine = c
        ) %dopar% {
            readRDS(f)
        }
    doParallel::stopImplicitCluster()
    
    dea_T <- combine_tabs(dea_T)
    
    ## Combine patients and providers into one
    results_dea_all_years <- summarize_tabulation(dea_T) %>%
        dplyr::mutate(year = 9999,
                      type = "dea_npi")
    
    ## Save it ----
    saveRDS(
        results_dea_all_years,
        here::here("result_objects",
                   "tabulations",
                   "dea_all_years.RDS"),
        compress = "xz"
    )
    rm(dea_T)
    gc2()
}

## Now let's make one across *all* years for patids
if (!fs::file_exists(here::here("result_objects",
                                "tabulations",
                                "patid_all_years.RDS"))) {
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
    
    ## Combine patients and providers into one
    results_patid_all_years <- summarize_tabulation(pat_T) %>%
        dplyr::mutate(year = 9999,
                      type = "patid")
    
    ## Save it ----
    saveRDS(
        results_patid_all_years,
        here::here("result_objects",
                   "tabulations",
                   "patid_all_years.RDS"),
        compress = "xz"
    )
}

## Load up all partial tabulations and combine ----
if (!fs::file_exists(here::here(
    "data",
    "descriptives",
    "descriptives_deanpi_tabulations.csv"
))) {
    results_yearly <-
        readRDS(here::here("result_objects", "tabulations", "yearly_partials.RDS"))
    results_dea_all_years <-
        readRDS(here::here("result_objects", "tabulations", "dea_all_years.RDS"))
    results_patid_all_years <-
        readRDS(here::here("result_objects", "tabulations", "patid_all_years.RDS"))
    results_full <- dplyr::bind_rows(results_yearly,
                                     results_dea_all_years,
                                     results_patid_all_years)
    
    ## Save it ----
    readr::write_csv(
        results_full %>%
            filter(n_rows > 10,
                   n_unique > 10),
        here::here(
            "data",
            "descriptives",
            "descriptives_deanpi_tabulations.csv"
        )
    )
}
