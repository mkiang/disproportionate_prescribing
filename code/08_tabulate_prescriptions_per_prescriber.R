## 08_tabulate_prescriptions_per_prescriber.R ----
## 
## For each year, we want the DEA_NPI id of every provider in our dataset 
## (across all NDCs). One sensitivity we will be doing is trimming providers
## based on how many *total* prescriptions they write. That is, is the 
## inequality in prescribing driven by having a lot of providers every year
## who only prescribe 1 or 2 prescriptions? To do that, we need to identify
## providers in our data set and then we need to count the number of *total*
## prescriptions they've written in each year across all drugs. 

## Imports ----
library(tidyverse)
library(fs)
library(here)
library(purrr)
library(foreach)
library(doParallel)
source(here::here("code", "utils.R"))

##Folders ----
fs::dir_create(here::here("result_objects", "active_prescribers"))

## Helper function ----
return_unique_dea <-
    function(year,
             mini_tab_folder = here::here("result_objects",
                                          "tabulations_mini")) {
        dea_tab_paths <- fs::dir_ls(
            mini_tab_folder,
            recurse = TRUE,
            regexp = sprintf("\\<dea_tabs_%iq[1-4]{1}.RDS\\>", year)
        )
        
        purrr::map(.x = dea_tab_paths,
                   .f = ~ readRDS(.x) %>%
                       names(.)) %>%
            purrr::reduce(c) %>%
            unique(.)
    }

chunked_reader_dea <- function(f) {
    ## Column selectors
    c_only <- readr::cols_only(Patid = "c",
                        Dea = "c",
                        Npi = "c",
                        Days_Sup = "c")
    
    ## Chunked callback
    callback_f <- function(x, pos) {
        x %>% 
            dplyr::select(dplyr::one_of("Patid", "Dea", "Npi", "Days_Sup")) %>%
            dplyr::mutate(Dea = ifelse(is.na(Dea), "NoID", Dea),
                          Npi = ifelse(is.na(Npi), "NoID", Npi)) %>%
            dplyr::filter(as.numeric(Days_Sup) > 0) %>% 
            dplyr::transmute(patid = Patid,
                             dea_npi = paste0(Dea, "_", Npi)) %>% 
            dplyr::filter(dea_npi != "NoID_NoID") 
    }
    
    temp_df <- readr::read_csv_chunked(
        f,
        col_types = c_only,
        callback = readr::DataFrameCallback$new(callback_f),
        progress = FALSE, 
        chunk_size = 50000)
    
    return(temp_df)
}

if (!fs::file_exists(here::here("data_private", "unique_deas_by_year.RDS"))) {
    ## Loop through all years and make a list of unique DEAs per year
    doParallel::registerDoParallel(cores = 15)
    dea_holder <- foreach::foreach(y = 2003:2017, .inorder = FALSE) %dopar% {
            x <- list()
            x[[paste0("y", y)]] <- return_unique_dea(y)
            x
    }
    doParallel::stopImplicitCluster()
    
    dea_holder <- unlist(dea_holder, recursive = FALSE)
    
    ## Save it ----
    saveRDS(dea_holder,
            here::here("data_private", "unique_deas_by_year.RDS"),
            compress = "xz")
}

## Load up DEA dictionary ----
dea_holder <- readRDS(here::here("data_private", "unique_deas_by_year.RDS"))

## For each full file, subset to just prescribers who are in our sample ----
## Note that it is perfectly possible that nearly every provider in our
## sample is also in the full data and thus this is a wasted step, but for now
## we want to keep the code flexible.
doParallel::registerDoParallel(cores = 16)
holder <-
    foreach::foreach(y = sample(2003:2017), .inorder = FALSE) %dopar% {
        rx_files <- fs::dir_ls(here::here("data_private", "rx"),
                               regexp = sprintf("zip5_r%sq[1-4]{1}.csv\\>", y))
        
        f_new <-
            here::here(
                "result_objects",
                "active_prescribers",
                sprintf("active_prescribers_%s.RDS", y)
            )
        
        ## Cheap logging
        sink(
            sprintf(
                "./result_objects/logs/log_%s_active_prescribers.txt",
                Sys.getpid()
            ),
            append = TRUE
        )
        print(sprintf("%s", y))
        print(Sys.time())
        
        if (!fs::file_exists(f_new)) {
            temp_df <- purrr::map_df(.x = rx_files,
                              .f = ~ chunked_reader_dea(.x))
            
            temp_df <- temp_df %>%
                remove_miscoded_dea_npi() %>%
                dplyr::filter(dea_npi %in% dea_holder[[paste0("y", y)]]) %>%
                dplyr::group_by(dea_npi) %>%
                dplyr::summarize(n_prescriptions = dplyr::n(),
                          n_patients = dplyr::n_distinct(patid)) %>%
                dplyr::mutate(year = y) %>%
                dplyr::ungroup()
            
            saveRDS(temp_df, f_new, compress = "xz")
        } else {
            sprintf("Skipping: %s", y)
        }
        rm(temp_df); gc2()
        sink()
        return(y)
    }
doParallel::stopImplicitCluster()

## Combine quarters into years ----
## We want a dataframe where every row is a provider-year and the columns are
## dea_npi, year, number of prescriptions, number of patients
if (!fs::file_exists( here::here("data_private", "active_prescribers.RDS"))) {
    f_list <- fs::dir_ls(here::here("result_objects",
                                    "active_prescribers"),
                         glob = "*.RDS")
    
    
    doParallel::registerDoParallel(cores = 16)
    all_active_prescribers <-
        foreach::foreach(f = f_list, .inorder = FALSE) %dopar% {
            readRDS(f) 
        }
    doParallel::stopImplicitCluster()
    all_active_prescribers <- dplyr::bind_rows(all_active_prescribers)
    
    saveRDS(all_active_prescribers, 
            here::here("data_private", "active_prescribers.RDS"),
            compress = "xz")
}
