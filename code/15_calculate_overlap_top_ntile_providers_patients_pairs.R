## 15_calculate_overlap_top_ntile_providers_patients_pairs.R ----
## 
## For all drugs, calculate the overlap in providers, patients, and provider-
## patient pairs between years after 2008. Note that we exclude earlier years
## because provider-specific data before then are sparse. Optum may fix this
## in the future, in which case, this code should be flexible enough to 
## re-run with previous years. We calculate both the literal overlap and
## a proper overlap measure (Jaccard). 

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(foreach)
library(doParallel)
source(here::here("code", "utils.R"))

## Create folders ----
fs::dir_create(here::here("result_objects", "overlap"))

## Get data ----
prov_ntiles <-
    readRDS(here::here("data_private", "top_ntile_providers.RDS"))
pt_ntiles <-
    readRDS(here::here("data_private", "top_ntile_patients.RDS"))
pair_ntiles <-
    readRDS(here::here("data_private", "top_ntile_pairs.RDS")) %>%
    dplyr::mutate(pair_id = paste0(patid, "_", dea_npi))

## Make a grid of all years (after 2008), trimming weights, states, provider
## types, etc. 
param_grid <- expand.grid(
    year_i = unique(prov_ntiles$year),
    year_j = unique(prov_ntiles$year),
    w_x = unique(prov_ntiles$w),
    state_x = unique(prov_ntiles$prov_state),
    cat_x = unique(prov_ntiles$prov_cat),
    stringsAsFactors = FALSE
) %>%
    dplyr::filter(year_i != year_j,
                  year_i >= 2008,
                  year_j >= 2008)

## Loop through each drug type and calculate the overlap ----
for (n_type in return_drug_names()) {
    if (!fs::file_exists(here::here(
        "result_objects",
        "overlap",
        sprintf("overlap_%s.RDS", n_type)
    ))) {
        
        print(n_type)
        
        doParallel::registerDoParallel(cores = 12)
        ## Providers
        jaccard_df <- foreach::foreach(
            i = 1:NROW(param_grid),
            .inorder = FALSE,
            .combine = dplyr::bind_rows
        ) %dopar% {
            year_i <- param_grid$year_i[i]
            year_j <- param_grid$year_j[i]
            w_x <- param_grid$w_x[i]
            state_x <- param_grid$state_x[i]
            cat_x <- param_grid$cat_x[i]
            
            ## Set up log file
            sink(sprintf("./result_objects/logs/log_%s.txt",
                         Sys.getpid()),
                 append = TRUE)
            print(sprintf("%s %s %s %s", year_i, year_j, n_type, state_x))
            
            x <- dplyr::bind_rows(
                ## Provider overlap
                return_overlap(
                    prov_ntiles,
                    year_i,
                    year_j,
                    n_type,
                    w_x,
                    state_x,
                    cat_x,
                    dea_npi
                ),
                ## Patient overlap
                return_overlap(
                    pt_ntiles,
                    year_i,
                    year_j,
                    n_type,
                    w_x,
                    state_x,
                    cat_x,
                    patid
                ),
                ## Patient-to-provider overlap
                return_overlap(
                    pair_ntiles,
                    year_i,
                    year_j,
                    n_type,
                    w_x,
                    state_x,
                    cat_x,
                    pair_id
                )
            )
            sink()
            x
        }
        doParallel::stopImplicitCluster()
        
        ## Remove small cells
        jaccard_df <- jaccard_df %>%
            dplyr::filter(n_i > 10,
                          n_j > 10)
        saveRDS(jaccard_df,
                here::here(
                    "result_objects",
                    "overlap",
                    sprintf("overlap_%s.RDS", n_type)
                ),
                compress = "xz")
    }
}

## Save all overlap files into one ----
if (!fs::file_exists(here::here("data", "overlap_all.RDS"))) {
    overlap_all <- purrr::map_df(
        .x = fs::dir_ls(here::here("result_objects", "overlap"),
                        glob = "*.RDS"),
        .f = ~ readRDS(.x))
    saveRDS(overlap_all, here::here("data", "overlap_all.RDS"))
}
