## 14_extract_top_ntile_providers_patients_pairs.R ----
##
## For each set of NDCs, we want to extract the top centile group of providers,
## patients, and provider-to-patient transactions for each year based on
## the volume prescribed. We do this for the whole US and across all providers
## (you can specify states or provider specialties if desired). Intermediate
## files are created and then collected at the end. 

## Imports ----
library(tidyverse)
library(here)
library(foreach)
library(doParallel)
library(parallel)
library(fs)
source(here::here("code", "utils.R"))

## Set the levels of trimming ----
winsor_p <- return_winsor_p()

## Data ----
prov_st_cat <-
    readr::read_csv(here::here("data_private", "provider_type_state.csv.xz"))
mme_convers <- load_mme_conversion()

## Make a custom grid ----
## This analysis is just for the whole US and across all provider types, but
## we want the code to be flexible enough to change.
ntile_grid <- expand.grid(
    year = 2017:2003,
    ndc = return_drug_names(),
    state = "US",
    specialty = "all_types",
    stringsAsFactors = FALSE
)

## Import and calculate all overlap ----
doParallel::registerDoParallel(cores = 20)
holder <-  foreach::foreach(
    i = sample(1:NROW(ntile_grid)),
    .inorder = FALSE) %dopar% {
        year_x <- ntile_grid$year[i]
        ndc_x <- ntile_grid$ndc[i]
        state_x <- ntile_grid$state[i]
        prov_cat_x <-
            ntile_grid$specialty[i]
        
        ## Import a full mini year of opioids
        orig_df <-
            import_mini_rx_data(year_x, ndc_x)
        
        ## Subset
        temp_df <- subset_df(
            orig_df = orig_df,
            state = state_x,
            prov_cat = prov_cat_x,
            prov_st_cat = prov_st_cat
        )
        
        ## Calculate MMEs
        temp_df <- temp_df %>%
            calculate_mmes(df = .,
                           mme_convers_df = mme_convers,
                           ndc_type_x = ndc_x)
        
        extract_top_ntile_providers(
            temp_df,
            year_x = year_x,
            ndc_x = ndc_x,
            state_x = state_x,
            prov_cat_x = prov_cat_x,
            mme_convers_x = mme_convers,
            prov_st_cat_x = prov_st_cat,
            winsor_p_x = winsor_p
        )
        
        extract_top_ntile_patients(
            temp_df,
            year_x = year_x,
            ndc_x = ndc_x,
            state_x = state_x,
            prov_cat_x = prov_cat_x,
            mme_convers_x = mme_convers,
            prov_st_cat_x = prov_st_cat,
            winsor_p_x = winsor_p
        )
        
        extract_top_ntile_pairs(
            temp_df,
            year_x = year_x,
            ndc_x = ndc_x,
            state_x = state_x,
            prov_cat_x = prov_cat_x,
            mme_convers_x = mme_convers,
            prov_st_cat_x = prov_st_cat,
            winsor_p_x = winsor_p
        )
        
    }
doParallel::stopImplicitCluster()

# Combine the top ntile providers into a single file ----
if (!fs::file_exists(here::here("data_private", "top_ntile_providers.RDS"))) {
    top_prov_files <-
        fs::dir_ls(
            here::here("result_objects", "top_ntile"),
            recurse = TRUE,
            glob = "*.RDS"
        )
    prov_holder <- vector("list", NROW(top_prov_files))
    for (i in seq_along(prov_holder)) {
        prov_holder[[i]] <- readRDS(top_prov_files[i])
    }
    prov_holder <- dplyr::bind_rows(prov_holder)
    saveRDS(prov_holder,
            here::here("data_private", "top_ntile_providers.RDS"),
            compress = "xz")
}

# Combine the top ntile patients into a single file ----
if (!fs::file_exists(here::here("data_private", "top_ntile_patients.RDS"))) {
    top_pt_files <-
        fs::dir_ls(
            here::here("result_objects", "top_ntile_patients"),
            recurse = TRUE,
            glob = "*.RDS"
        )
    pt_holder <- vector("list", NROW(top_pt_files))
    for (i in seq_along(pt_holder)) {
        pt_holder[[i]] <- readRDS(top_pt_files[i])
    }
    pt_holder <- dplyr::bind_rows(pt_holder)
    saveRDS(pt_holder,
            here::here("data_private", "top_ntile_patients.RDS"),
            compress = "xz")
}

# Combine the top ntile pairs into a single file ----
if (!fs::file_exists(here::here("data_private", "top_ntile_pairs.RDS"))) {
    top_pairs_files <-
        fs::dir_ls(
            here::here("result_objects", "top_ntile_pairs"),
            recurse = TRUE,
            glob = "*.RDS"
        )
    pairs_holder <- vector("list", NROW(top_pairs_files))
    for (i in seq_along(pairs_holder)) {
        pairs_holder[[i]] <- readRDS(top_pairs_files[i])
    }
    pairs_holder <- dplyr::bind_rows(pairs_holder)
    saveRDS(pairs_holder,
            here::here("data_private", "top_ntile_pairs.RDS"),
            compress = "xz")
}
