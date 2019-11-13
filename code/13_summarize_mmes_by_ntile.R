## 13_summarize_mmes_by_ntile.R ----
## 
## For each set of NDCs-years, we divide the providers into 100 equally sized 
## groups by the amount prescribed. We then calculate the average within each
## of the groups. Files are saved into intermediate files and then collected
## at the end. 

## Imports ----
library(tidyverse)
library(here)
library(foreach)
library(doParallel)
library(parallel)
library(fs)
source(here::here("code", "utils.R"))

## Constants ----
## I divide up the tasks into "large" and "small". Roughly, small tasks are
## such that calculating the estimand is fast relative to loading the data, so 
## I load the data once and complete all tasks for that dataset and repeat.
## 
## Conversely, large tasks are those where loading the data is fast relative
## to the calculating the estimand so for those, each node loads the data 
## itself.
## 
## In a sufficiently resourced computing environment, you don't need to split
## tasks this way and you can instead just run through the whole loop at once,
## copying the data on to each node separately. We do it this way but provide
## the other two loops for resource-limited computing environments. Just change
## the constants below to TRUE.
large_loop <- FALSE
small_loop <- FALSE
large_core <- 12
small_core <- 24
full_core  <- 18

## Set the levels of trimming ----
## Note that there are around 300 combinations of area/provider type, 
## so if you run the two full sensitivity analyses below, you will run 
## through ~17000 analyses. 
## 
## To skip the sensitivity analyses, set winsor_p = 0 and min_r = 1. 
winsor_p <- return_winsor_p()
min_rx <- return_min_rx()

## Data ----
prov_st_cat <-
    readr::read_csv(here::here("data_private", "provider_type_state.csv.xz"))
active_prescribers <-
    readRDS(here::here("data_private", "active_prescribers.RDS"))
mme_convers <- load_mme_conversion()

## Large grid first ----
if (large_loop) {
    large_grid <- return_large_grid()
    doParallel::registerDoParallel(cores = large_core)
    results <-
        foreach::foreach(i = 1:NROW(large_grid), .inorder = FALSE) %dopar% {
            ## Get constants
            ndc_x <- large_grid$ndc_type[i]
            year_x <- large_grid$year[i]
            state_x <- large_grid$state[i]
            prov_cat_x <- large_grid$specialty[i]
            
            load_and_calculate_ntile_mmes(
                year_x = year_x,
                ndc_x = ndc_x,
                state_x = state_x,
                prov_cat_x = prov_cat_x,
                mme_convers_x = mme_convers,
                prov_st_cat_x = prov_st_cat,
                winsor_p_x = winsor_p, 
                active_prescribers_x = active_prescribers, 
                min_rx_x = min_rx
            )
        }
    doParallel::stopImplicitCluster()
}

## Small grid ----
if (small_loop) {
    small_grid <- return_small_grid()
    for (y in 2017:2003) {
        for (ndc in rev(return_drug_names())) {
            print(sprintf("%s %s", y, ndc))
            orig_df <- import_mini_rx_data(y, ndc, use_vroom = TRUE)
            
            doParallel::registerDoParallel(cores = small_core)
            
            results <- foreach::foreach(
                i = 1:NROW(small_grid), .inorder = FALSE
                ) %dopar% {
                    ## Get constants
                    ndc_x <- ndc
                    year_x <- y
                    state_x <- small_grid$state[i]
                    prov_cat_x <- small_grid$specialty[i]
                    
                    calculate_ntile_mmes(
                        orig_df = orig_df,
                        year_x = year_x,
                        ndc_x = ndc_x,
                        state_x = state_x,
                        prov_cat_x = prov_cat_x,
                        mme_convers_x = mme_convers,
                        prov_st_cat_x = prov_st_cat,
                        winsor_p_x = winsor_p,
                        active_prescribers_x = active_prescribers,
                        min_rx_x = min_rx
                    )
                }
            doParallel::stopImplicitCluster()
        }
    }
}

## Full grid ----
## We *always* loop through the full grid in order to make sure all files
## have been created. (Sometimes a node will crash before finishing a task.)
param_grid_full <- return_full_grid()
doParallel::registerDoParallel(cores = full_core)
results <- foreach::foreach(
    i = 1:NROW(param_grid_full), 
    .inorder = FALSE) %dopar% {
        ## Get constants
        ndc_x <- param_grid_full$ndc_type[i]
        year_x <- param_grid_full$year[i]
        state_x <- param_grid_full$state[i]
        prov_cat_x <- param_grid_full$specialty[i]
        
        load_and_calculate_ntile_mmes(
            year_x = year_x,
            ndc_x = ndc_x,
            state_x = state_x,
            prov_cat_x = prov_cat_x,
            mme_convers_x = mme_convers,
            prov_st_cat_x = prov_st_cat,
            winsor_p_x = winsor_p, 
            active_prescribers_x = active_prescribers, 
            min_rx_x = min_rx
        )
    }
doParallel::stopImplicitCluster()

## Get metadata of mme summaries for future reference ----
all_mme_ntiles <- fs::dir_ls(here::here("result_objects", "mme_ntiles"),
                         recursive = TRUE, glob = "*.RDS")
mme_meta <- fs::file_info(all_mme_ntiles)
fs::dir_create(here::here("data", "metadata"))
saveRDS(mme_meta, 
        here::here("data", "metadata", "intermediate_mme_ntile_summary_files.RDS"))

## Collect mme_summaries into one file for each state/provider-type ----
doParallel::registerDoParallel(cores = full_core)
results <-
    foreach::foreach(
        s = c(datasets::state.abb, "US", "NA"), .inorder = FALSE) %dopar% {
            print(sprintf("%s", s))
            
            if (!fs::dir_exists(here::here("data", "mme_ntiles"))) {
                fs::dir_create(here::here("data", "mme_ntiles"))
            }
            
            target_files <-
                fs::dir_ls(here::here("result_objects", "mme_ntiles", s),
                           recurse = TRUE,
                           glob = "*.RDS")
            temp_df <- purrr::map_df(.x = target_files,
                                     .f = ~ readRDS(.x))
            
            ## Remove groups that have <10 providers or patients
            temp_df <- temp_df %>%
                dplyr::filter(n_prov > 10,
                              n_patid > 10)
            
            for (prov_x in unique(temp_df$prov_cat)) {
                f_new <-
                    here::here("data",
                               "mme_ntiles",
                               sprintf("%s_%s_mme_ntile_summary.csv.xz", s, prov_x))
                
                if (!fs::file_exists(f_new)) {
                    readr::write_csv(temp_df %>%
                                         dplyr::filter(prov_cat == prov_x),
                                     f_new)
                } else {
                    print(sprintf("Skipping: %s", f_new))
                }
            }
        }
doParallel::stopImplicitCluster()
