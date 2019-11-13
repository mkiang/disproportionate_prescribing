## 28_supp_calculate_alt_inequality_measures.R ----
##
## Calculates a variety of inequality and concentration measures on the
## prescription data for all NDCs and selected permutations of provider type
## and state. Further, we run a variety of sensitivity analyses trimming both
## the upper end of the distribution and the lower end to check the robustness
## of our results. 

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
full_core  <- 16

## Set the levels of trimming ----
## Note that there are around 300 combinations of area/provider type, 
## so if you run the two full sensitivity analyses below, you will run 
## through ~22000 analyses. 
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
            
            load_and_calculate_ineq_measures_results(
                year_x = year_x,
                ndc_x = ndc_x,
                state_x = as.character(state_x),
                prov_cat_x = prov_cat_x,
                mme_convers_x = mme_convers,
                active_prescribers_x = active_prescribers,
                prov_st_cat_x = prov_st_cat,
                winsor_p_x = winsor_p,
                min_rx_x = min_rx
            )
            
        }
    doParallel::stopImplicitCluster()
}

## Small grid ----
if (small_loop) {
    small_grid <- return_small_grid()
    for (y in 2017:2003) {
        for (ndc in return_drug_names()) {
            print(sprintf("%s %s", y, ndc))
            
            ## Check if we've already made these intermediate files
            if (all(fs::file_exists(
                here::here(
                    "result_objects",
                    "ineq",
                    small_grid$state,
                    ndc,
                    sprintf(
                        "%s_%i_%s_%s_ineq_results.RDS",
                        ndc,
                        y,
                        small_grid$state,
                        small_grid$specialty
                    )
                )
            ))) {
                next
            }
            
            orig_df <- import_mini_rx_data(y, ndc)
            
            doParallel::registerDoParallel(cores = small_core)
            results <-
                foreach::foreach(i = 1:NROW(small_grid),
                        .inorder = FALSE) %dopar% {
                            ## Get constants
                            ndc_x <- ndc
                            year_x <- y
                            state_x <- small_grid$state[i]
                            prov_cat_x <- small_grid$specialty[i]
                            
                            calculate_ineq_measures_results(
                                orig_df = orig_df,
                                year_x = year_x,
                                ndc_x = ndc_x,
                                state_x = as.character(state_x),
                                prov_cat_x = prov_cat_x,
                                mme_convers_x = mme_convers,
                                active_prescribers_x = active_prescribers,
                                prov_st_cat_x = prov_st_cat,
                                winsor_p_x = winsor_p,
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
    i = 1:NROW(param_grid_full), .inorder = FALSE) %dopar% {
        ## Get constants
        ndc_x <- param_grid_full$ndc_type[i]
        year_x <- param_grid_full$year[i]
        state_x <- param_grid_full$state[i]
        prov_cat_x <- param_grid_full$specialty[i]
        
        load_and_calculate_ineq_measures_results(
            year_x = year_x,
            ndc_x = ndc_x,
            state_x = as.character(state_x),
            prov_cat_x = prov_cat_x,
            mme_convers_x = mme_convers,
            active_prescribers_x = active_prescribers,
            prov_st_cat_x = prov_st_cat,
            winsor_p_x = winsor_p,
            min_rx_x = min_rx
        )
    }
doParallel::stopImplicitCluster()

## Get metadata of all ineq files for future reference ----
all_ineq_files <- fs::dir_ls(here::here("result_objects", "ineq_meas"),
                         recurse = TRUE,
                         glob = "*.RDS")
ineq_meta <- fs::file_info(all_ineq_files)
fs::dir_create(here::here("data", "metadata"))
saveRDS(ineq_meta,
        here::here("data", "metadata", "intermediate_ineq_files.RDS"))

## Now collect all the ineq coefs into a single file ----
ineq_df <- vector("list", NROW(all_ineq_files))
for (i in seq_along(ineq_df)) {
    # print(i)
    ineq_df[[i]] <- readRDS(all_ineq_files[i])
}
ineq_df <- dplyr::bind_rows(ineq_df) %>%
    dplyr::filter(n_patients > 10,
           n_deanpi > 10,
           !is.na(gini))
readr::write_csv(ineq_df, here::here("data", "ineq_estimates_all.csv.xz"))
