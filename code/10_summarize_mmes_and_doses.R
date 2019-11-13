## 10_summarize_mmes_and_doses.R ----
##
## We want descriptives of the doses for each type of drug and year. Number of
## prescriptions, providers, patients, doses, etc. This loops through all
## subsetted prescription files and calculates the statistics for each, saving
## them into intermediate files. After all intermediate files are created,
## they will be collected into a single file. 

## Imports ----
library(tidyverse)
library(here)
library(foreach)
library(doParallel)
library(parallel)
library(fs)
source(here::here("code", "utils.R"))

## Create folder ----
fs::dir_create(here::here("data", "metadata"))

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
mme_convers <- load_mme_conversion()
active_prescribers <- 
    readRDS(here::here("data_private", "active_prescribers.RDS"))

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
            
            load_and_calculate_mme_summaries(
                year_x = year_x,
                ndc_x = ndc_x,
                state_x = state_x,
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
            if (all(fs::file_exists(
                here::here(
                    "result_objects",
                    "mme_summary",
                    sprintf(
                        "%s/%s/%s_%s_%s_%s_mme_summary.RDS",
                        small_grid$state,
                        ndc,
                        ndc,
                        y,
                        small_grid$state,
                        small_grid$specialty
                    )
                )
            ))) {
                next
            }
            
            orig_df <- import_mini_rx_data(y, ndc, progress_b = TRUE)
            
            doParallel::registerDoParallel(cores = small_core)
            results <- foreach::foreach(
                i = 1:NROW(small_grid), .inorder = FALSE
                ) %dopar% {
                    ## Get constants
                    ndc_x <- ndc
                    year_x <- y
                    state_x <- small_grid$state[i]
                    prov_cat_x <-
                        small_grid$specialty[i]
                    
                    calculate_mme_summaries(
                        orig_df = orig_df,
                        year_x = year_x,
                        ndc_x = ndc_x,
                        state_x = state_x,
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
## Even if you run the small and large loops above, you should run the full
## grid. It'll skip files you've done and sometimes nodes crash before 
## completing a task. 
param_grid_full <- return_full_grid()
doParallel::registerDoParallel(cores = full_core)
results <- foreach::foreach(
    i = 1:NROW(param_grid_full), .inorder = FALSE
    ) %dopar% {
        ## Get constants
        ndc_x <- param_grid_full$ndc_type[i]
        year_x <- param_grid_full$year[i]
        state_x <- param_grid_full$state[i]
        prov_cat_x <-
            param_grid_full$specialty[i]
        
        load_and_calculate_mme_summaries(
            year_x = year_x,
            ndc_x = ndc_x,
            state_x = state_x,
            prov_cat_x = prov_cat_x,
            mme_convers_x = mme_convers,
            active_prescribers_x = active_prescribers,
            prov_st_cat_x = prov_st_cat,
            winsor_p_x = winsor_p,
            min_rx_x = min_rx
        )
    }
doParallel::stopImplicitCluster()

## Get metadata of all mme summary files for future reference ----
all_mme_files <-
    fs::dir_ls(
        here::here("result_objects", "mme_summary"),
        recurse = TRUE,
        glob = "*.RDS"
    )
mme_meta <- fs::file_info(all_mme_files)
saveRDS(mme_meta,
        here::here("data", "metadata", "intermediate_mme_summary_files.RDS"))

## Now collect all the mme summaries into a single file ----
mme_results <- vector("list", NROW(all_mme_files))
for (i in seq_along(all_mme_files)) {
    mme_results[[i]] <- readRDS(all_mme_files[i])
}
mme_results <- dplyr::bind_rows(mme_results) %>%
    dplyr::filter(!is.na(median),
                  n_patients > 10,
                  n_deanpi > 10)
readr::write_csv(
    mme_results,
    here::here("data", "descriptives", "descriptives_mme_summaries.csv.xz")
)
