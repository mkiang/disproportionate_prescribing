## 12_calculate_lorenz_curves.R
## 
## Calculates the Lorenz curve for all NDCs, for each year. Further, we do this
## for specified state/provider types and all sensitivity analyses. A total
## of ~25000 Lorenz curves are calculated. Warning: This takes a long time. 
## 
## Note also that the Lorenz curves are originally calculated to return the 
## same length as observations --- this is unmanageably large (e.g., some curves
## would have >500,000 prescribers/datapoints). All intermediate files are
## saved to a maximum of 500 points. The final file used in plots and the Shiny
## app is saved to <100 points (or they wouldn't fit on a Shiny app). 

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
            
            load_and_calculate_lorenz_results(
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
            if (all(fs::file_exists(here::here(
                "result_objects",
                "lorenz",
                sprintf(
                    "%s/%s/%s_%s_%s_%s_lorenz_results.RDS",
                    small_grid$state,
                    ndc,
                    ndc,
                    y,
                    small_grid$state,
                    small_grid$specialty
                )
            )))) {
                next
            }
            
            orig_df <- import_mini_rx_data(y, ndc)
            
            doParallel::registerDoParallel(cores = small_core)
            results <- foreach::foreach(
                i = 1:NROW(small_grid), .inorder = FALSE
                ) %dopar% {
                    ## Get constants
                    ndc_x <- ndc
                    year_x <- y
                    state_x <- small_grid$state[i]
                    prov_cat_x <- small_grid$specialty[i]
                    
                    calculate_lorenz_results(
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
    i = 1:NROW(param_grid_full), 
    .inorder = FALSE) %dopar% {
        ## Get constants
        ndc_x <- param_grid_full$ndc_type[i]
        year_x <- param_grid_full$year[i]
        state_x <- param_grid_full$state[i]
        prov_cat_x <- param_grid_full$specialty[i]
        
        load_and_calculate_lorenz_results(
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

# Get metadata of all lorenz files for future reference ----
all_lorenz_files <- fs::dir_ls(here::here("result_objects", "lorenz"),
                           recurse = TRUE, glob = "*.RDS")
lorenz_meta <- fs::file_info(all_lorenz_files)
fs::dir_create(here::here("data", "metadata"))
saveRDS(lorenz_meta,
        here::here("data", "metadata", "intermediate_lorenz_files.RDS"))
 
## Save downsampled lorenz curves ----
lorenz_files <- fs::dir_ls(here::here("result_objects", "lorenz"), 
                           recurse = TRUE, 
                           glob = "*.RDS")

doParallel::registerDoParallel(cores = full_core)
results <- foreach::foreach(i = 1:NROW(lorenz_files), .inorder = FALSE) %dopar% {
    sink(sprintf("./result_objects/logs/log_%s.txt", Sys.getpid()))
    f <- lorenz_files[i]
    f_new <- gsub("result_objects", "data", f, fixed = TRUE)
    
    if (!fs::dir_exists(dirname(f_new))) {
        fs::dir_create(dirname(f_new))
    }
    
    if (!fs::file_exists(f_new)) {
        sprintf("Downsampling: %s", f_new)
        
        temp_df <- downsample_lorenz_curve(readRDS(f))
        if (NROW(temp_df) > 25) {
            saveRDS(temp_df, f_new)
        }
    } else {
        sprintf("Skipping: %s", f)
    }
    sink()
}
doParallel::stopImplicitCluster()
