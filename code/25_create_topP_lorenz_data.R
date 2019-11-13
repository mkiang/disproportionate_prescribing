## 25_create_topP_lorenz_data.R ----
## 
## For the Shiny app and figure, we don't need the full Lorenz curve and it is
## quite cumbersome to work with. Instead, we will subset the curve into the 
## top P where P is the top 1%, 5%, 10%, 25%, and 50% of providers. This 
## smaller data set will be used for plots and the Shiny app (as well as faster
## exploration for interested readers).

## Imports ----
library(tidyverse)
library(fs)
library(foreach)
library(doParallel)
library(here)
source(here::here("code", "utils.R"))

if (!fs::file_exists(here::here("data", "lorenz_top_p_all.RDS"))) {
    ## Cycle through all lorenz files and save only the points we will be
    ## plotting for the shiny app -- top 1, 5, 10, 25, 50% over time.
    lorenz_files <- fs::dir_ls(here::here("data", "lorenz"),
                               recurse = TRUE,
                               glob = "*.RDS")
    
    doParallel::registerDoParallel(cores = 14)
    results_holder <- foreach::foreach(
        i = 1:NROW(lorenz_files),
        .inorder = FALSE
    ) %dopar% {
        x <- import_lorenz_top_p_only(lorenz_files[i])
    }
    doParallel::stopImplicitCluster()
    
    ## Save file ----
    ## NOTE: Save as RDS instead of compressed csv(.gz). RDS with xz 
    ## compression is about 33% smaller and loads faster (but is slower to 
    ## save). Using csv.gz results in a Shiny app that cannot be used online
    ## due to size and speed constraints.
    saveRDS(dplyr::bind_rows(results_holder),
            here::here("data", "lorenz_top_p_all.RDS"),
            compress = "xz")
}
