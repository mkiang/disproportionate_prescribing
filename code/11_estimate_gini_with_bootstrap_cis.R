## 11_estimate_gini_with_bootstrap_cis.R ----
##
## In general, our Gini coefficients at the national level will be very precise
## because of the large sample size. To confirm this, we'll estimate the
## bootstrapped 95% confidence intervals of the Gini coefficient. However,
## to keep this computationally-feasible, we will limit the bootstrapping to
## the Gini coefficient (i.e., no other inequality metrics). See note below
## regarding memory-intensity. This should probably only be done if you *really*
## need to do it and you're comparing small subsets (e.g., providers of one
## specialty ine one state to providers in another state). Once you have more 
## than ~50,000 observations, the Gini is precise within rounding error. .

## Imports ----
library(tidyverse)
library(here)
library(boot)
library(foreach)
library(doParallel)
library(parallel)
library(fs)
source(here::here("code", "utils.R"))

## Data ----
prov_st_cat <-
    readr::read_csv(here::here("data_private", "provider_type_state.csv.xz"))
mme_convers <- load_mme_conversion()

## Cycle through a partial grid ----
full_grid <- return_full_grid()

## Warning: Bootstraps are *highly* variable in the amount of memory they need.
## Most of the bootstraps will be around 2-5 GB of memory, but some will be
## as high as 70 GB and some ~1 GB. To prevent high cost nodes from crashing,
## you should be as conservative as reasonable with the cores. We used 8 cores
## with 128 GB of RAM and it took approximately ~1,000 compute-hours with one
## or two restarts after nodes failed from consuming too much memory.
doParallel::registerDoParallel(cores = 8)
results <- foreach::foreach(i = sample(1:NROW(full_grid)),
                            .inorder = FALSE) %dopar% {
                                ## Get constants
                                ndc_x <- full_grid$ndc_type[i]
                                year_x <- full_grid$year[i]
                                state_x <- full_grid$state[i]
                                prov_cat_x <- full_grid$specialty[i]
                                
                                load_and_calculate_bootstrapped_gini(
                                    year_x = year_x,
                                    ndc_x = ndc_x,
                                    state_x = as.character(state_x),
                                    prov_cat_x = prov_cat_x,
                                    mme_convers_x = mme_convers,
                                    reps_x = 1000
                                )
                            }
doParallel::stopImplicitCluster()

## Gather files. NOTE: I forgot to save provider type in the tibble of the
## main function so we save the file name so that we can parse out provider
## type.
if (!fs::file_exists(here::here("data", "gini_with_ci.csv"))) {
    results <- purrr::map_df(
        .x = fs::dir_ls(
            here::here("result_objects", "bootstrap_gini"),
            recurse = TRUE,
            glob = "*.RDS"
        ),
        .f = ~ readRDS(.x) %>%
            dplyr::mutate(f_path = .x)
    )
    
    results <- results %>%
        dplyr::mutate(
            prov_cat =
                dplyr::case_when(
                    grepl("all_types", f_path, fixed = TRUE) ~ "all_types",
                    grepl("critical_care", f_path, fixed = TRUE) ~ "critical_care",
                    grepl("emergency", f_path, fixed = TRUE) ~ "emergency",
                    grepl("prev_internal_family", f_path, fixed = TRUE) ~ "prev_internal_family",
                    grepl("cat_of_interest", f_path, fixed = TRUE) ~ "cat_of_interest",
                    grepl("hospice", f_path, fixed = TRUE) ~ "hospice",
                    grepl("all_surgery", f_path, fixed = TRUE) ~ "all_surgery",
                    grepl("gen_surgery", f_path, fixed = TRUE) ~ "gen_surgery",
                    grepl("plastics", f_path, fixed = TRUE) ~ "plastics",
                    grepl("internal", f_path, fixed = TRUE) ~ "internal",
                    grepl("family", f_path, fixed = TRUE) ~ "family",
                    grepl("pmnr", f_path, fixed = TRUE) ~ "pmnr",
                    grepl("pediatrics", f_path, fixed = TRUE) ~ "pediatrics",
                    grepl("ortho", f_path, fixed = TRUE) ~ "ortho",
                    grepl("obgyn", f_path, fixed = TRUE) ~ "obgyn"
                )
        ) %>%
        dplyr::select(-f_path)
    readr::write_csv(results, here::here("data", "gini_with_ci.csv"))
}
