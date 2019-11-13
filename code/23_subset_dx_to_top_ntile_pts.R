## 21_subset_dx_to_top_ntile_pts.R ----
## 
## The main files are large and cumbersome to work with so this script just
## subsets the main files into the columns and rows we need for the majority
## of the analyses in order to cut down on file loading times. We subset rows
## to only patients who are in the top 1% of any of our drugs of interest.

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(foreach)
library(doParallel)
library(parallel)
source(here::here("code", "utils.R"))

## Constants ----
data_minimized_dx <- here::here("data_private_mini", "dx")
ndc_chars <- return_drug_names()

## Make a list of all top ntile patients for each ndc ----
## Recall we did this with several weights so length will be > 1% of total.
pt_dict <- list()
for (ndc in ndc_chars) {
    pt_dict[[ndc]] <-
        purrr::map_df(.x = fs::dir_ls(here::here(
            "result_objects", "top_ntile_patients", ndc
        )), .f = ~ readRDS(.x)) %>% 
        dplyr::pull(patid) %>% 
        unique(.)
}


dx_files <- fs::dir_ls(here::here("data_private", "dx"))

doParallel::registerDoParallel(cores = 4)
results <-
    foreach::foreach(i = sample(1:NROW(dx_files)), .inorder = FALSE) %dopar% {
        sink(sprintf("./result_objects/logs/log_%s.txt", Sys.getpid()))
        
        f <- dx_files[i]
        
        if (any(!fs::file_exists(
            sprintf("%s/%s/%s.xz", data_minimized_dx, ndc_chars, basename(f))
        ))) {
            print(sprintf("Importing: %s", f))
            
            full_dx <- readr::read_csv(f, progress = FALSE) %>%
                janitor::clean_names(.)
            
            print(sprintf("%s imported", basename(f)))
            
            for (ndc in ndc_chars) {
                print(sprintf("%s %s %s", i, f, ndc))
                if (!fs::dir_exists(here::here("data_private_mini", "dx", ndc))) {
                    fs::dir_create(here::here("data_private_mini", "dx", ndc))
                }
                
                f_new <- sprintf("%s/%s/%s.xz",
                                 data_minimized_dx,
                                 ndc,
                                 basename(f))
                
                if (!fs::file_exists(f_new)) {
                    print(sprintf("Subsetting %s", ndc))
                    sub_df <- full_dx %>%
                        dplyr::filter(patid %in% pt_dict[[ndc]])
                    
                    print(sprintf("Writing %s", f_new))
                    readr::write_csv(sub_df, f_new)
                    rm(sub_df); gc2()
                } else {
                    print(sprintf("Skipping: %s", f_new))
                }
            }
        }
        sink()
    }
doParallel::stopImplicitCluster()
