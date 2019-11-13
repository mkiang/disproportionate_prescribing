## 05_subset_rx_by_ndc.R ----
## 
## The main files are large and cumbersome to work with so this script just
## subsets the main files into the columns and rows we need for the majority
## of the analyses in order to cut down on file loading times. For each full
## prescription file, there will be N subsetted files where N is the number of
## drug categories (NDCs) we analyze -- defined in return_ndc_keypairs().
## 
## Note that this is not space-conservative because we duplicate data in many
## instances (e.g., all benzos appear both in the benzos folder and the 
## individual folder); however, the cost of disk space (vs complexity and 
## compute time) is small. 

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(vroom)
library(foreach)
library(doParallel)
library(parallel)
source(here::here("code", "utils.R"))

## Data / Constants ----
data_minimized_rx <- here::here("data_private_mini", "rx")
ndcs <- load_ndc_dictionary()
ndc_chars <- return_drug_names()

## Create folders --- one for each ndc ----
fs::dir_create(here::here("data_private_mini", "rx", ndc_chars))
fs::dir_create(here::here("result_objects", "logs"))

## Helper function to read csv's in chunks ----
read_select_chunked <- function(f_path) {
    ## Column selectors
    c_only <- readr::cols_only(Patid = "c",
                        Dea = "c",
                        Npi = "c", 
                        Ndc = "c", 
                        Days_Sup = "i", 
                        Quantity = "i", 
                        Fill_Dt = "c")
    
    ## Chunked callback
    callback_f <- function(x, pos) {
        x %>% 
            dplyr::select(dplyr::one_of("Patid", "Dea", "Npi", "Ndc",
                          "Days_Sup", "Quantity", "Fill_Dt"))
    }
    
    ## Read in
    temp_df <- readr::read_csv_chunked(
        f_path,
        col_types = c_only,
        callback = readr::DataFrameCallback$new(callback_f),
        progress = FALSE,
        chunk_size = 50000
    )
    
    ## Better column names
    names(temp_df) <- tolower(names(temp_df))
    
    return(temp_df)
}

## Loop through all Rx files and make subsets based on NDC ---- 
f_list <- sample(fs::dir_ls(here::here("data_private", "rx")))
doParallel::registerDoParallel(cores = 12)
results <- foreach::foreach(f = f_list, .inorder = FALSE) %dopar% {
    if (any(!fs::file_exists(
        here::here(
            "data_private_mini",
            "rx",
            ndc_chars,
            sprintf("%s_%s.xz", ndc_chars, basename(f))
        )
    ))) {
        ## Cheap logging
        sink(sprintf(
            "./result_objects/logs/log_%s_rx_subsetting.txt",
            Sys.getpid()
        ),
        append = TRUE)
        print(sprintf("%s", f))
        print(Sys.time())
        
        ## Read in full Rx with just columns we want
        temp_df <- read_select_chunked(f)
        
        ## Subset rows and save each subset individually
        for (n in ndc_chars) {
            f_new <- here::here("data_private_mini",
                                "rx",
                                n,
                                sprintf("%s_%s.xz", n, basename(f)))
            
            if (!fs::file_exists(f_new)) {
                print(n)
                
                temp_sub <- temp_df %>%
                    dplyr::filter(ndc %in% ndcs[[n]])
                readr::write_csv(temp_sub, path = f_new)
                
                rm(temp_sub); gc2()
            } else {
                print(sprintf("Skipping: %s", f_new))
            }
        }
        
        rm(temp_df); gc2()
        sink()
    }
}
doParallel::stopImplicitCluster()
