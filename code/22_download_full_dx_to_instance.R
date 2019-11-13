## 22_download_full_dx_to_instance.R ----
## 
## Download the full diagnostic files for Optum. These files are *large* so
## this will take a while. We need these files in order to get the prevalence
## of malignant cancer among the top 1% of opioid-receiving patients as well 
## as the most common recent primary diagnoses.

## Imports ----
library(tidyverse)
library(here)
library(foreach)
library(doParallel)
library(fs)
library(googleCloudStorageR)
source(here::here("code", "utils.R"))

## Download FULL diagnostic data ----
fs::dir_create(here::here("data_private", "dx"))

## Get google storage locations of all prescription files
dx_files <- return_bucket_objects(f_regex = "zip5_diag[0-9]{4}q[1-4]{1}.csv")

## Loop through each google storage object and download to a local disk
doParallel::registerDoParallel(cores = parallel::detectCores() - 2)
foreach::foreach(i = sample(1:NROW(dx_files))) %dopar% {
    f <- dx_files$name[i]
    bucket <- dx_files$bucket[i]
    expected_size <- fs::fs_bytes(paste0(dx_files$gigabytes[i] * .97, "GB"))
    
    f_priv <- here::here("data_private", "dx", f)
    
    ## Make sure we haven't already downloaded and
    ## that the file completed download
    if (!fs::file_exists(f_priv) |
        fs::file_info(f_priv)$size < expected_size) {
        googleAuthR::gar_gce_auth()
        
        googleCloudStorageR::gcs_get_object(
            object_name = f,
            bucket = bucket,
            saveToDisk = f_priv,
            overwrite = TRUE
        )
        
        print(f_priv)
        
    } else {
        sprintf("Skipping: %s", f_priv)
    }
}
doParallel::stopImplicitCluster()
