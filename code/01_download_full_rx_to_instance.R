## 01_download_full_rx_to_instance.R ----
##
## This file downloads all of the full prescription files from a secure
## storage bucket to our (still secure) compute instance. Because files
## sometimes don't complete, we check for both the existence of the file and
## whether or not that file is at least 97% of the expected file size.
##
## You MUST have the full files to calculate the descriptives and in order
## to conduct our sensitivity analyses based on prescriber activity. 
## Once those are done, we will subset the files to only the rows and 
## columns necessary for analysis and remove the full files.

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(googleCloudStorageR)
library(foreach)
library(doParallel)
library(parallel)
source(here::here("code", "utils.R"))

## Download FULL prescription data ----
fs::dir_create(here::here("data_private", "rx"))

## Get google storage locations of all prescription files (zip5_r).
rx_files <- return_bucket_objects(f_regex = "\\<zip5_r[0-9]{4}q[1-4]{1}")

## Loop through each google storage object and download to a local disk
## Note: I check the file size because there are a lot of files and if a
## download fails, it's hard to detect. Loop twice to verify full files are
## downloaded.
doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
foreach::foreach(i = 1:NROW(rx_files)) %dopar% {
    f <- rx_files$name[i]
    bucket <- rx_files$bucket[i]
    expected_size <- fs::fs_bytes(paste0(rx_files$gigabytes[i] * .97, "GB"))
    
    f_priv <- here::here("data_private", "rx", f)
    
    ## Make sure we haven't already downloaded and
    ## that the file completed download
    if (!fs::file_exists(f_priv) || 
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
