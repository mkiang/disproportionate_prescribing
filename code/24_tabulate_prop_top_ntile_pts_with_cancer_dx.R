## 24_tabulate_proportion_top_patients_with_cancer_dx.R ----
##
## We go through the subsetted dx files and tabulate the proportion of top
## patients with a previous malignant cancer diagnosis (for each drug and year).
## We count any cancer diagnosis, which includes cancers that were in 
## remission when the patient was in the top centile. 

## Imports ----
library(tidyverse)
library(foreach)
library(parallel)
library(doParallel)
library(here)
library(fs)
source(here::here("code", "utils.R"))

param_grid <- expand.grid(
    year_x = 2017:2003,
    ndc_x = return_drug_names(),
    stringsAsFactors = FALSE
)

if (!fs::file_exists(here::here("data_private", "top_ntile_pt_with_cancer_dx.RDS"))) {
    doParallel::registerDoParallel(cores = 14)
    results <- foreach::foreach(
        i = 1:NROW(param_grid),
        .inorder = FALSE,
        .combine = dplyr::bind_rows
    ) %dopar% {
        sink(sprintf("./result_objects/logs/log_%s.txt", Sys.getpid()))
        year_x <- param_grid$year_x[i]
        ndc_x <- param_grid$ndc_x[i]
        
        print(sprintf("%s %s %s", i, year_x, ndc_x))
        
        temp_df <- import_mini_dx_data(year_x, ndc_x)
        
        ## If using ICD-10, flag cancers with this
        if (temp_df$icd_flag[1] == 10) {
            temp_df$cancer <-
                grepl("\\<C[0-9]|\\<D[012]|\\<D3[123456]",
                      temp_df$diag) + 0
            
            temp_df$malignant_cancer <-
                grepl("\\<C[0-9]", temp_df$diag) + 0
            
        }
        
        ## If using ICD-9, flag with this
        if (temp_df$icd_flag[1] == 9) {
            temp_df$cancer <-
                grepl(paste(
                    c(
                        "\\<1[4589][0-9]{2}\\>",
                        "\\<1[67][012345][0-9]{1}\\>",
                        "\\<176[0-9]{1,2}\\>",
                        "\\<179[0-9]{1,2}\\>",
                        "\\<200[0-9]{1}[0-9]{0,1}\\>",
                        "\\<201[0-9]{1}\\>",
                        "\\<202[0-9]{1,2}\\>",
                        "\\<2030",
                        "\\<20[45][01]{1}\\>",
                        "\\<2060",
                        "\\<207[012][0-9]{0,1}\\>",
                        "\\<2080",
                        "\\<209[0-6]{1}[0-9]{0,1}\\>",
                        "\\<2100",
                        "\\<211[03]{1}\\>",
                        "\\<212[0-7]{1}[0-9]{0,1}\\>",
                        "\\<213[09]{1}\\>",
                        "\\<21[456789]{1}0\\>",
                        "\\<22[012345679]{1}0\\>",
                        "\\<228[01]{1}\\>",
                        "\\<23[0123456]{1}0\\>",
                        "\\<237[07]{1}\\>",
                        "\\<238[04]{1}\\>",
                        "\\<239[02]{1}\\>"
                    ),
                    collapse = "|"
                ),
                temp_df$diag) + 0
            
            temp_df$malignant_cancer <-
                grepl(paste(
                    c(
                        "\\<1[4589][0-9]{2}\\>",
                        "\\<1[67][012345][0-9]{1}\\>",
                        # "\\<176[0-9]{1,2}\\>",
                        "\\<179[0-9]{1,2}\\>",
                        "\\<200[0-9]{1}[0-9]{0,1}\\>",
                        "\\<201[0-9]{1}\\>",
                        "\\<202[0-9]{1,2}\\>",
                        "\\<2030",
                        "\\<20[45][01]{1}\\>",
                        "\\<2060",
                        "\\<207[012][0-9]{0,1}\\>",
                        "\\<2080",
                        "\\<209[0-3]{1}[0-9]{0,1}\\>"
                        # "\\<2100",
                        # "\\<211[03]{1}\\>",
                        # "\\<212[0-7]{1}[0-9]{0,1}\\>",
                        # "\\<213[09]{1}\\>",
                        # "\\<21[456789]{1}0\\>",
                        # "\\<22[012345679]{1}0\\>",
                        # "\\<228[01]{1}\\>",
                        # "\\<23[0123456]{1}0\\>",
                        # "\\<237[07]{1}\\>",
                        # "\\<238[04]{1}\\>",
                        # "\\<239[02]{1}\\>"
                    ),
                    collapse = "|"
                ),
                temp_df$diag) + 0
            
        }
        
        ## Return only malignant cancer patients
        patid_x <- temp_df %>%
            dplyr::filter(malignant_cancer == 1) %>%
            dplyr::select(patid) %>%
            dplyr::distinct() %>%
            dplyr::mutate(year = year_x, ndc_type = ndc_x)
        
        ## Clean up
        rm(temp_df)
        gc2()
        sink()
        
        patid_x
    }
    doParallel::stopImplicitCluster()
    
    results <- results %>%
        dplyr::group_by(patid) %>%
        dplyr::summarize(year_first_cancer_dx = min(year)) %>%
        dplyr::ungroup()
    
    saveRDS(
        results,
        here::here("data_private", "top_ntile_pt_with_cancer_dx.RDS"),
        compress = "xz"
    )
}

## Now load up every year/ndc ----
## For each year/ndc, find the proportion of patients in the top centile
## who have had *any* diagnosis of malignant cancer in the previous years.

cancer_pt <-
    readRDS(here::here("data_private", "top_ntile_pt_with_cancer_dx.RDS"))

doParallel::registerDoParallel(cores = 14)
results <- foreach::foreach(
    i = 1:NROW(param_grid),
    .inorder = FALSE,
    .combine = dplyr::bind_rows
) %dopar% {
    year_x <- param_grid$year_x[i]
    ndc_x <- param_grid$ndc_x[i]
    
    print(sprintf("%s %s %s", i, year_x, ndc_x))
    
    sub_pt <- cancer_pt %>%
        dplyr::filter(year_first_cancer_dx <= year_x) %>%
        dplyr::pull(patid) %>%
        unique(.)
    
    temp_x <- readRDS(here::here(
        "result_objects",
        "top_ntile_patients",
        ndc_x,
        sprintf("%s_%s_US_all_types_top_ntile.RDS", ndc_x, year_x)
    )) %>%
        dplyr::pull(patid)
    
    dplyr::tibble(
        year = year_x,
        ndc_type = ndc_x,
        prop_cancer_dx = mean(temp_x %in% sub_pt)
    )
    
    
}
doParallel::stopImplicitCluster()
readr::write_csv(results, here::here("data", "proportion_cancer_dx.csv"))
