## 23_tabluate_ccs_of_top_patients.R ----
##
## For each of the subsetted dx files (by drug), take the primary diagnoses
## (i.e., diag_position == 1), collapse them into CCS groupings and then
## get the most frequent diagnostic groups by year and drug. 
## 
## Note that we cannot link diagnoses to prescriptions so this is a crude (but
## conservative) way of understanding the underlying prevalence of different
## diagnoses in this population. 

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(foreach)
source(here::here("code", "utils.R"))

## Get ICD to CCS crosswalks ----
icd10ccs <- readr::read_csv(
    here::here("data_raw", "ccs_dx_icd10cm_2019_1.csv"),
    col_names = c(
        "icd",
        "ccs",
        "icd_desc",
        "ccs_desc",
        "multi_ccs_1",
        "multi_ccs_1_desc",
        "multi_ccs_2",
        "multi_ccs_2_desc"
    ),
    col_types = "cccccccc",
    skip = 1,
) %>%
    dplyr::mutate(icd = gsub("'", "", icd, fixed = TRUE))
icd9ccs <- readr::read_csv(
    here::here("data_raw", "dxicd2ccsxw.csv"),
    skip = 2,
    col_names = c(
        "icd",
        "ccs",
        "ccs_desc",
        "icd_desc",
        "opt_ccs",
        "opt_ccs_desc",
        "icdvers"
    ),
    col_types = "ccccccc"
)

## For every year t, find top CCS primary diagnosis for t and t-1 ----
##
## We load up the top ntile patients for every year and then all diagnoses
## for year t and year t-1, filtering on top ntile of patients and primary
## diagnosis only. We then join with CCS codes and tabulate the top 25
## CCS codes. We also keep some summary data.
param_grid <- expand.grid(
    ndc = return_drug_names(),
    year = 2003:2017,
    prov_cat = "all_types",
    prov_state = "US",
    stringsAsFactors = FALSE
)
winsor_p <- return_winsor_p()

doParallel::registerDoParallel(cores = 14)
results_all <-
    foreach::foreach(i = 1:NROW(param_grid), .inorder = FALSE) %dopar% {
        ndc_x <- param_grid$ndc[i]
        year_x <- param_grid$year[i]
        prov_cat_x <-  param_grid$prov_cat[i]
        prov_state_x <-  param_grid$prov_state[i]
        
        sink(sprintf("./result_objects/logs/log_%s.txt", Sys.getpid()))
        print(sprintf("%s %s %s %s", year_x, ndc_x, prov_state_x, prov_cat_x))
        
        f_new_summary <- here::here(
            "result_objects",
            "top_ntile_patients_dx_summary",
            ndc_x,
            sprintf(
                "%s_%s_%s_%s_top_ntile_dx_summary.RDS",
                ndc_x,
                year_x,
                prov_state_x,
                prov_cat_x
            )
        )
        f_new_top_dx <- here::here(
            "result_objects",
            "top_ntile_patients_dx",
            ndc_x,
            sprintf(
                "%s_%s_%s_%s_top_ntile_dx.RDS",
                ndc_x,
                year_x,
                prov_state_x,
                prov_cat_x
            )
        )
        
        if (any(!fs::file_exists(c(f_new_summary, f_new_top_dx)))) {
            sprintf("Creating: %s", f_new_summary)
            
            ## Load top patients for year t
            top_pt <-
                readRDS(here::here(
                    "result_objects",
                    "top_ntile_patients",
                    ndc_x,
                    sprintf(
                        "%s_%s_%s_%s_top_ntile.RDS",
                        ndc_x,
                        year_x,
                        prov_state_x,
                        prov_cat_x
                    )
                ))
            
            ## Load primary diagnoses for year t and t-1
            dx_t <- import_mini_dx_data(year_x, ndc_x) %>%
                dplyr::filter(diag_position == 1)
            if (year_x > 2003) {
                dx_t <- dplyr::bind_rows(
                    dx_t,
                    import_mini_dx_data(year_x - 1, ndc_x) %>%
                        dplyr::filter(diag_position == 1)
                )
            }
            
            ## Loop through the different subsets by weight
            summary_holder <- NULL
            results_holder <- NULL
            for (w_x in winsor_p) {
                ## Subset to just the patients we want
                sub_patid <- top_pt %>%
                    dplyr::filter(
                        w == w_x,
                        ndc_type == ndc_x,
                        year == year_x,
                        prov_cat == prov_cat_x,
                        prov_state == prov_state_x
                    ) %>%
                    dplyr::pull(patid) %>%
                    unique(.)
                
                ## Subset primary diagnoses to just those patients
                sub_dx <- dx_t %>%
                    dplyr::filter(patid %in% sub_patid)
                
                ## Summarize sub_dx
                sub_dx_summary <- sub_dx %>%
                    dplyr::summarize(
                        n_primary_dx = dplyr::n(),
                        n_patid = dplyr::n_distinct(patid),
                        n_unique_diag = dplyr::n_distinct(diag)
                    ) %>%
                    dplyr::mutate(
                        w = w_x,
                        year = year_x,
                        ndc_type = ndc_x,
                        prov_state = prov_state_x,
                        prov_cat = prov_cat_x
                    )
                summary_holder <-
                    dplyr::bind_rows(summary_holder, sub_dx_summary)
                
                if (sub_dx$icd_flag[1] == 9) {
                    sub_dx <- sub_dx %>%
                        dplyr::left_join(icd9ccs %>%
                                             dplyr::select(diag = icd, ccs_desc))
                } else if (sub_dx$icd_flag[1] == 10) {
                    sub_dx <- sub_dx %>%
                        dplyr::left_join(icd10ccs %>%
                                             dplyr::select(diag = icd, ccs_desc))
                }
                
                temp_results <- sub_dx %>%
                    dplyr::group_by(ccs_desc) %>%
                    dplyr::summarize(
                        n_primary_dx = dplyr::n(),
                        n_patids = dplyr::n_distinct(patid)
                    ) %>%
                    dplyr::arrange(dplyr::desc(n_primary_dx))
                
                results <-  dplyr::bind_rows(
                    temp_results %>%
                        dplyr::filter(!is.na(ccs_desc)) %>%
                        dplyr::slice(1:50),
                    dplyr::tibble(
                        ccs_desc = "All primary diagnoses",
                        n_primary_dx = sum(temp_results$n_primary_dx),
                        n_patids = dplyr::n_distinct(sub_dx$patid)
                    ),
                    dplyr::tibble(
                        ccs_desc = "Missing diagnoses",
                        n_primary_dx = sum(is.na(sub_dx$ccs_desc)),
                        n_patids = dplyr::n_distinct(sub_dx$patid)
                    )
                ) %>%
                    dplyr::mutate(
                        w = w_x,
                        year = year_x,
                        ndc_type = ndc_x,
                        prov_state = prov_state_x,
                        prov_cat = prov_cat_x
                    )
                results_holder <-
                    dplyr::bind_rows(results_holder, results)
            }
            
            ## Dir creation
            if (!fs::dir_exists(dirname(f_new_summary))) {
                fs::dir_create(dirname(f_new_summary))
            }
            if (!fs::dir_exists(dirname(f_new_top_dx))) {
                fs::dir_create(dirname(f_new_top_dx))
            }
            
            ## Save intermediate files
            saveRDS(summary_holder, f_new_summary)
            saveRDS(results_holder, f_new_top_dx)
        }
        print(i)
        sink()
    }
doParallel::stopImplicitCluster()

## Collect all the files ----
patient_dx <- purrr::map_df(
    .x = fs::dir_ls(
        here::here("result_objects", "top_ntile_patients_dx"),
        recurse = TRUE,
        glob = "*.RDS"
    ),
    .f = ~ readRDS(.x)
)

dx_summary <- purrr::map_df(
    .x = fs::dir_ls(
        here::here("result_objects", "top_ntile_patients_dx_summary"),
        recurse = TRUE,
        glob = "*.RDS"
    ),
    .f = ~ readRDS(.x)
)

## Rename some of the CCS descriptions so they are consistent across ICD9/10
patient_dx <- patient_dx %>%
    dplyr::mutate(
        ccs_desc_rename = dplyr::case_when(
            ccs_desc == "2ndary malig" ~
                "Secondary malignancies",
            ccs_desc == "Abdomnl pain" ~
                "Abdominal pain",
            ccs_desc == "Breast cancr" ~
                "Cancer of breast",
            ccs_desc == "Brnch/lng ca" ~
                "Cancer of bronchus; lung",
            ccs_desc == "Chr kidney disease" ~ "Chronic kidney disease",
            ccs_desc == "Coag/hemr dx" ~
                "Coagulation and hemorrhagic disorders",
            ccs_desc == "COPD" ~
                "Chronic obstructive pulmonary disease and bronchiectasis",
            ccs_desc == "Coron athero" ~
                "Coronary atherosclerosis and other heart disease",
            ccs_desc == "DiabMel no c" ~
                "Diabetes mellitus without complication",
            ccs_desc == "DiabMel w/cm" ~
                "Diabetes mellitus with complications",
            ccs_desc == "Hrt valve dx" ~ "Heart valve disorders",
            ccs_desc == "HTN" ~ "Essential hypertension",
            ccs_desc == "Immuniz/scrn" ~
                "Immunizations and screening for infectious disease",
            ccs_desc == "Osteoarthros" ~
                "Osteoarthritis",
            ccs_desc == "Ot aftercare" ~ "Other aftercare",
            ccs_desc == "Ot bnign neo" ~
                "Other and unspecified benign neoplasm",
            ccs_desc == "Ot bone dx" ~
                "Other bone disease and musculoskeletal deformities",
            ccs_desc == "Ot conn tiss" ~ "Other connective tissue disease",
            ccs_desc == "Ot joint dx" ~ "Other connective tissue disease",
            ccs_desc == "Ot up rsp in" ~ "Other upper respiratory infections",
            ccs_desc == "Ot uppr resp" ~ "Other upper respiratory disease",
            ccs_desc == "Oth low resp" ~ "Other lower respiratory disease",
            ccs_desc == "Oth nerv dx" ~ "Other nervous system disorders",
            ccs_desc == "Oth skin dx" ~ "Other skin disorders",
            ccs_desc == "Other GI dx" ~ "Other gastrointestinal disorders",
            ccs_desc == "Other screen" ~ "Other screening",
            ccs_desc == "Other screening for suspected conditions (not mental disorders or infectious diseaseccs_desc == )" ~
                "Other screening",
            ccs_desc == "Phlebitis" ~
                "Phlebitis; thrombophlebitis and thromboembolism",
            ccs_desc == "Pulm hart dx" ~ "Pulmonary heart disease",
            ccs_desc == "Residual codes; unclassified" ~ "Unclassified",
            ccs_desc == "Retinal dx" ~
                "Retinal detachments; defects; vascular occlusion; and retinopathy",
            ccs_desc ==
                "Spondylosis; intervertebral disc disorders; other back problems" ~ "Back problem",
            ccs_desc == "Thyroid cncr" ~ "Cancer of thyroid",
            ccs_desc == "Thyroid dsor" ~ "Thyroid disorders",
            ccs_desc == "Ulcer skin" ~ "Chronic ulcer of skin",
            ccs_desc == "UTI" ~ "Urinary tract infections",
            TRUE ~ ccs_desc
        )
    )

## Join with the total diagnoses so we can calculate proportion
patient_dx <- patient_dx %>%
    dplyr::left_join(
        patient_dx %>%
            dplyr::filter(ccs_desc == "All primary diagnoses") %>%
            dplyr::select(all_primary_dx = n_primary_dx,
                          w,
                          year,
                          ndc_type,
                          prov_state,
                          prov_cat)
    ) %>%
    dplyr::mutate(prop_primary_diag = n_primary_dx / all_primary_dx)

## Save them ----
readr::write_csv(patient_dx,
                 here::here("data", "top_ntile_pt_primary_dx.csv"))
readr::write_csv(
    dx_summary,
    here::here(
        "data",
        "descriptives",
        "top_ntile_pt_primary_diag_descriptives.csv"
    )
)
