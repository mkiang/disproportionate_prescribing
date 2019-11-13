## utils.R ----
## Utility functions for the Prolific Prescribing project.
##
## Lots of functions. I try to group them in a way that makes sense. Use
## outline mode in R Studio (shift + command + O) to navigate more easily.

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(vroom)
library(googleCloudStorageR)
library(googleAuthR)
library(ineq)

## Google Cloud helpers ----

#' Return the google cloud storage objects from a specified bucket
#'
#' This assumes that you have gar_gce_auth() set up.
#'
#' @param bucket_name name of the google cloud storage bucket
#' @param f_regex the regex of the files you want to return
#'
#' @return a dataframe with specified bucket objects
return_bucket_objects <-
    function(bucket_name = "som-phs-basus-optum",
             f_regex = NA) {
        googleAuthR::gar_gce_auth()
        x <- googleCloudStorageR::gcs_list_objects(bucket_name)
        
        x <- x %>%
            dplyr::mutate(
                gigabytes = dplyr::case_when(
                    grepl("Mb", size) ~ as.numeric(gsub("Mb", "", size)) / 1000,
                    grepl("Gb", size) ~ as.numeric(gsub("Gb", "", size)),
                    TRUE ~ NA_real_
                )
            )
        
        if (is.na(f_regex)) {
            x
        } else {
            x %>%
                dplyr::filter(grepl(f_regex, name))
        }
    }

## Helpers for the comparison drugs ----

#' Create a dictionary of shortname:regex pattern key:pair values
#'
#' For each comparison drug, return the regex we need for the NDC lookup
#'
#' @return a list
return_ndc_keypairs <- function() {
    list(
        alprazolam = "ALPRAZOLAM",
        cyclobenzaprine = "CYCLOBENZAPRINE HCL",
        gabapentin = "GABAPENTIN",
        buspirone = "BUSPIRONE HCL",
        lorazepam = "LORAZEPAM",
        dextroamphetamine = "DEXTROAMPHETAMINE/AMPHETAMINE",
        methlyphenidate = "METHYLPHENIDATE HCL",
        statins = "SIMVASTATIN",
        levothyroxine = "LEVOTHYROXINE SODIUM",
        warfarin = "WARFARIN SODIUM",
        citalopram = "CITALOPRAM HYDROBROMIDE",
        metformin = "METFORMIN HCL",
        chlordiazepoxide = "CHLORDIAZEPOXIDE HCL",
        diazepam = "DIAZEPAM",
        clonazepam = "CLONAZEPAM",
        oxazepam = "OXAZEPAM",
        temazepam = "TEMAZEPAM",
        triazolam = "TRIAZOLAM"
    )
}

#' Return a vector with the short names of the comparison drugs
#'
#' @return a vector with drug shortnames
return_drug_names <- function() {
    ## Note that we use a different source for the opioids so they
    ## are not in the return_ndc_keypairs() function
    c(names(return_ndc_keypairs()),
      "opioids",
      "schii_opioids",
      "benzos")
}

return_drug_list <- function() {
  list(
    "Opioids" = "opioids",
    "Benzodiazepines" = "benzos", 
    "Alprazolam" = "alprazolam", 
    "Buspirone" = "buspirone", 
    "Chlordiazepoxide" = "chlordiazepoxide", 
    "Citalopram" = "citalopram",
    "Clonazepam" = "clonazepam", 
    "Cyclobenzaprine" = "cyclobenzaprine", 
    "Dextroamphetamine" = "dextroamphetamine",
    "Diazepam" = "diazepam", 
    "Gabapentin" = "gabapentin", 
    "Levothyroxine" = "levothyroxine",
    "Lorazepam" = "lorazepam", 
    "Metformin" = "metformin",
    "Methylphenidate" = "methlyphenidate", 
    "Oxazepam" = "oxazepam", 
    "Schedule II opioids" = "schii_opioids",
    "Simvastatin" = "statins",
    "Temazepam" = "temazepam", 
    "Triazolam" = "triazolam",
    "Warfarin" = "warfarin"
  )
}


## File helpers ----
#' Load NDC dictionary with codes for each drug defined in return_drug_names
#'
#' @return a named list containing the NDCs for all drugs of interest
load_ndc_dictionary <- function() {
    ndcs <- list()
    for (n in return_drug_names()) {
        ndcs[[n]] <-
            readRDS(here::here("data", "ndcs", sprintf("ndc_%s.RDS", n)))
    }
    ndcs
}

#' Count the number of rows in a text file (can be compressed)
#'
#' @param f_path path to a (potentially compressed) text file
#'
#' @return a dataframe with the file name, quarter, year, and number of rows
count_rows <- function(f_path,
                       use_vroom = FALSE,
                       nthreads = 1) {
    if (use_vroom) {
        dplyr::tibble(fpath = basename(f_path),
                      n_rows = length(
                          vroom::vroom_lines(f_path,
                                             num_threads = nthreads,
                                             progress = FALSE)
                      ))
    } else {
        dplyr::tibble(fpath = basename(f_path),
                      n_rows = R.utils::countLines(f_path))
    }
}

#' Extract the quarter and year from a filename / file path
#'
#' @param df_with_fpath a dataframe (likely one resulting from count_rows())
#'
#' @return a modified dataframe
extract_qtr_year <- function(df_with_fpath) {
    r_matches <- regmatches(df_with_fpath$fpath,
                            gregexpr("[[:digit:]]+", df_with_fpath$fpath))
    years <-
        as.numeric(unlist(lapply(r_matches, function(x)
            x[NROW(x) - 1])))
    qtrs <-
        as.numeric(unlist(lapply(r_matches, function(x)
            x[NROW(x)])))
    df_with_fpath %>%
        dplyr::mutate(year = years,
                      quarter = qtrs)
}

#' Load a dataframe with all NDCs and their conversion. Separate out
#' the opioids and the schedule ii opioids.
#'
#' @return a tibble with converion factors
load_mme_conversion <- function() {
    schii_ndc <-
        readRDS(here::here("data", "ndcs", "ndc_schii_opioids.RDS"))
    mme_convers <-
        readr::read_csv(here::here("data", "ndcs", "all_ndcs_with_mme_conversion.csv"))
    
    mme_convers <- dplyr::bind_rows(
        mme_convers,
        mme_convers %>%
            dplyr::filter(ndc %in% schii_ndc) %>%
            dplyr::mutate(drug_type = "schii_opioids")
    ) %>%
        dplyr::mutate(
            drug_type = dplyr::case_when(
                drug_type == "opioid" ~ "opioids",
                drug_type == "statin" ~ "statins",
                TRUE ~ drug_type
            )
        )
    
    mme_convers
}

#' Helper to import any year of subsetted prescription data
#'
#' @param year numeric between 2003 and 2017
#' @param ndc_type one of {citalopram, levothyroxine, metformin, opioids, schii_opioids, statins, warfarin}
#' @param drop_cols drop dea and npi columns after making combined ID
#' @param progress_b display progress of import
#'
#' @return a dataframe of all prescriptions of specified drug type / year
import_mini_rx_data <- function(year,
                                ndc_type,
                                drop_cols = TRUE,
                                progress_b = FALSE,
                                use_vroom = FALSE) {
    f_list <-
        fs::dir_ls(here::here("data_private_mini", "rx", ndc_type),
                   regexp = as.character(year))
    
    if (length(f_list) != 4) {
        return("Invalid year.")
    }
    
    ## Import all files
    if (use_vroom) {
        x <- vroom::vroom(
            f_list,
            delim = ",",
            col_types = "ccccccc",
            progress = progress_b
        )
    } else {
        x <- purrr::map_df(
            .x = f_list,
            .f = ~ readr::read_csv(.x,
                                   col_types = "ccccccc",
                                   progress = progress_b)
        )
    }
    
    ## Basic munging and converting
    x <- x %>%
        tidyr::replace_na(list(dea = "NoID", npi = "NoID")) %>%
        dplyr::mutate(
            dea_npi = paste0(dea, "_", npi),
            quantity = as.numeric(quantity),
            days_sup = as.numeric(days_sup),
            fill_dt = lubridate::ymd(fill_dt),
            year = lubridate::year(fill_dt),
            month = lubridate::month(fill_dt)
        ) %>%
        remove_miscoded_dea_npi()
    
    if (drop_cols) {
        x <- x %>%
            dplyr::select(-dea, -npi, -fill_dt)
    }
    
    return(x)
}

#' Helper to import any year of subsetted diagnostic data
#'
#' @param year numeric between 2003 and 2017
#' @param ndc_type one of the ndc types defined in return_ndc_keypairs
#' @param drop_cols drop columns we won't use (e.g., extract_ym)
#' @param progress_b display progress of import
#'
#' @return a dataframe of all prescriptions of specified drug type / year
import_mini_dx_data <- function(year,
                                ndc_type,
                                drop_cols = TRUE,
                                progress_b = FALSE) {
    f_list <-
        fs::dir_ls(here::here("data_private_mini", "dx", ndc_type),
                   regexp = as.character(year))
    
    if (length(f_list) != 4) {
        return("Invalid year.")
    }
    
    ## Import all files
    x <- purrr::map_df(
        .x = f_list,
        .f = ~ readr::read_csv(.x,
                               col_types = "cccccccccc",
                               progress = progress_b)
    )
    
    ## Basic munging and converting
    x <- x %>%
        dplyr::mutate(diag_position = as.numeric(diag_position),
                      icd_flag = as.numeric(icd_flag))
    
    if (drop_cols) {
        x <- x %>%
            dplyr::select(patid, diag, diag_position, icd_flag)
    }
    
    return(x)
}

#' Delete all log files -- requires user interaction
#'
#' @return none
delete_log_files <- function() {
    user_input <- readline(prompt = "Confirm delete log files (y/n): ")
    if (user_input %in% c("y", "Y", "TRUE", "yes", "Yes", "YES", "true")) {
        fs::file_delete(fs::dir_ls(here::here("result_objects", "logs"), glob = "*.txt"))
    } else {
        print("Nothing deleted.")
    }
}

import_lorenz_curves <-
    function(state_x = c("US"),
             ndc_x = c(
                 "alprazolam",
                 "cyclobenzaprine",
                 "gabapentin",
                 "buspirone",
                 "lorazepam",
                 "dextroamphetamine",
                 "methlyphenidate",
                 "opioids"
             ),
             year_x = 2017,
             prov_cat_x = "all_types") {
        f_list <- fs::dir_ls(
            here::here("data", "lorenz", state_x),
            recurse = TRUE,
            regexp = paste(
                sprintf("%s_%i_%s_%s", ndc_x, year_x, state_x, prov_cat_x),
                collapse = "|"
            )
        )
        
        purrr::map_df(.x = f_list,
                      .f = ~ readRDS(.x)) %>%
            ## Note, we do a filter because regex picks up schii_opioids
            dplyr::filter(ndc_type %in% ndc_x)
    }

import_ntile_mme_file <- function(prov_state_x = "US",
                                  prov_cat_x = "all_types",
                                  use_vroom = FALSE) {
    f_path <- sprintf(here::here(
        "data",
        "mme_ntiles",
        sprintf(
            "%s_%s_mme_ntile_summary.csv.xz",
            prov_state_x,
            prov_cat_x
        )
    ))
    
    if (use_vroom) {
        x <- vroom::vroom(f_path, progress = FALSE)
    } else {
        x <- readr::read_csv(f_path, progress = FALSE)
    }
    return(x)
}

## Return a 1-column tibble of dea_npi for each year/ndc
import_deanpis <- function(ndc_type, year_x) {
    dea_files <-
        fs::dir_ls(
            sprintf("./result_objects/tabulations_mini/%s/",
                    ndc_type),
            regexp = sprintf("dea_tabs_%sq[1234]{1}.RDS", year_x)
        )
    x <- purrr::map(.x = dea_files,
                    .f = ~ names(readRDS(.x))) %>%
        purrr::reduce(c) %>%
        unique()
    tibble(dea_npi = x) %>%
        remove_miscoded_dea_npi()
}

## MME Ntile helpers ----
reshape_ntile_mme_to_long <- function(ntile_df) {
    dplyr::bind_rows(
        ntile_df %>%
            dplyr::select(-mean_patid) %>%
            dplyr::select(
                year,
                w,
                prov_state,
                prov_cat,
                top_ntile,
                n_prov,
                n_patid,
                ndc_type,
                dplyr::starts_with("mean_")
            ) %>%
            tidyr::gather(meas, value, mean_mme:mean_n_prescriptions) %>%
            dplyr::mutate(
                stat_type = "mean",
                meas = gsub("mean_", "", meas, fixed = TRUE)
            ),
        ntile_df %>%
            dplyr::select(-mean_patid) %>%
            dplyr::select(
                year,
                w,
                prov_state,
                prov_cat,
                top_ntile,
                n_prov,
                n_patid,
                ndc_type,
                dplyr::starts_with("total_")
            ) %>%
            tidyr::gather(meas, value, total_mme:total_n_prescriptions) %>%
            dplyr::mutate(
                stat_type = "total",
                meas = gsub("total_", "", meas, fixed = TRUE)
            ),
        ntile_df %>%
            dplyr::select(-mean_patid) %>%
            dplyr::select(
                year,
                w,
                prov_state,
                prov_cat,
                top_ntile,
                n_prov,
                n_patid,
                ndc_type,
                dplyr::starts_with("med_")
            ) %>%
            tidyr::gather(meas, value, med_mme:med_n_prescriptions) %>%
            dplyr::mutate(
                stat_type = "median",
                meas = gsub("med_", "", meas, fixed = TRUE)
            ),
        ntile_df %>%
            dplyr::select(-mean_patid) %>%
            dplyr::select(
                year,
                w,
                prov_state,
                prov_cat,
                top_ntile,
                n_prov,
                n_patid,
                ndc_type,
                dplyr::starts_with("sd_")
            ) %>%
            tidyr::gather(meas, value, sd_mme:sd_n_prescriptions) %>%
            dplyr::mutate(
                stat_type = "sd",
                meas = gsub("sd_", "", meas, fixed = TRUE)
            )
    )
}

extract_ntile_comparison <- function(sub_df, comp_ntile_x = 50) {
    sub_df %>%
        dplyr::ungroup() %>%
        dplyr::filter(top_ntile == comp_ntile_x) %>%
        dplyr::select(year,
                      w,
                      prov_state,
                      prov_cat,
                      ndc_type,
                      stat_type,
                      meas,
                      comp_value = value)
}

ntile_subset <- function(ntile_df,
                         w_x = 0,
                         ndc_type_x = c("opioids", "statins"),
                         prov_state_x = "US",
                         prov_cat_x = "all_types") {
    ntile_df %>%
        dplyr::filter(
            w == w_x,
            ndc_type %in% ndc_type_x,
            prov_state == prov_state_x,
            prov_cat == prov_cat_x
        ) %>%
        reshape_ntile_mme_to_long() %>%
        categorize_meas() %>%
        categorize_ndc() %>%
        categorize_prov_cat() %>%
        categorize_prov_state() %>%
        categorize_w() %>%
        categorize_stat()
}

## Grid helpers ----
return_small_grid <- function(shuffle = TRUE) {
    ## If you want to look by provider specialty, must use whole country
    ## or missing values. If by state, must use all specialties.
    small_grid <- dplyr::bind_rows(
        expand.grid(
            specialty = c(
                "prev_internal_family",
                "all_surgery",
                "cat_of_interest",
                "critical_care",
                "emergency",
                "family",
                "gen_surgery",
                "hospice",
                "internal",
                "obgyn",
                "ortho",
                "pediatrics",
                "pmnr",
                "plastics"
            ),
            state = c("US", NA),
            stringsAsFactors = FALSE
        ),
        expand.grid(
            specialty = "all_types",
            state = c(datasets::state.abb, "DC"),
            stringsAsFactors = FALSE
        )
    )
    if (shuffle) {
        small_grid <- small_grid[sample(1:NROW(small_grid)),]
    }
    small_grid
}

return_large_grid <- function(shuffle = TRUE) {
    large_grid <- expand.grid(
        specialty = "all_types",
        year = 2003:2017,
        state = c("US", NA),
        ndc_type = return_drug_names(),
        stringsAsFactors = FALSE
    )
    if (shuffle) {
        large_grid <- large_grid[sample(1:NROW(large_grid)),]
    }
    large_grid
}

return_full_grid <- function(shuffle = TRUE) {
    param_grid_full <- dplyr::bind_rows(
        expand.grid(
            specialty = c(
                "prev_internal_family",
                "all_surgery",
                "cat_of_interest",
                "critical_care",
                "emergency",
                "family",
                "gen_surgery",
                "hospice",
                "internal",
                "obgyn",
                "ortho",
                "pediatrics",
                "pmnr",
                "plastics"
            ),
            year = 2003:2017,
            state = c("US", NA),
            ndc_type = return_drug_names(),
            stringsAsFactors = FALSE
        ),
        expand.grid(
            specialty = "all_types",
            year = 2003:2017,
            state = c("US", NA, datasets::state.abb, "DC"),
            ndc_type = return_drug_names(),
            stringsAsFactors = FALSE
        )
    )
    if (shuffle) {
        param_grid_full <-
            param_grid_full[sample(1:NROW(param_grid_full)),]
    }
    param_grid_full
}

## Tabulation helpers ----
#' Summarize a `table()` object
#'
#' @param table_object
#'
#' @return a summary tibble
summarize_tabulation <- function(table_object) {
    dplyr::tibble(
        n_rows = sum(table_object),
        n_unique = length(table_object),
        min = min(table_object),
        max = max(table_object),
        mean = mean(table_object),
        sd = stats::sd(table_object),
        median = stats::median(table_object),
        p025 = stats::quantile(table_object, .025),
        p050 = stats::quantile(table_object, .05),
        p250 = stats::quantile(table_object, .25),
        p750 = stats::quantile(table_object, .75),
        p950 = stats::quantile(table_object, .95),
        p975 = stats::quantile(table_object, .975)
    )
}

## Dataframe helpers ----
add_active_prescribers <- function(df, active_prescribers, year_x) {
    df %>%
        dplyr::left_join(
            active_prescribers %>%
                dplyr::filter(year == year_x) %>%
                dplyr::select(dea_npi, total_rx = n_prescriptions),
            by = c("dea_npi")
        )
}

#' Merge dataframe with provider states and specialties
#'
#' @param df left df for join
#' @param prov_st_cat right df for join (with provider data)
#'
#' @return a new dataframe with state and specialty data
add_state_cat <- function(df, prov_st_cat) {
    df %>%
        dplyr::left_join(prov_st_cat %>%
                             dplyr::filter(duplicated_id == 0),
                         by = "dea_npi")
}

#' Join provider state/specialty with an Rx dataframe
#'
#' @param orig_df original Rx dataframe
#' @param state state to subset to (can be NA)
#' @param prov_cat provider specialty to subset to (can be NA)
#' @param prov_st_cat dataframe of provider state/specialty to join
#'
#' @return dataframe, potentially with additional columns
subset_df <- function(orig_df, state, prov_cat, prov_st_cat) {
    ## We sometimes need to subset the full data into provider or state but
    ## never both provider and state.
    
    ## Check if there is any subsetting at all.
    if (state != "US" & !is.na(state)) {
        temp_df <- orig_df %>%
            add_state_cat(., prov_st_cat) %>%
            dplyr::filter(prov_state == state)
    } else if (is.na(state)) {
        temp_df <- orig_df %>%
            add_state_cat(., prov_st_cat) %>%
            dplyr::filter(is.na(state))
    } else if (state == "US" &
               !is.na(prov_cat) & prov_cat != "all_types") {
        temp_df <- orig_df %>%
            dplyr::left_join(
                prov_st_cat %>%
                    dplyr::select(dea_npi, critical_care:cat_of_interest) %>%
                    dplyr::distinct(),
                by = "dea_npi"
            )
    } else if (state == "US" & prov_cat == "all_types") {
        return(orig_df)
    }
    
    ## Physician cat subsetting
    if (prov_cat == "all_types") {
        temp_df <- temp_df
    } else if (prov_cat != "all_types" & !is.na(prov_cat)) {
        temp_df <- temp_df %>%
            dplyr::filter(UQ(as.name(prov_cat)) == 1)
    } else if (is.na(prov_cat)) {
        temp_df <- temp_df %>%
            dplyr::filter(is.na(prov_cat))
    }
    
    return(temp_df)
}

## Trimming helpers ----
#' Replace all values above top_p with NA
#'
#' @param x numeric vector
#' @param top_p top percentile to be converted to NA
#'
#' @return a vector of length(x) with NA values above top_p quantile
upper_trim <- function(x, top_p = .01) {
    ul_val <- stats::quantile(x, p = (1 - top_p), na.rm = TRUE)
    x[x > ul_val] <- NA
    return(x)
}

#' Trim all quantities of interest with upper trim at level w
#'
#' @param df dataframe with mme, mme_per_pt, etc.
#' @param w percentile to trim
#'
#' @return modified dataframe with NA for values above w percentile
trim_mmes <- function(df, w) {
    df %>%
        dplyr::mutate(
            mme = upper_trim(mme, top_p = w),
            mme_per_day = upper_trim(mme_per_day, top_p = w),
            mme_per_pt = upper_trim(mme_per_pt, top_p = w),
            mme_per_ptday = upper_trim(mme_per_ptday, top_p = w),
            mme_per_prescription = upper_trim(mme_per_prescription, top_p = w)
        )
}

#' Trim all quantities of interest with upper trim at level w
#'
#' @param df dataframe that has gone through `summarize_mmes_patients()`
#' @param w percentile to trim
#'
#' @return modified dataframe with NA for values above w percentile
trim_mmes_patients <- function(df, w) {
    df %>%
        dplyr::mutate(
            mme = upper_trim(mme, top_p = w),
            mme_per_day = upper_trim(mme_per_day, top_p = w),
            mme_per_prescriber = upper_trim(mme_per_prescriber, top_p = w),
            mme_per_prescriberday = upper_trim(mme_per_prescriberday, top_p = w),
            mme_per_prescription = upper_trim(mme_per_prescription, top_p = w)
        )
}

#' Trim all quantities of interest with upper trim at level w
#'
#' @param df dataframe that has gone through `summarize_mmes_pairs()`
#' @param w percentile to trim
#'
#' @return modified dataframe with NA for values above w percentile
trim_mmes_pairs <- function(df, w) {
    df %>%
        dplyr::mutate(
            mme = upper_trim(mme, top_p = w),
            mme_per_day = upper_trim(mme_per_day, top_p = w),
            mme_per_prescription = upper_trim(mme_per_prescription, top_p = w)
        )
}

trim_bottom_prescribers <- function(df, w) {
    df %>%
        filter(total_rx >= w)
}

## MME helpers ----
#' Calculate the MMEs of a specified drug using a conversion data frame
#'
#' @param df dataframe of a *single* specified drug
#' @param mme_convers_df conversion dataframe (with multiple drugs is ok)
#' @param ndc_type_x NDC of drug to combine
#'
#' @return dataframe with MME conversion columns
calculate_mmes <-
    function(df, mme_convers_df, ndc_type_x) {
        df %>%
            dplyr::left_join(
                mme_convers_df %>%
                    dplyr::filter(drug_type == ndc_type_x) %>%
                    dplyr::select(-drug_type) %>%
                    dplyr::distinct(),
                by = "ndc"
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(mme = quantity * strength_per_unit * mme_conversion_factor) %>%
            dplyr::filter(mme > 0)
    }

#' Summarize the MMEs in a dataframe (after calculating them)
#'
#' @param df a dataframe that has already run through `calculate_mmes()`
#' @param ... grouping variables
#'
#' @return a smaller (summarized) dataframe
summarize_mmes <- function(df, main_grp = dea_npi, ...) {
    grps <- dplyr::quos(...)
    
    df %>%
        dplyr::filter(dea_npi != "NoID_NoID") %>%
        remove_miscoded_dea_npi() %>%
        dplyr::filter(quantity > 0,
                      days_sup > 0) %>%
        dplyr::group_by({
            {
                main_grp
            }
        }) %>%
        dplyr::group_by(!!!grps, add = TRUE) %>%
        calc_mme_summaries() %>%
        dplyr::ungroup() %>%
        calc_normalized_mme()
}

#' Summarize the MMEs each patient received
#'
#' @param df a dataframe that has already run through `calculate_mmes()`
#' @param ... grouping variables
#'
#' @return a smaller (summarized) dataframe
summarize_mmes_patients <- function(df, ...) {
    grps <- dplyr::quos(...)
    
    df %>%
        dplyr::filter(dea_npi != "NoID_NoID") %>%
        dplyr::filter(quantity > 0,
                      days_sup > 0) %>%
        dplyr::group_by(patid) %>%
        dplyr::group_by(!!!grps, add = TRUE) %>%
        dplyr::summarize(
            mme_per_day = sum(mme / days_sup, na.rm = TRUE),
            quantity = sum(quantity, na.rm = TRUE),
            mme = sum(mme, na.rm = TRUE),
            n_deanpis = dplyr::n_distinct(dea_npi),
            n_prescriptions = dplyr::n(),
            n_days = sum(days_sup, na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            mme_per_prescriber = mme / n_deanpis,
            mme_per_prescriberday = mme_per_day / n_deanpis,
            mme_per_prescription = mme / n_prescriptions
        )
}

#' Summarize the MMEs each provider-patient pair received
#'
#' @param df a dataframe that has already run through `calculate_mmes()`
#' @param ... grouping variables
#'
#' @return a smaller (summarized) dataframe
summarize_mmes_pairs <- function(df, ...) {
    grps <- dplyr::quos(...)
    
    df %>%
        dplyr::filter(dea_npi != "NoID_NoID") %>%
        dplyr::filter(quantity > 0,
                      days_sup > 0) %>%
        dplyr::group_by(patid, dea_npi) %>%
        dplyr::group_by(!!!grps, add = TRUE) %>%
        dplyr::summarize(
            mme_per_day = sum(mme / days_sup, na.rm = TRUE),
            quantity = sum(quantity, na.rm = TRUE),
            mme = sum(mme, na.rm = TRUE),
            n_prescriptions = dplyr::n(),
            n_days = sum(days_sup, na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            mme_per_day = mme / n_days,
            mme_per_prescription = mme / n_prescriptions
        )
}

calculate_mme_summaries <-
    function(orig_df,
             year_x,
             ndc_x,
             state_x,
             prov_cat_x,
             mme_convers_x = mme_convers,
             prov_st_cat_x = prov_st_cat,
             active_prescribers_x = active_prescribers,
             winsor_p_x = winsor_p,
             min_rx_x = min_rx) {
        ## File name
        f_mme_summary <-
            here::here(
                "result_objects",
                "mme_summary",
                state_x,
                ndc_x,
                sprintf(
                    "%s_%i_%s_%s_mme_summary.RDS",
                    ndc_x,
                    year_x,
                    state_x,
                    prov_cat_x
                )
            )
        
        ## Make dirs -- NOTE: This conditional must be here or sometimes
        ## your whole directory will be replaced (losing old work).
        if (!fs::dir_exists(here::here("result_objects", "mme_summary",
                                       state_x, ndc_x))) {
            fs::dir_create(here::here("result_objects", "mme_summary", state_x, ndc_x))
        }
        
        ## If we already have this file, skip it
        if (!fs::file_exists(f_mme_summary)) {
            ## Set up log file
            sink(sprintf("./result_objects/logs/log_%s.txt", Sys.getpid()))
            print(sprintf("%s %s %s %s", year_x, ndc_x, state_x, prov_cat_x))
            
            ## Subset
            temp_df <- subset_df(
                orig_df = orig_df,
                state = state_x,
                prov_cat = prov_cat_x,
                prov_st_cat = prov_st_cat_x
            )
            rm(orig_df)
            gc2()
            
            ## Calculate and summarize MMEs at provider level
            temp_df <- temp_df %>%
                calculate_mmes(
                    df = .,
                    mme_convers_df = mme_convers_x,
                    ndc_type_x = ndc_x
                ) %>%
                summarize_mmes() %>%
                add_active_prescribers(active_prescribers_x, year_x)
            
            ## Loop through different trimming levels
            mme_results <- NULL
            for (w_x in winsor_p_x) {
                print(w_x)
                trimmed_df <- temp_df %>%
                    trim_mmes(., w = w_x)
                
                mme_results <- dplyr::bind_rows(
                    mme_results,
                    mme_distribution(df = trimmed_df) %>%
                        dplyr::mutate(
                            w = w_x,
                            year = year_x,
                            prov_state = state_x,
                            prov_cat = prov_cat_x,
                            ndc_type = ndc_x,
                            trim_type = "upper_mme"
                        )
                )
                rm(trimmed_df)
                gc2()
            }
            
            ## Loop through different trimming levels
            ## NOTE: We create df in the loop above so you MUST run it.
            for (w_x in min_rx_x) {
                print(w_x)
                trimmed_df <- temp_df %>%
                    trim_bottom_prescribers(., w = w_x)
                
                mme_results <- dplyr::bind_rows(
                    mme_results,
                    mme_distribution(df = trimmed_df) %>%
                        dplyr::mutate(
                            w = w_x,
                            year = year_x,
                            prov_state = state_x,
                            prov_cat = prov_cat_x,
                            ndc_type = ndc_x,
                            trim_type = "bottom_prescriptions"
                        )
                )
                rm(trimmed_df)
                gc2()
            }
            
            ## Save and close log
            saveRDS(mme_results, f_mme_summary, compress = "xz")
            rm(temp_df)
            gc2()
            sink()
        } else {
            return(sprintf("Skipping %s", f_mme_summary))
        }
    }

load_and_calculate_mme_summaries <-
    function(year_x,
             ndc_x,
             state_x,
             prov_cat_x,
             mme_convers_x = mme_convers,
             prov_st_cat_x = prov_st_cat,
             active_prescribers_x = active_prescribers,
             winsor_p_x = winsor_p,
             min_rx_x = min_rx) {
        ## File name
        f_mme_summary <-
            here::here(
                "result_objects",
                "mme_summary",
                state_x,
                ndc_x,
                sprintf(
                    "%s_%i_%s_%s_mme_summary.RDS",
                    ndc_x,
                    year_x,
                    state_x,
                    prov_cat_x
                )
            )
        
        if (!fs::file_exists(f_mme_summary)) {
            ## Import year of data
            orig_df <- import_mini_rx_data(year_x, ndc_x)
            
            calculate_mme_summaries(
                orig_df,
                year_x,
                ndc_x,
                state_x,
                prov_cat_x,
                mme_convers_x = mme_convers_x,
                prov_st_cat_x = prov_st_cat_x,
                active_prescribers_x = active_prescribers_x,
                winsor_p_x = winsor_p_x,
                min_rx_x = min_rx_x
            )
        } else {
            return(sprintf("Skipping %s", f_mme_summary))
        }
    }

## Inequality measures helpers ----
#' Calculate a variety of inequality measures
#'
#' @param x a numeric vector
#'
#' @return a tibble summarizing inequality in this vector
calculate_ineq_measures <- function(x) {
    if (length(stats::na.omit(x)) > 10) {
        temp_x <- dplyr::tibble(
            gini = ineq::Gini(x, corr = TRUE, na.rm = TRUE),
            ricci_schutz = ineq::RS(x, na.rm = TRUE),
            atkinson = ineq::Atkinson(x, na.rm = TRUE),
            theil = ineq::Theil(x, na.rm = TRUE),
            entropy = ineq::entropy(x, na.rm = TRUE),
            herfindahl = ineq::Herfindahl(x, na.rm = TRUE),
            rosenbluth = ineq::Rosenbluth(x, na.rm = TRUE)
        )
    } else {
        temp_x <- dplyr::tibble(
            gini = NA,
            ricci_schutz = NA,
            atkinson = NA,
            theil = NA,
            entropy = NA,
            herfindahl = NA,
            rosenbluth = NA
        )
    }
    return(temp_x)
}

#' Calculate inequality measures across all vectors of interest
#'
#' @param df a dataframe with mme, mme_per_day, mme_per_pt, etc.
#'
#' @return a tibble with a summary of the inequality for all measures
calculate_all_ineq_measures <- function(df) {
    ineq_meas <- dplyr::bind_rows(
        calculate_ineq_measures(df$mme),
        calculate_ineq_measures(df$mme_per_day),
        calculate_ineq_measures(df$mme_per_pt),
        calculate_ineq_measures(df$mme_per_ptday),
        calculate_ineq_measures(df$mme_per_prescription),
        calculate_ineq_measures(df$n_prescriptions)
    ) %>%
        dplyr::mutate(
            meas = c(
                "mme",
                "mme_per_day",
                "mme_per_pt",
                "mme_per_ptday",
                "mme_per_prescription",
                "n_prescriptions"
            )
        )
    
    alt_n <- dplyr::tibble(
        n_patients = c(
            df %>%
                dplyr::filter(!is.na(mme)) %>%
                dplyr::pull(n_patients) %>%
                sum(),
            df %>%
                dplyr::filter(!is.na(mme_per_day)) %>%
                dplyr::pull(n_patients) %>%
                sum(),
            df %>%
                dplyr::filter(!is.na(mme_per_pt)) %>%
                dplyr::pull(n_patients) %>%
                sum(),
            df %>%
                dplyr::filter(!is.na(mme_per_ptday)) %>%
                dplyr::pull(n_patients) %>%
                sum(),
            df %>%
                dplyr::filter(!is.na(mme_per_prescription)) %>%
                dplyr::pull(n_patients) %>%
                sum(),
            df %>%
                dplyr::filter(!is.na(n_prescriptions)) %>%
                dplyr::pull(n_patients) %>%
                sum()
        ),
        n_prescriptions = c(
            df %>%
                dplyr::filter(!is.na(mme)) %>%
                dplyr::pull(n_prescriptions) %>%
                sum(),
            df %>%
                dplyr::filter(!is.na(mme_per_day)) %>%
                dplyr::pull(n_prescriptions) %>%
                sum(),
            df %>%
                dplyr::filter(!is.na(mme_per_pt)) %>%
                dplyr::pull(n_prescriptions) %>%
                sum(),
            df %>%
                dplyr::filter(!is.na(mme_per_ptday)) %>%
                dplyr::pull(n_prescriptions) %>%
                sum(),
            df %>%
                dplyr::filter(!is.na(mme_per_prescription)) %>%
                dplyr::pull(n_prescriptions) %>%
                sum(),
            df %>%
                dplyr::filter(!is.na(n_prescriptions)) %>%
                dplyr::pull(n_prescriptions) %>%
                sum()
        ),
        n_deanpi = c(
            sum(!is.na(df$mme), na.rm = TRUE),
            sum(!is.na(df$mme_per_day), na.rm = TRUE),
            sum(!is.na(df$mme_per_pt), na.rm = TRUE),
            sum(!is.na(df$mme_per_ptday), na.rm = TRUE),
            sum(!is.na(df$mme_per_prescription), na.rm = TRUE),
            sum(!is.na(df$n_prescriptions), na.rm = TRUE)
        ),
        meas = c(
            "mme",
            "mme_per_day",
            "mme_per_pt",
            "mme_per_ptday",
            "mme_per_prescription",
            "n_prescriptions"
        )
    )
    alt_n %>%
        dplyr::left_join(ineq_meas, by = "meas")
}

#' Calculate all inequality measures
#'
#' @param orig_df a full data with calculate_mme() and summarize_mme()
#' @param year_x year to subset
#' @param ndc_x ndc code to subset
#' @param state_x state to subset
#' @param prov_cat_x provider type to subset
#' @param mme_convers_x mme conversion dataframe
#' @param prov_st_cat_x provider/state information dataframe
#' @param winsor_p_x vector of trimming values
#'
#' @return
#' @export
#'
#' @examples
calculate_ineq_measures_results <-
    function(orig_df,
             year_x,
             ndc_x,
             state_x,
             prov_cat_x,
             mme_convers_x = mme_convers,
             prov_st_cat_x = prov_st_cat,
             active_prescribers_x = active_prescribers,
             winsor_p_x = winsor_p,
             min_rx_x = min_rx) {
        ## File name
        f_alt <-
            here::here(
                "result_objects",
                "ineq_meas",
                state_x,
                ndc_x,
                sprintf(
                    "%s_%i_%s_%s_ineq_meas.RDS",
                    ndc_x,
                    year_x,
                    state_x,
                    prov_cat_x
                )
            )
        
        ## Make dirs -- NOTE: This conditional must be here or sometimes
        ## your whole directory will be replaced (losing old work).
        if (!fs::dir_exists(here::here("result_objects", "ineq_meas", state_x, ndc_x))) {
            fs::dir_create(here::here("result_objects", "ineq_meas", state_x, ndc_x))
        }
        
        ## If we already have this file, skip it
        if (!fs::file_exists(f_alt)) {
            ## Set up log file
            sink(sprintf("./result_objects/logs/log_%s.txt", Sys.getpid()))
            print(sprintf("%s %s %s %s", year_x, ndc_x, state_x, prov_cat_x))
            
            ## Subset
            temp_df <- subset_df(
                orig_df = orig_df,
                state = state_x,
                prov_cat = prov_cat_x,
                prov_st_cat = prov_st_cat_x
            )
            rm(orig_df)
            gc2()
            
            ## Calculate and summarize MMEs at provider level
            temp_df <- temp_df %>%
                calculate_mmes(
                    df = .,
                    mme_convers_df = mme_convers_x,
                    ndc_type_x = ndc_x
                ) %>%
                summarize_mmes() %>%
                add_active_prescribers(active_prescribers_x, year_x)
            
            ## Loop through different trimming levels
            ineq_meas_results <- NULL
            for (w_x in winsor_p_x) {
                print(w_x)
                trimmed_df <- temp_df %>%
                    trim_mmes(., w = w_x)
                
                ineq_meas_results <- dplyr::bind_rows(
                    ineq_meas_results,
                    calculate_all_ineq_measures(df = trimmed_df) %>%
                        dplyr::mutate(
                            w = w_x,
                            year = year_x,
                            prov_state = state_x,
                            prov_cat = prov_cat_x,
                            ndc_type = ndc_x,
                            trim_type = "upper_mme"
                        )
                )
                rm(trimmed_df)
                gc2()
            }
            
            ## Loop through different trimming levels
            ## NOTE: We create df in the loop above so you MUST run it.
            for (w_x in min_rx_x) {
                print(w_x)
                trimmed_df <- temp_df %>%
                    trim_bottom_prescribers(., w = w_x)
                
                ineq_meas_results <- dplyr::bind_rows(
                    ineq_meas_results,
                    calculate_all_ineq_measures(df = trimmed_df) %>%
                        dplyr::mutate(
                            w = w_x,
                            year = year_x,
                            prov_state = state_x,
                            prov_cat = prov_cat_x,
                            ndc_type = ndc_x,
                            trim_type = "bottom_prescriptions"
                        )
                )
                rm(trimmed_df)
                gc2()
            }
            
            ## Save and close log
            rm(temp_df)
            gc2()
            saveRDS(ineq_meas_results, f_alt, compress = "xz")
            sink()
        } else {
            return(sprintf("Skipping %s", f_alt))
        }
    }

load_and_calculate_ineq_measures_results <-
    function(year_x,
             ndc_x,
             state_x,
             prov_cat_x,
             mme_convers_x = mme_convers,
             prov_st_cat_x = prov_st_cat,
             active_prescribers_x = active_prescribers,
             winsor_p_x = winsor_p,
             min_rx_x = min_rx) {
        ## File name
        f_alt <-
            here::here(
                "result_objects",
                "ineq_meas",
                state_x,
                ndc_x,
                sprintf(
                    "%s_%i_%s_%s_ineq_meas.RDS",
                    ndc_x,
                    year_x,
                    state_x,
                    prov_cat_x
                )
            )
        
        if (!fs::file_exists(f_alt)) {
            ## Import year of data
            orig_df <- import_mini_rx_data(year_x, ndc_x)
            
            calculate_ineq_measures_results(
                orig_df,
                year_x,
                ndc_x,
                state_x,
                prov_cat_x,
                mme_convers_x = mme_convers_x,
                prov_st_cat_x = prov_st_cat_x,
                active_prescribers_x = active_prescribers_x,
                winsor_p_x = winsor_p,
                min_rx_x = min_rx_x
            )
        } else {
            return(sprintf("Skipping %s", f_alt))
        }
    }

gini_bootstrap <- function(x,
                           conf_level = .95,
                           reps = 5000) {
    gini_ix <- function(x, ix) {
        ineq::Gini(x[ix])
    }
    
    booted_x <- boot::boot(data = x,
                           statistic = gini_ix,
                           R = reps)
    booted_ci <-
        boot::boot.ci(booted_x, conf = conf_level, type = "perc")
    
    tibble::tibble(
        gini = booted_ci$t0,
        gini_lower = booted_ci$percent[1, 4],
        gini_upper = booted_ci$percent[1, 5],
        gini_reps = reps
    )
}

calculate_bootstrapped_gini <-
    function(orig_df,
             year_x,
             ndc_x,
             state_x,
             prov_cat_x,
             mme_convers_x = mme_convers,
             prov_st_cat_x = prov_st_cat,
             reps_x = 1000) {
        ## File name
        f_alt <-
            here::here(
                "result_objects",
                "bootstrap_gini",
                state_x,
                ndc_x,
                sprintf(
                    "%s_%i_%s_%s_ineq_meas.RDS",
                    ndc_x,
                    year_x,
                    state_x,
                    prov_cat_x
                )
            )
        
        ## Make dirs -- NOTE: This conditional must be here or sometimes
        ## your whole directory will be replaced (losing old work).
        if (!fs::dir_exists(here::here("result_objects", "bootstrap_gini",
                                       state_x, ndc_x))) {
            fs::dir_create(here::here("result_objects", "bootstrap_gini",
                                      state_x, ndc_x))
        }
        
        ## If we already have this file, skip it
        if (!fs::file_exists(f_alt)) {
            ## Set up log file
            sink(sprintf("./result_objects/logs/log_%s.txt", Sys.getpid()),
                 append = TRUE)
            print(sprintf("%s %s %s %s", year_x, ndc_x, state_x, prov_cat_x))
            print(Sys.time())
            
            ## Subset
            temp_df <- subset_df(
                orig_df = orig_df,
                state = state_x,
                prov_cat = prov_cat_x,
                prov_st_cat = prov_st_cat_x
            )
            rm(orig_df)
            gc2()
            
            ## Calculate and summarize MMEs at provider level
            temp_df <- temp_df %>%
                calculate_mmes(
                    df = .,
                    mme_convers_df = mme_convers_x,
                    ndc_type_x = ndc_x
                ) %>%
                summarize_mmes()
            
            if (length(stats::na.omit(temp_df$mme)) > 10) {
                bootstrapped_gini <- dplyr::bind_rows(
                    gini_bootstrap(x = temp_df$mme, reps = reps_x) %>%
                        mutate(meas = "mme"),
                    gini_bootstrap(x = temp_df$mme_per_day, reps = reps_x) %>%
                        mutate(meas = "mme_per_day"),
                    gini_bootstrap(x = temp_df$mme_per_pt, reps = reps_x) %>%
                        mutate(meas = "mme_per_pt"),
                    gini_bootstrap(
                        x = temp_df$mme_per_prescription,
                        reps = reps_x
                    ) %>%
                        mutate(meas = "mme_per_prescription"),
                    gini_bootstrap(x = temp_df$n_prescriptions, reps = reps_x) %>%
                        mutate(meas = "n_prescriptions")
                )
            } else {
                bootstrapped_gini <- tibble(
                    gini = NA,
                    gini_lower = NA,
                    gini_upper = NA,
                    gini_reps = 0,
                    meas = c(
                        "mme",
                        "mme_per_day",
                        "mme_per_pt",
                        "mme_per_prescription",
                        "n_prescriptions"
                    )
                )
            }
            
            bootstrapped_gini <- bootstrapped_gini %>%
                mutate(
                    year = year_x,
                    prov_state = state_x,
                    ndc_type = ndc_x,
                    n_rows = NROW(temp_df$mme)
                )
            
            ## Save and close log
            rm(temp_df)
            gc2()
            saveRDS(bootstrapped_gini, f_alt, compress = "xz")
            sink()
        } else {
            return(sprintf("Skipping %s", f_alt))
        }
    }

load_and_calculate_bootstrapped_gini <-
    function(year_x,
             ndc_x,
             state_x,
             prov_cat_x,
             mme_convers_x = mme_convers,
             prov_st_cat_x = prov_st_cat,
             reps_x = 5000) {
        ## File name
        f_alt <-
            here::here(
                "result_objects",
                "bootstrap_gini",
                state_x,
                ndc_x,
                sprintf(
                    "%s_%i_%s_%s_ineq_meas.RDS",
                    ndc_x,
                    year_x,
                    state_x,
                    prov_cat_x
                )
            )
        
        if (!fs::file_exists(f_alt)) {
            ## Import year of data
            orig_df <- import_mini_rx_data(year_x, ndc_x)
            
            calculate_bootstrapped_gini(
                orig_df,
                year_x,
                ndc_x,
                state_x,
                prov_cat_x,
                mme_convers_x = mme_convers_x,
                prov_st_cat_x = prov_st_cat_x,
                reps_x = reps_x
            )
        } else {
            return(sprintf("Skipping %s", f_alt))
        }
    }

## Lorenz curve helpers ----
#' Given a vector x, return the lorenz curve and generalized lorenz curve
#'
#' @param x numeric vector of positive values
#' @param length_out how long the lorenz curve should be (resolution)
#'
#' @return a dataframe with lorenz curves of specified length (or shorter)
calculate_lorenz_curve <- function(x, length_out = 500) {
    ## ineq::Lc() returns a vector of length(x + 1), which is generally
    ## far larger than we need. Instead, I will just take every nth estimate
    ## such that the vector will be no more than length_out elements.
    
    keep_bool <- TRUE
    
    if (NROW(x) < 10) {
        dplyr::tibble(p = NA,
                      Lp = NA,
                      Lgen = NA)
    } else if (NROW(x) > length_out) {
        keep_bool <- as.integer(round(c(
            1, seq(0L, NROW(x),
                   length.out = length_out + 1)[-1]
        )))
    }
    
    temp_x <- ineq::Lc(x)
    dplyr::tibble(p = temp_x$p[keep_bool],
                  Lp = temp_x$L[keep_bool],
                  Lgen = temp_x$L.general[keep_bool]) %>%
        dplyr::filter(!is.na(p))
}

#' Calculate all lorenz curves of vectors we are interested in
#'
#' @param df dataframe of aggregated MMEs
#' @param length_out maximum length (resolution) of the lorenz curve
#'
#' @return dataframe of lorenz curves in long format
calculate_all_lorenz_curves <- function(df, length_out = 500) {
    dplyr::bind_rows(
        calculate_lorenz_curve(df$mme,
                               length_out = length_out) %>%
            dplyr::mutate(meas = "mme"),
        calculate_lorenz_curve(df$mme_per_day,
                               length_out = length_out) %>%
            dplyr::mutate(meas = "mme_per_day"),
        calculate_lorenz_curve(df$mme_per_pt,
                               length_out = length_out) %>%
            dplyr::mutate(meas = "mme_per_pt"),
        calculate_lorenz_curve(df$mme_per_prescription,
                               length_out = length_out) %>%
            dplyr::mutate(meas = "mme_per_prescription"),
        calculate_lorenz_curve(df$mme_per_ptday,
                               length_out = length_out) %>%
            dplyr::mutate(meas = "mme_per_ptday"),
        calculate_lorenz_curve(df$n_prescriptions,
                               length_out = length_out) %>%
            dplyr::mutate(meas = "n_prescriptions")
    )
}

calculate_lorenz_results <-
    function(orig_df,
             year_x,
             ndc_x,
             state_x,
             prov_cat_x,
             mme_convers_x = mme_convers,
             prov_st_cat_x = prov_st_cat,
             active_prescribers_x = active_prescribers,
             winsor_p_x = winsor_p,
             min_rx_x = min_rx) {
        ## File name
        f_lorenz <-
            here::here(
                "result_objects",
                "lorenz",
                state_x,
                ndc_x,
                sprintf(
                    "%s_%i_%s_%s_lorenz_results.RDS",
                    ndc_x,
                    year_x,
                    state_x,
                    prov_cat_x
                )
            )
        
        ## Make dirs -- NOTE: This conditional must be here or sometimes
        ## your whole directory will be replaced (losing old work).
        if (!fs::dir_exists(here::here("result_objects", "lorenz", state_x, ndc_x))) {
            fs::dir_create(here::here("result_objects", "lorenz", state_x, ndc_x))
        }
        
        ## If we already have this file, skip it
        if (!fs::file_exists(f_lorenz)) {
            ## Set up log file
            sink(sprintf("./result_objects/logs/log_%s.txt", Sys.getpid()))
            print(sprintf("%s %s %s %s", year_x, ndc_x, state_x, prov_cat_x))
            
            ## Subset
            temp_df <- subset_df(
                orig_df = orig_df,
                state = state_x,
                prov_cat = prov_cat_x,
                prov_st_cat = prov_st_cat_x
            )
            rm(orig_df)
            gc2()
            
            ## Calculate and summarize MMEs at provider level
            temp_df <- temp_df %>%
                calculate_mmes(
                    df = .,
                    mme_convers_df = mme_convers_x,
                    ndc_type_x = ndc_x
                ) %>%
                summarize_mmes() %>%
                add_active_prescribers(active_prescribers_x, year_x)
            
            ## Loop through different trimming levels
            lorenz_results <- NULL
            for (w_x in winsor_p_x) {
                print(w_x)
                trimmed_df <- temp_df %>%
                    trim_mmes(., w = w_x)
                
                lorenz_results <- dplyr::bind_rows(
                    lorenz_results,
                    calculate_all_lorenz_curves(df = trimmed_df) %>%
                        dplyr::mutate(
                            w = w_x,
                            year = year_x,
                            prov_state = state_x,
                            prov_cat = prov_cat_x,
                            ndc_type = ndc_x,
                            trim_type = "upper_mme"
                        )
                )
                rm(trimmed_df)
                gc2()
            }
            
            ## Loop through different trimming levels
            ## NOTE: We create df in the loop above so you MUST run it.
            for (w_x in min_rx_x) {
                print(w_x)
                trimmed_df <- temp_df %>%
                    trim_bottom_prescribers(., w = w_x)
                
                lorenz_results <- dplyr::bind_rows(
                    lorenz_results,
                    calculate_all_lorenz_curves(df = trimmed_df) %>%
                        dplyr::mutate(
                            w = w_x,
                            year = year_x,
                            prov_state = state_x,
                            prov_cat = prov_cat_x,
                            ndc_type = ndc_x,
                            trim_type = "bottom_prescriptions"
                        )
                )
                rm(trimmed_df)
                gc2()
            }
            
            rm(temp_df)
            gc()
            saveRDS(lorenz_results, f_lorenz, compress = "xz")
            sink()
            
        } else {
            return(sprintf("Skipping %s", f_lorenz))
        }
    }

load_and_calculate_lorenz_results <-
    function(year_x,
             ndc_x,
             state_x,
             prov_cat_x,
             mme_convers_x = mme_convers,
             prov_st_cat_x = prov_st_cat,
             active_prescribers_x = active_prescribers,
             winsor_p_x = winsor_p,
             min_rx_x = min_rx) {
        ## File name
        f_lorenz <-
            here::here(
                "result_objects",
                "lorenz",
                state_x,
                ndc_x,
                sprintf(
                    "%s_%i_%s_%s_lorenz_results.RDS",
                    ndc_x,
                    year_x,
                    state_x,
                    prov_cat_x
                )
            )
        
        if (!fs::file_exists(f_lorenz)) {
            ## Import year of data
            orig_df <- import_mini_rx_data(year_x, ndc_x)
            
            calculate_lorenz_results(
                orig_df,
                year_x,
                ndc_x,
                state_x,
                prov_cat_x,
                mme_convers_x = mme_convers_x,
                prov_st_cat_x = prov_st_cat_x,
                active_prescribers_x = active_prescribers_x,
                winsor_p_x = winsor_p_x,
                min_rx_x = min_rx_x
            )
        } else {
            return(sprintf("Skipping %s", f_lorenz))
        }
    }

#' Given a lorenz curve, flag the points closest to top 1, 5, 10, 50%
#'
#' @param df lorenz curve dataframe
#'
#' @return a dataframe with an indicator if top 1, 5, 10, 50% datapoint
flag_landmarks <- function(df) {
    df %>% dplyr::mutate(
        top_presc =
            dplyr::case_when(
                flag_closest_val(p1, .01) ~ 1,
                flag_closest_val(p1, .05) ~ 1,
                flag_closest_val(p1, .1) ~ 1,
                flag_closest_val(p1, .5) ~ 1,
                TRUE ~ 0
            )
    )
}

#' Mirrors a lorenz curve around the 45 degree line to make it MD-friendly
#'
#' @param df a lorenz curve dataframe
#'
#' @return a dataframe with mirrored columns
mirror_lorenz_curve <- function(df) {
    df %>%
        dplyr::mutate(p1 = 1 - p,
                      Lp1 = 1 - Lp)
}

#' Add factor labels to top 1, 5, 10, 50% datapoints
#'
#' @param df
#'
#' @return factor with top 1, 5, 10, 50%
make_p1_labels <- function(df) {
    df %>%
        dplyr::mutate(p1_round = sprintf("%0.2f", round(1 - p, 2))) %>%
        dplyr::mutate(p1_lab = factor(
            p1_round,
            levels = c("0.01", "0.05", "0.10", "0.25", "0.50"),
            labels = c("Top 1%", "Top 5%", "Top 10%", "Top 25%", "Top 50%"),
            ordered = TRUE
        ))
}

#' Add text labels to Lp1
#'
#' @param df
#'
#' @return dataframe with text labels for Lp1
make_Lp1_labels <- function(df) {
    df %>%
        dplyr::mutate(Lp1_lab = sprintf("%0.2f",
                                        round(Lp1, 2)))
}

downsample_lorenz_curve <-
    function(df, p1_vals =  (c(0:25, seq(26, 100, 2))) / 100) {
        grid_df <- df %>%
            dplyr::count(meas,
                         w,
                         year,
                         prov_cat,
                         prov_state,
                         ndc_type,
                         trim_type) %>%
            dplyr::filter(n > length(p1_vals))
        
        if (NROW(grid_df) > 0) {
            holder <- vector("list", length = NROW(grid_df))
            for (i in 1:NROW(grid_df)) {
                sub_df <- df %>%
                    dplyr::filter(
                        meas == grid_df$meas[i],
                        w == grid_df$w[i],
                        year == grid_df$year[i],
                        prov_cat == grid_df$prov_cat[i],
                        prov_state == grid_df$prov_state[i],
                        ndc_type == grid_df$ndc_type[i],
                        trim_type == grid_df$trim_type[i]
                    ) %>%
                    mirror_lorenz_curve() %>%
                    dplyr::filter(flag_all_closest_vals(p1, p1_vals))
                
                if (NROW(sub_df) > 0) {
                    holder[[i]] <- sub_df
                }
            }
            dplyr::bind_rows(holder)
        } else {
            NULL
        }
    }

import_lorenz_top_p_only <- function(f_path) {
    temp_df <- readRDS(f_path)
    if (NROW(temp_df) > 0) {
        temp_df %>%
            dplyr::group_by(meas,
                            w,
                            year,
                            prov_state,
                            prov_cat,
                            ndc_type,
                            trim_type) %>%
            dplyr::mutate(
                flag_val =
                    dplyr::case_when(
                        flag_closest_val(p1, .01) ~ 1,
                        flag_closest_val(p1, .05) ~ 1,
                        flag_closest_val(p1, .1) ~ 1,
                        flag_closest_val(p1, .25) ~ 1,
                        flag_closest_val(p1, .5) ~ 1,
                        TRUE ~ 0
                    )
            ) %>%
            dplyr::filter(flag_val == 1) %>%
            dplyr::select(-flag_val) %>%
            dplyr::ungroup()
    } else {
        return(NULL)
    }
}

## Summarize MMEs by ntile ----
summarize_mme_by_ntile <- function(df) {
    df %>%
        dplyr::mutate(top_ntile = 101 - dplyr::ntile(mme, 100)) %>%
        dplyr::group_by(top_ntile) %>%
        dplyr::summarize(
            n_prov = dplyr::n_distinct(dea_npi),
            n_patid = sum(n_patients, na.rm = TRUE),
            mean_patid = mean(n_patients, na.rm = TRUE),
            mean_mme = mean(mme, na.rm = TRUE),
            mean_mme_per_day = mean(mme_per_day, na.rm = TRUE),
            mean_mme_per_pt = mean(mme_per_pt, na.rm = TRUE),
            mean_mme_per_ptday = mean(mme_per_ptday, na.rm = TRUE),
            mean_mme_per_prescription = mean(mme_per_prescription, na.rm = TRUE),
            mean_n_prescriptions = mean(n_prescriptions, na.rm = TRUE),
            total_mme = sum(mme, na.rm = TRUE),
            total_mme_per_day = sum(mme_per_day, na.rm = TRUE),
            total_mme_per_pt = sum(mme_per_pt, na.rm = TRUE),
            total_mme_per_ptday = sum(mme_per_ptday, na.rm = TRUE),
            total_mme_per_prescription = sum(mme_per_prescription, na.rm = TRUE),
            total_n_prescriptions = sum(n_prescriptions, na.rm = TRUE),
            med_mme = stats::median(mme, na.rm = TRUE),
            med_mme_per_day = stats::median(mme_per_day, na.rm = TRUE),
            med_mme_per_pt = stats::median(mme_per_pt, na.rm = TRUE),
            med_mme_per_ptday = stats::median(mme_per_ptday, na.rm = TRUE),
            med_mme_per_prescription = stats::median(mme_per_prescription, na.rm = TRUE),
            med_n_prescriptions = stats::median(n_prescriptions, na.rm = TRUE),
            sd_mme = stats::sd(mme, na.rm = TRUE),
            sd_mme_per_day = stats::sd(mme_per_day, na.rm = TRUE),
            sd_mme_per_pt = stats::sd(mme_per_pt, na.rm = TRUE),
            sd_mme_per_ptday = stats::sd(mme_per_ptday, na.rm = TRUE),
            sd_mme_per_prescription = stats::sd(mme_per_prescription, na.rm = TRUE),
            sd_n_prescriptions = stats::sd(n_prescriptions, na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(top_ntile))
}

calculate_ntile_mmes <-
    function(orig_df,
             year_x,
             ndc_x,
             state_x,
             prov_cat_x,
             mme_convers_x = mme_convers_x,
             prov_st_cat_x = prov_st_cat_x,
             active_prescribers_x = active_prescribers_x,
             winsor_p_x = winsor_p,
             min_rx_x = min_rx) {
        ## File name
        f_ntile_mme <-
            here::here(
                "result_objects",
                "mme_ntiles",
                state_x,
                ndc_x,
                sprintf(
                    "%s_%i_%s_%s_mme_ntiles.RDS",
                    ndc_x,
                    year_x,
                    state_x,
                    prov_cat_x
                )
            )
        
        ## Make dirs -- NOTE: This conditional must be here or sometimes
        ## your whole directory will be replaced (losing old work).
        if (!fs::dir_exists(here::here("result_objects", "mme_ntiles",
                                       state_x, ndc_x))) {
            fs::dir_create(here::here("result_objects", "mme_ntiles", state_x, ndc_x))
        }
        
        ## If we already have this file, skip it
        if (!fs::file_exists(f_ntile_mme)) {
            ## Set up log file
            sink(sprintf("./result_objects/logs/log_%s.txt", Sys.getpid()))
            print(sprintf("%s %s %s %s", year_x, ndc_x, state_x, prov_cat_x))
            
            ## Subset
            temp_df <- subset_df(
                orig_df = orig_df,
                state = state_x,
                prov_cat = prov_cat_x,
                prov_st_cat = prov_st_cat_x
            )
            rm(orig_df)
            gc2()
            
            ## Calculate and summarize MMEs at provider level
            temp_df <- temp_df %>%
                calculate_mmes(
                    df = .,
                    mme_convers_df = mme_convers_x,
                    ndc_type_x = ndc_x
                ) %>%
                summarize_mmes() %>%
                add_active_prescribers(active_prescribers_x, year_x)
            
            ## Loop through different trimming levels
            ntile_mme_results <- NULL
            for (w_x in winsor_p_x) {
                print(w_x)
                trimmed_df <- temp_df %>%
                    trim_mmes(., w = w_x)
                
                ntile_mme_results <- dplyr::bind_rows(
                    ntile_mme_results,
                    summarize_mme_by_ntile(df = trimmed_df) %>%
                        dplyr::mutate(
                            w = w_x,
                            year = year_x,
                            prov_state = state_x,
                            prov_cat = prov_cat_x,
                            ndc_type = ndc_x,
                            trim_type = "upper_mme"
                        )
                )
                rm(trimmed_df)
                gc2()
            }
            
            ## Loop through different trimming levels
            ## NOTE: We create df in the loop above so you MUST run it.
            for (w_x in min_rx_x) {
                print(w_x)
                trimmed_df <- temp_df %>%
                    trim_bottom_prescribers(., w = w_x)
                
                ntile_mme_results <- dplyr::bind_rows(
                    ntile_mme_results,
                    summarize_mme_by_ntile(df = trimmed_df) %>%
                        dplyr::mutate(
                            w = w_x,
                            year = year_x,
                            prov_state = state_x,
                            prov_cat = prov_cat_x,
                            ndc_type = ndc_x,
                            trim_type = "bottom_prescriptions"
                        )
                )
                rm(trimmed_df)
                gc2()
            }
            
            ## Save and close log
            rm(temp_df)
            gc2()
            saveRDS(ntile_mme_results, f_ntile_mme, compress = "xz")
            sink()
        } else {
            return(sprintf("Skipping %s", f_ntile_mme))
        }
    }

load_and_calculate_ntile_mmes <-
    function(year_x,
             ndc_x,
             state_x,
             prov_cat_x,
             mme_convers_x = mme_convers,
             prov_st_cat_x = prov_st_cat,
             active_prescribers_x = active_prescribers,
             winsor_p_x = winsor_p,
             min_rx_x = min_rx) {
        ## File name
        f_ntile_mme <-
            here::here(
                "result_objects",
                "mme_ntiles",
                state_x,
                ndc_x,
                sprintf(
                    "%s_%i_%s_%s_mme_ntiles.RDS",
                    ndc_x,
                    year_x,
                    state_x,
                    prov_cat_x
                )
            )
        
        if (!fs::file_exists(f_ntile_mme)) {
            ## Import year of data
            orig_df <- import_mini_rx_data(year_x, ndc_x)
            
            calculate_ntile_mmes(
                orig_df,
                year_x,
                ndc_x,
                state_x,
                prov_cat_x,
                mme_convers_x = mme_convers_x,
                prov_st_cat_x = prov_st_cat_x,
                active_prescribers_x = active_prescribers_x,
                winsor_p_x = winsor_p,
                min_rx_x = min_rx_x
            )
        } else {
            return(sprintf("Skipping %s", f_ntile_mme))
        }
    }

## Extract top n-tile patients, providers, pairs ----
filter_top1_ntile <- function(df) {
    df %>%
        dplyr::mutate(top_ntile = 101 - dplyr::ntile(mme, 100)) %>%
        dplyr::filter(top_ntile == 1) %>%
        dplyr::select(-top_ntile)
}

extract_top_ntile_providers <-
    function(subsetted_df,
             year_x,
             ndc_x,
             state_x,
             prov_cat_x,
             mme_convers_x = mme_convers,
             prov_st_cat_x = prov_st_cat,
             winsor_p_x = winsor_p) {
        ## File name
        f_top_ntile <-
            here::here(
                "result_objects",
                "top_ntile",
                ndc_x,
                sprintf(
                    "%s_%i_%s_%s_top_ntile.RDS",
                    ndc_x,
                    year_x,
                    state_x,
                    prov_cat_x
                )
            )
        
        ## Make dirs -- NOTE: This conditional must be here or sometimes
        ## your whole directory will be replaced (losing old work).
        if (!fs::dir_exists(here::here("result_objects", "top_ntile", ndc_x))) {
            fs::dir_create(here::here("result_objects", "top_ntile", ndc_x))
        }
        
        ## If we already have this file, skip it
        if (!fs::file_exists(f_top_ntile)) {
            ## Set up log file
            sink(sprintf("./result_objects/logs/log_%s.txt", Sys.getpid()))
            print(sprintf("%s %s %s %s", year_x, ndc_x, state_x, prov_cat_x))
            
            ## Calculate and summarize MMEs at provider level
            temp_df <- subsetted_df %>%
                summarize_mmes()
            
            ## Loop through different trimming levels
            top_ntile_results <- NULL
            for (w_x in winsor_p_x) {
                print(w_x)
                trimmed_df <- temp_df %>%
                    trim_mmes(., w = w_x)
                
                top_ntile_results <- dplyr::bind_rows(
                    top_ntile_results,
                    filter_top1_ntile(df = trimmed_df) %>%
                        dplyr::mutate(
                            w = w_x,
                            year = year_x,
                            prov_state = state_x,
                            prov_cat = prov_cat_x,
                            ndc_type = ndc_x
                        )
                )
                rm(trimmed_df)
                gc2()
            }
            
            ## Save and close log
            rm(temp_df)
            gc2()
            saveRDS(top_ntile_results, f_top_ntile, compress = "xz")
            sink()
        } else {
            return(sprintf("Skipping %s", f_top_ntile))
        }
    }

extract_top_ntile_patients <-
    function(subsetted_df,
             year_x,
             ndc_x,
             state_x,
             prov_cat_x,
             mme_convers_x = mme_convers,
             prov_st_cat_x = prov_st_cat,
             winsor_p_x = winsor_p) {
        ## File name
        f_top_ntile_pt <-
            here::here(
                "result_objects",
                "top_ntile_patients",
                ndc_x,
                sprintf(
                    "%s_%i_%s_%s_top_ntile.RDS",
                    ndc_x,
                    year_x,
                    state_x,
                    prov_cat_x
                )
            )
        
        ## Make dirs -- NOTE: This conditional must be here or sometimes
        ## your whole directory will be replaced (losing old work).
        if (!fs::dir_exists(here::here("result_objects", "top_ntile_patients", ndc_x))) {
            fs::dir_create(here::here("result_objects", "top_ntile_patients", ndc_x))
        }
        
        ## If we already have this file, skip it
        if (!fs::file_exists(f_top_ntile_pt)) {
            ## Set up log file
            sink(sprintf("./result_objects/logs/log_%s.txt", Sys.getpid()))
            print(sprintf("%s %s %s %s", year_x, ndc_x, state_x, prov_cat_x))
            
            ## Calculate and summarize MMEs at provider level
            temp_df <- subsetted_df %>%
                summarize_mmes_patients()
            
            ## Loop through different trimming levels
            top_ntile_results <- NULL
            for (w_x in winsor_p_x) {
                print(w_x)
                trimmed_df <- temp_df %>%
                    trim_mmes_patients(., w = w_x)
                
                top_ntile_results <- dplyr::bind_rows(
                    top_ntile_results,
                    filter_top1_ntile(df = trimmed_df) %>%
                        dplyr::mutate(
                            w = w_x,
                            year = year_x,
                            prov_state = state_x,
                            prov_cat = prov_cat_x,
                            ndc_type = ndc_x
                        )
                )
                rm(trimmed_df)
                gc2()
            }
            
            ## Save and close log
            rm(temp_df)
            gc2()
            saveRDS(top_ntile_results, f_top_ntile_pt, compress = "xz")
            sink()
        } else {
            return(sprintf("Skipping %s", f_top_ntile_pt))
        }
    }

extract_top_ntile_pairs <-
    function(subsetted_df,
             year_x,
             ndc_x,
             state_x,
             prov_cat_x,
             mme_convers_x = mme_convers,
             prov_st_cat_x = prov_st_cat,
             winsor_p_x = winsor_p) {
        ## File name
        f_top_ntile_pair <-
            here::here(
                "result_objects",
                "top_ntile_pairs",
                ndc_x,
                sprintf(
                    "%s_%i_%s_%s_top_ntile_pair.RDS",
                    ndc_x,
                    year_x,
                    state_x,
                    prov_cat_x
                )
            )
        
        ## Make dirs -- NOTE: This conditional must be here or sometimes
        ## your whole directory will be replaced (losing old work).
        if (!fs::dir_exists(here::here("result_objects", "top_ntile_pairs", ndc_x))) {
            fs::dir_create(here::here("result_objects", "top_ntile_pairs", ndc_x))
        }
        
        ## If we already have this file, skip it
        if (!fs::file_exists(f_top_ntile_pair)) {
            ## Set up log file
            sink(sprintf("./result_objects/logs/log_%s.txt", Sys.getpid()))
            print(sprintf("%s %s %s %s", year_x, ndc_x, state_x, prov_cat_x))
            
            ## Calculate and summarize MMEs at provider level
            temp_df <- subsetted_df %>%
                summarize_mmes_pairs()
            
            ## Loop through different trimming levels
            top_ntile_results <- NULL
            for (w_x in winsor_p_x) {
                print(w_x)
                trimmed_df <- temp_df %>%
                    trim_mmes_pairs(., w = w_x)
                
                top_ntile_results <- dplyr::bind_rows(
                    top_ntile_results,
                    filter_top1_ntile(df = trimmed_df) %>%
                        dplyr::mutate(
                            w = w_x,
                            year = year_x,
                            prov_state = state_x,
                            prov_cat = prov_cat_x,
                            ndc_type = ndc_x
                        )
                )
                rm(trimmed_df)
                gc2()
            }
            
            ## Save and close log
            rm(temp_df)
            gc2()
            saveRDS(top_ntile_results, f_top_ntile_pair, compress = "xz")
            sink()
        } else {
            return(sprintf("Skipping %s", f_top_ntile_pair))
        }
    }

## Ntile to Ntile transactions ----
get_dea_ntiles <- function(orig_df, collapse_ntile = 11) {
    ## NOTE: This collapses AT AND ABOVE the collapse_ntile value.
    orig_df %>%
        summarize_mmes() %>%
        dplyr::mutate(n_tile = 101 - dplyr::ntile(mme, 100)) %>%
        dplyr::transmute(
            dea_npi = dea_npi,
            dea_ntile = dplyr::case_when(
                n_tile < collapse_ntile ~ n_tile,
                n_tile >= collapse_ntile ~ collapse_ntile,
                TRUE ~ NA_real_
            )
        )
}

get_patid_ntiles <- function(orig_df, collapse_ntile = 11) {
    ## NOTE: This collapses AT AND ABOVE the collapse_ntile value.
    orig_df %>%
        summarize_mmes_patients() %>%
        dplyr::mutate(n_tile = 101 - dplyr::ntile(mme, 100)) %>%
        dplyr::transmute(
            patid = patid,
            patid_ntile = dplyr::case_when(
                n_tile < collapse_ntile ~ n_tile,
                n_tile >= collapse_ntile ~ collapse_ntile,
                TRUE ~ NA_real_
            )
        )
}

tabulate_ntile_transactions <-
    function(orig_df,
             year_x,
             ndc_x,
             state_x = "US",
             prov_cat_x = "all_types",
             mme_convers_x = mme_convers) {
        ## Get MMEs
        orig_df <- orig_df %>%
            calculate_mmes(df = .,
                           mme_convers_df = mme_convers_x,
                           ndc_type_x = ndc_x)
        
        ## Get ntiles of providers and patients
        dea_ntiles <- get_dea_ntiles(orig_df)
        patid_ntiles <- get_patid_ntiles(orig_df)
        
        ## Join back to original dataframe
        orig_df <- orig_df %>%
            dplyr::left_join(dea_ntiles, by = "dea_npi") %>%
            dplyr::left_join(patid_ntiles, by = "patid")
        
        ## Summarize ntile to ntile transactions
        orig_df %>%
            dplyr::group_by(dea_ntile, patid_ntile) %>%
            dplyr::summarize(
                days_sup = sum(days_sup),
                prescriptions = dplyr::n(),
                mme = sum(mme)
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
                year = year_x,
                ndc_type = ndc_x,
                prov_cat = prov_cat_x,
                prov_state = state_x
            )
    }

collapse_ntiles <- function(transaction_df,
                            dea_maxntile = 2,
                            patid_maxntile = 2) {
    transaction_df %>%
        mutate(
            dea_ntile = ifelse(dea_ntile < dea_maxntile,
                               dea_ntile,
                               dea_maxntile),
            patid_ntile = ifelse(patid_ntile < patid_maxntile,
                                 patid_ntile,
                                 patid_maxntile)
        ) %>%
        group_by(year,
                 prov_cat,
                 ndc_type,
                 dea_ntile,
                 patid_ntile,
                 prov_state) %>%
        summarize_all(sum) %>%
        mutate(
            dea_ntile_cat = factor(
                dea_ntile,
                levels = 1:dea_maxntile,
                labels = c(1:(dea_maxntile - 1), "All others"),
                ordered = TRUE
            ),
            patid_ntile_cat = factor(
                patid_ntile,
                levels = 1:patid_maxntile,
                labels = c(1:(patid_maxntile - 1), "All others"),
                ordered = TRUE
            )
        )
}

## Tabulate top ntile provider types ----
tabulate_provider_types <- function(ntile_df, prov_st_cat_x) {
    tab_df <- ntile_df %>%
        dplyr::left_join(prov_st_cat_x, by = "dea_npi") %>%
        dplyr::pull(desc) %>%
        table(., useNA = "always")
    
    dplyr::tibble(desc = names(tab_df),
                  freq = as.vector(tab_df))
}

## Tabulate prop CDC guildelines by ntile ----
tabulate_cdc_guidelines <-
    function(orig_df,
             year_x,
             ndc_x,
             state_x = "US",
             prov_cat_x = "all_types",
             mme_convers_x = mme_convers) {
        ## Get MMEs
        orig_df <- orig_df %>%
            calculate_mmes(df = .,
                           mme_convers_df = mme_convers_x,
                           ndc_type_x = ndc_x) %>%
            dplyr::filter(mme > 0,
                          days_sup > 0) %>%
            dplyr::mutate(mme_per_day = mme / days_sup)
        
        ## Get ntiles of providers
        dea_ntiles <-  orig_df %>%
            summarize_mmes() %>%
            dplyr::mutate(dea_ntile = 101 - dplyr::ntile(mme, 100)) %>%
            dplyr::select(dea_npi, dea_ntile)
        
        ## Join back to original dataframe
        orig_df <- orig_df %>%
            dplyr::left_join(dea_ntiles, by = "dea_npi")
        
        ## Summarize ntile to ntile transactions
        orig_df %>%
            dplyr::group_by(dea_ntile) %>%
            dplyr::summarize(
                n_prescriptions = dplyr::n(),
                n_deanpi = dplyr::n_distinct(dea_npi),
                n_patients = dplyr::n_distinct(patid),
                prop_days_sup_3 = mean(days_sup > 3, na.rm = TRUE),
                prop_days_sup_7 = mean(days_sup > 7, na.rm = TRUE),
                prop_days_sup_14 = mean(days_sup > 14, na.rm = TRUE),
                prop_days_sup_21 = mean(days_sup > 21, na.rm = TRUE),
                prop_days_sup_30 = mean(days_sup > 30, na.rm = TRUE),
                prop_mme_per_day_50 = mean(mme_per_day >= 50, na.rm = TRUE),
                prop_mme_per_day_90 = mean(mme_per_day >= 90, na.rm = TRUE)
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
                year = year_x,
                ndc_type = ndc_x,
                prov_cat = prov_cat_x,
                prov_state = state_x
            )
    }

tabulate_cdc_guidelines_patients <-
  function(orig_df,
           year_x,
           ndc_x,
           state_x = "US",
           prov_cat_x = "all_types",
           mme_convers_x = mme_convers) {
    ## Get MMEs
    orig_df <- orig_df %>%
      calculate_mmes(df = .,
                     mme_convers_df = mme_convers_x,
                     ndc_type_x = ndc_x) %>%
      dplyr::filter(mme > 0,
                    days_sup > 0) %>%
      dplyr::mutate(mme_per_day = mme / days_sup)
    
    ## Get ntiles of patients based on mmes received per year
    patid_ntiles <-  orig_df %>%
      summarize_mmes_patients() %>%
      dplyr::mutate(patid_ntile = 101 - dplyr::ntile(mme, 100)) %>%
      dplyr::select(patid, patid_ntile)
    
    ## Join back to original dataframe
    orig_df <- orig_df %>%
      dplyr::left_join(patid_ntiles, by = "patid")
    
    ## Summarize ntile to ntile transactions
    orig_df %>%
      dplyr::group_by(patid_ntile) %>%
      dplyr::summarize(
        n_prescriptions = dplyr::n(),
        n_deanpi = dplyr::n_distinct(dea_npi),
        n_patients = dplyr::n_distinct(patid),
        prop_days_sup_3 = mean(days_sup > 3, na.rm = TRUE),
        prop_days_sup_7 = mean(days_sup > 7, na.rm = TRUE),
        prop_days_sup_14 = mean(days_sup > 14, na.rm = TRUE),
        prop_days_sup_21 = mean(days_sup > 21, na.rm = TRUE),
        prop_days_sup_30 = mean(days_sup > 30, na.rm = TRUE),
        prop_mme_per_day_50 = mean(mme_per_day >= 50, na.rm = TRUE),
        prop_mme_per_day_90 = mean(mme_per_day >= 90, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        year = year_x,
        ndc_type = ndc_x,
        prov_cat = prov_cat_x,
        prov_state = state_x
      )
  }

## Tabulate and rank drugs ----
summarize_rank_by_drug <- function(df, year_x) {
    df %>%
        dplyr::group_by(generic_name) %>%
        dplyr::summarize(
            n_prescriptions = dplyr::n(),
            n_deanpi = dplyr::n_distinct(dea_npi),
            n_patid = dplyr::n_distinct(patid),
            days_sup = sum(days_sup),
            quantity = sum(quantity),
            mme = sum(mme)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            prescription_rank = dplyr::min_rank(dplyr::desc(n_prescriptions)),
            mme_rank = dplyr::min_rank(dplyr::desc(mme)),
            days_sup_rank = dplyr::min_rank(dplyr::desc(days_sup)),
            quantity_rank = dplyr::min_rank(dplyr::desc(quantity)),
            provider_rank = dplyr::min_rank(dplyr::desc(n_deanpi)),
            patient_rank = dplyr::min_rank(dplyr::desc(n_patid))
        ) %>%
        dplyr::mutate(year = year_x)
}

## Overlap / Jaccard helpers ----
jaccard_index <- function(x, y) {
    inter_xy <- NROW(base::intersect(x, y))
    inter_xy / (NROW(x) + NROW(y) - inter_xy)
}

overlap_xy <- function(x, y) {
    mean(x %in% y)
}

#' Given a dataframe, calculate the overlap between two specified years.
#'
#' NOTE: Jaccard is symmetrical so we could make this faster by only looping
#' year_i > year_j for Jaccard and looping over all years for overlap, but
#' that seems like unnecessary complexity for a minor analysis. You only need
#' to run this once.
#'
#' @param ntiles_df
#' @param year_i
#' @param year_j
#' @param ndc_x
#' @param w_x
#' @param prov_state_x
#' @param prov_cat_x
#' @param target_col
#'
#' @return
#' @export
#'
#' @examples
return_overlap <-
    function(ntiles_df,
             year_i,
             year_j,
             ndc_x,
             w_x,
             prov_state_x,
             prov_cat_x,
             target_col) {
        id_i <- ntiles_df %>%
            dplyr::filter(
                year == year_i,
                ndc_type == ndc_x,
                w == w_x,
                prov_state == prov_state_x,
                prov_cat == prov_cat_x
            ) %>%
            dplyr::pull({
                {
                    target_col
                }
            })
        
        id_j <- ntiles_df %>%
            dplyr::filter(
                year == year_j,
                ndc_type == ndc_x,
                w == w_x,
                prov_state == prov_state_x,
                prov_cat == prov_cat_x
            ) %>%
            dplyr::pull({
                {
                    target_col
                }
            })
        
        dplyr::tibble(
            year_i = year_i,
            year_j = year_j,
            overlap_object = dplyr::quo_name(dplyr::enquo(target_col)),
            ndc_type = ndc_x,
            w = w_x,
            prov_state = prov_state_x,
            prov_cat = prov_cat_x,
            n_i = NROW(id_i),
            n_j = NROW(id_j),
            jaccard_ij = jaccard_index(id_i, id_j),
            overlap_ij = overlap_xy(id_i, id_j)
        )
    }

## Factor/plotting helpers ----
categorize_w <- function(df) {
    df %>%
        dplyr::mutate(w_cat = factor(
            w,
            levels = c(0, .001, .005, .01, .025, .05, .10,
                       1, 3, 6, 12, 24, 50, 100, 200),
            labels = c(
                "Untrimmed",
                "Top 0.1%",
                "Top 0.5%",
                "Top 1%",
                "Top 2.5%",
                "Top 5%",
                "Top 10%",
                "Untrimmed ",
                "3+ Rx/year",
                "6+ Rx/year",
                "12+ Rx/year",
                "24+ Rx/year",
                "50+ Rx/year",
                "100+ Rx/year",
                "200+ Rx/year"
            ),
            ordered = TRUE
        ))
}

categorize_meas <- function(df) {
    df %>%
        dplyr::mutate(meas_cat = factor(
            meas,
            levels = c(
                "mme",
                "mme_per_day",
                "mme_per_pt",
                "mme_per_prescription",
                "mme_per_ptday",
                "n_prescriptions"
            ),
            labels = c(
                "Doses",
                "Dose per day",
                "Dose per patient",
                "Dose per prescription",
                "Dose per patient-day",
                "Prescriptions"
            ),
            ordered = TRUE
        ))
}

categorize_trim_type <- function(df) {
    df %>%
        dplyr::mutate(trim_cat = factor(
            trim_type,
            levels = c("upper_trim", "bottom_prescriptions"),
            labels = c("Trim top doses", "Trim bottom prescribers"),
            ordered = TRUE
        ))
}

categorize_prov_cat <- function(df) {
    df %>%
        dplyr::mutate(specialty_cat = factor(
            prov_cat,
            levels = c(
                "all_types",
                "family",
                "internal",
                "pediatrics",
                "obgyn",
                "emergency",
                "pmnr",
                "critical_care",
                "gen_surgery",
                "ortho",
                "plastics",
                "all_surgery",
                "prev_internal_family",
                "cat_of_interest"
            ),
            labels = c(
                "All",
                "Family medicine",
                "Internal medicine",
                "Pediatrics",
                "Ob / Gyn",
                "Emergency medicine",
                "PM and R",
                "Critical care",
                "General surgery",
                "Orthopedic surgery",
                "Plastic surgery",
                "All surgery",
                "Preventive / Internal / Family",
                "All categories of interest"
            ),
            ordered = TRUE
        ))
}

categorize_ndc <- function(df) {
    df %>%
        dplyr::mutate(ndc_cat = factor(
            ndc_type,
            levels = c(
                "all_prescriptions",
                "opioids",
                "benzos",
                "alprazolam",
                "cyclobenzaprine",
                "gabapentin",
                "buspirone",
                "lorazepam",
                "dextroamphetamine",
                "methlyphenidate",
                "statins",
                "levothyroxine",
                "citalopram",
                "metformin",
                "warfarin",
                "chlordiazepoxide",
                "diazepam",
                "clonazepam",
                "oxazepam",
                "temazepam",
                "triazloam",
                "schii_opioids"
            ),
            labels = c(
                "All prescriptions",
                "Opioids",
                "Benzodiazepines",
                "Alprazolam",
                "Cyclobenzaprine",
                "Gabapentin",
                "Buspirone",
                "Lorazepam",
                "Dextroamphetamine",
                ## Oh no. I mis-spelled this and now all my code is wrong.
                "Methylphenidate",
                "Simvastatin",
                "Levothyroxine",
                "Citalopram",
                "Metformin",
                "Warfarin",
                "Chlordiazepoxide",
                "Diazepam",
                "Clonazepam",
                "Oxazepam",
                "Temazepam",
                "Triazloam",
                "Schedule II Opioids"
            ),
            ordered = TRUE
        ))
}

categorize_prov_state <- function(df) {
    df %>%
        dplyr::left_join(
            return_st_info() %>%
                dplyr::select(
                    prov_state = abbrev,
                    state_name = name,
                    state_cat = st_cat,
                    name_cat,
                    lon_rank
                ),
            by = "prov_state"
        )
}

categorize_stat <- function(df) {
    df %>%
        dplyr::mutate(stat_cat = factor(
            stat_type,
            levels = c("mean", "median", "total", "sd"),
            labels = c("Mean", "Median", "Total", "Standard Deviation"),
            ordered = TRUE
        ))
}

categorize_desc_big <- function(df) {
    df %>%
        dplyr::mutate(desc_big_cat = factor(
            desc_big,
            levels = c(
                "General Medicine",
                "Internal Medicine",
                "Family Medicine",
                "Pediatrician",
                "Ob/Gyn",
                "Emergency Medicine",
                "Critical Care",
                "Hospice",
                "PM and R",
                "Anesthesiologist",
                "Surgeon",
                "Addiction Medicine",
                "Pharmacy",
                "Dentist",
                "Other",
                "Unknown"
            ),
            labels = c(
                "General Medicine",
                "Internal Medicine",
                "Family Medicine",
                "Pediatrics",
                "Ob / Gyn",
                "Emergency Medicine",
                "Critical Care",
                "Hospice",
                "PM and R",
                "Anesthesiology",
                "Surgery",
                "Addiction Medicine",
                "Pharmacy",
                "Dentistry",
                "Other",
                "Unknown"
            ),
            ordered = TRUE
        ))
}

categorize_desc_bigger <- function(df) {
    df %>%
        dplyr::mutate(desc_bigger_cat = factor(
            desc_bigger,
            levels = c(
                "Primary Care",
                "Critical Care",
                "Hospice",
                "PM and R",
                "Anesthesiologist",
                "Surgeon",
                "Other",
                "Unknown"
            ),
            labels = c(
                "Primary Care",
                "Critical Care",
                "Hospice",
                "PM and R",
                "Anesthesiology",
                "Surgery",
                "Other",
                "Specialty not identified"
            ),
            ordered = TRUE
        ))
}

## Misc. Micro-helpers (no documentation -- self-explanatory) ----
return_ineq_name <- function(ineq) {
    dplyr::case_when(
        ineq == "gini" ~ "Gini coefficient",
        ineq == "ricci_schutz" ~ "Ricci-Schutz coefficient",
        ineq == "atkinson" ~ "Atkinson index",
        ineq == "theil" ~ "Theil entropy",
        ineq == "entropy" ~ "Generalized entropy",
        ineq == "herfindahl" ~ "Herfindahl-Hirschman index (concentration)",
        ineq == "rosenbluth" ~ "Rosenbluth index (concentration)"
    )
}

flag_closest_val <- function(x, val = .01) {
    if (sum(!is.na(x)) == 0) {
        return(NA)
    } else {
        abs(val - x) == min(abs(val - x))
    }
}

flag_all_closest_vals <- function(x, vals = (0:100) / 100) {
    bools <- rep(FALSE, NROW(x))
    for (v in vals) {
        bools[which(flag_closest_val(x, val = v))] <- TRUE
    }
    return(bools)
}

return_dose <- function(ndc_x) {
    dplyr::case_when(
        ndc_x == "opioids" ~ "MME",
        ndc_x == "levothyroxine" ~ "mcg",
        ndc_x == "schii_opioids" ~ "MME",
        ndc_x == "benzos" ~ "Lorazepam milligram equivalents",
        TRUE ~ "mg"
    )
}

return_proper_name <- function(ndc_x) {
    dplyr::case_when(
        ndc_x == "schii_opioids" ~ "Schedule II Opioids",
        ndc_x == "benzos" ~ "Benzodiazepines",
        TRUE ~ snakecase::to_title_case(ndc_x)
    )
}

#' Return a dataframe with useful state information
#'
#' @return df
return_st_info <- function() {
    st_info <- dplyr::tibble(
        abbrev   = datasets::state.abb,
        division = as.character(datasets::state.division),
        st_lat   = datasets::state.center$y,
        st_lon   = datasets::state.center$x
    ) %>%
        ## We have to add DC because it's not a state
        dplyr::add_row(
            abbrev = "DC",
            division = "South Atlantic",
            st_lat = 38.9072,
            st_lon = -77.0369
        ) %>%
        dplyr::left_join(narcan::st_fips_map) %>%
        ## Add in whole US and NA
        dplyr::add_row(
            abbrev = "US",
            name = "zzWhole US",
            division = "Whole US",
            st_lat = 0,
            st_lon = 200
        ) %>%
        dplyr::add_row(
            abbrev = NA,
            name = "zzzUnknown State",
            division = "Unknown",
            st_lat = 0,
            st_lon = 199
        ) %>%
        dplyr::rename(st_fips = fips) %>%
        dplyr::arrange(st_lon) %>%
        dplyr::mutate(
            lon_rank = dplyr::dense_rank(st_lon),
            alpha_rank = dplyr::dense_rank(name)
        ) %>%
        dplyr::mutate(name = gsub("zz|zzz", "", name))
    
    st_info %>%
        dplyr::mutate(
            st_cat = factor(
                abbrev,
                levels = st_info %>%
                    dplyr::arrange(lon_rank) %>%
                    dplyr::pull(abbrev),
                ordered = TRUE
            ),
            name_cat = factor(
                name,
                levels = st_info %>%
                    dplyr::arrange(lon_rank) %>%
                    dplyr::pull(name),
                ordered = TRUE
            ),
            name_cat_alpha = factor(
                name,
                levels = st_info %>%
                    dplyr::arrange(alpha_rank) %>%
                    dplyr::pull(name),
                ordered = TRUE
            )
        )
}

return_winsor_p <- function() {
    c(0, .001, .005, .01, .025, .05, .1)
}

return_min_rx <- function() {
    c(1, 3, 6, 12, 24, 50, 100, 200)
}

calc_mme_summaries <- function(grouped_df) {
    ## Define the quantities we want to summarize over.
    grouped_df %>%
        dplyr::summarize(
            mme_per_day = sum(mme / days_sup, na.rm = TRUE),
            quantity = sum(quantity, na.rm = TRUE),
            mme = sum(mme, na.rm = TRUE),
            n_patients = dplyr::n_distinct(patid),
            n_prescriptions = dplyr::n(),
            n_days = sum(days_sup, na.rm = TRUE)
        )
}

calc_normalized_mme <- function(df) {
    df %>%
        dplyr::mutate(
            mme_per_pt = mme / n_patients,
            mme_per_ptday = mme_per_day / n_patients,
            mme_per_prescription = mme / n_prescriptions
        )
}

is_in <- function(x, search_strings) {
    ## Return a 1 if x is in search_strings, else 0
    (x %in% search_strings) + 0
}

gc2 <- function() {
    ## Quietly clear memory -- useful for GCP/AWS
    invisible(gc(FALSE))
}

combine_tabs <- function(combined_tables) {
    ## Given two table() objects, combine them even if names aren't in each.
    tapply(combined_tables, names(combined_tables), sum)
}

extract_year <- function(x) {
    ## Assumes year is second to last number of file name (or string)
    r_matches <- regmatches(x, gregexpr("[[:digit:]]+", x))
    res <-
        as.numeric(unlist(lapply(r_matches, function(x)
            x[NROW(x) - 1])))
    if (length(res) > 0) {
        return(res)
    } else {
        NA
    }
}

extract_quarter <- function(x) {
    ## Assumes quarter is the last number of file name (or string)
    r_matches <- regmatches(x, gregexpr("[[:digit:]]+", x))
    res <-
        as.numeric(unlist(lapply(r_matches, function(x)
            x[NROW(x)])))
    if (length(res) > 0) {
        return(res)
    } else {
        NA
    }
}

remove_miscoded_dea_npi <- function(df) {
    ## Just remove the dea_npi codes that are miscoded
    df %>%
        dplyr::filter(!(
            dea_npi %in% c(
                "NoID_9",
                "T29999999_9",
                "(P5PI)_NoID",
                "TPOFVRFRR_NoID",
                "SML99JR9L_9999999999",
                "T29999999_NoID",
                "`_NoID",
                "\\_NoID",
                "(P5PI)_9",
                "._NoID",
                "'_NoID ",
                "!_NoID"
            )
        ))
}

mme_distribution <- function(df) {
    ## I don't remember why I had to write the code this way... But I assume
    ## my past self had a good reason to.
    dplyr::tibble(
        n_patients = c(
            df %>%
                dplyr::filter(!is.na(mme)) %>%
                dplyr::pull(n_patients) %>%
                sum(),
            df %>%
                dplyr::filter(!is.na(mme_per_day)) %>%
                dplyr::pull(n_patients) %>%
                sum(),
            df %>%
                dplyr::filter(!is.na(mme_per_pt)) %>%
                dplyr::pull(n_patients) %>%
                sum(),
            df %>%
                dplyr::filter(!is.na(mme_per_ptday)) %>%
                dplyr::pull(n_patients) %>%
                sum(),
            df %>%
                dplyr::filter(!is.na(mme_per_prescription)) %>%
                dplyr::pull(n_patients) %>%
                sum()
        ),
        n_prescriptions = c(
            df %>%
                dplyr::filter(!is.na(mme)) %>%
                dplyr::pull(n_prescriptions) %>%
                sum(),
            df %>%
                dplyr::filter(!is.na(mme_per_day)) %>%
                dplyr::pull(n_prescriptions) %>%
                sum(),
            df %>%
                dplyr::filter(!is.na(mme_per_pt)) %>%
                dplyr::pull(n_prescriptions) %>%
                sum(),
            df %>%
                dplyr::filter(!is.na(mme_per_ptday)) %>%
                dplyr::pull(n_prescriptions) %>%
                sum(),
            df %>%
                dplyr::filter(!is.na(mme_per_prescription)) %>%
                dplyr::pull(n_prescriptions) %>%
                sum()
        ),
        n_deanpi = c(
            sum(!is.na(df$mme), na.rm = TRUE),
            sum(!is.na(df$mme_per_day), na.rm = TRUE),
            sum(!is.na(df$mme_per_pt), na.rm = TRUE),
            sum(!is.na(df$mme_per_ptday), na.rm = TRUE),
            sum(!is.na(df$mme_per_prescription), na.rm = TRUE)
        ),
        meas = c(
            "mme",
            "mme_per_day",
            "mme_per_pt",
            "mme_per_ptday",
            "mme_per_prescription"
        ),
        sum = c(
            sum(df$mme, na.rm = TRUE),
            sum(df$mme_per_day, na.rm = TRUE),
            sum(df$mme_per_pt, na.rm = TRUE),
            sum(df$mme_per_ptday, na.rm = TRUE),
            sum(df$mme_per_prescription, na.rm = TRUE)
        ),
        mean = c(
            mean(df$mme, na.rm = TRUE),
            mean(df$mme_per_day, na.rm = TRUE),
            mean(df$mme_per_pt, na.rm = TRUE),
            mean(df$mme_per_ptday, na.rm = TRUE),
            mean(df$mme_per_prescription, na.rm = TRUE)
        ),
        sd = c(
            stats::sd(df$mme, na.rm = TRUE),
            stats::sd(df$mme_per_day, na.rm = TRUE),
            stats::sd(df$mme_per_pt, na.rm = TRUE),
            stats::sd(df$mme_per_ptday, na.rm = TRUE),
            stats::sd(df$mme_per_prescription, na.rm = TRUE)
        ),
        median = c(
            stats::median(df$mme, na.rm = TRUE),
            stats::median(df$mme_per_day, na.rm = TRUE),
            stats::median(df$mme_per_pt, na.rm = TRUE),
            stats::median(df$mme_per_ptday, na.rm = TRUE),
            stats::median(df$mme_per_prescription, na.rm = TRUE)
        ),
        min = c(
            min(df$mme, na.rm = TRUE),
            min(df$mme_per_day, na.rm = TRUE),
            min(df$mme_per_pt, na.rm = TRUE),
            min(df$mme_per_ptday, na.rm = TRUE),
            min(df$mme_per_prescription, na.rm = TRUE)
        ),
        max = c(
            max(df$mme, na.rm = TRUE),
            max(df$mme_per_day, na.rm = TRUE),
            max(df$mme_per_pt, na.rm = TRUE),
            max(df$mme_per_ptday, na.rm = TRUE),
            max(df$mme_per_prescription, na.rm = TRUE)
        ),
        p01 = c(
            p01(df$mme),
            p01(df$mme_per_day),
            p01(df$mme_per_pt),
            p01(df$mme_per_ptday),
            p01(df$mme_per_prescription)
        ),
        p05 = c(
            p05(df$mme),
            p05(df$mme_per_day),
            p05(df$mme_per_pt),
            p05(df$mme_per_ptday),
            p05(df$mme_per_prescription)
        ),
        p10 = c(
            p10(df$mme),
            p10(df$mme_per_day),
            p10(df$mme_per_pt),
            p10(df$mme_per_ptday),
            p10(df$mme_per_prescription)
        ),
        p25 = c(
            p25(df$mme),
            p25(df$mme_per_day),
            p25(df$mme_per_pt),
            p25(df$mme_per_ptday),
            p25(df$mme_per_prescription)
        ),
        p75 = c(
            p75(df$mme),
            p75(df$mme_per_day),
            p75(df$mme_per_pt),
            p75(df$mme_per_ptday),
            p75(df$mme_per_prescription)
        ),
        p90 = c(
            p90(df$mme),
            p90(df$mme_per_day),
            p90(df$mme_per_pt),
            p90(df$mme_per_ptday),
            p90(df$mme_per_prescription)
        ),
        p95 = c(
            p95(df$mme),
            p95(df$mme_per_day),
            p95(df$mme_per_pt),
            p95(df$mme_per_ptday),
            p95(df$mme_per_prescription)
        ),
        p99 = c(
            p99(df$mme),
            p99(df$mme_per_day),
            p99(df$mme_per_pt),
            p99(df$mme_per_ptday),
            p99(df$mme_per_prescription)
        )
    )
}

match_desc_tax <-
    function(grepl_desc,
             grepl_tax) {
        grepl(grepl_desc, comparison_df$desc, fixed = TRUE) &
            (
                grepl(grepl_tax, comparison_df$tax_desc1, fixed = TRUE) |
                    grepl(grepl_tax, comparison_df$tax_desc2, fixed = TRUE)
            )
    }

p25 <- function(x) {
    stats::quantile(x, .25, na.rm = TRUE)
}
p75 <- function(x) {
    stats::quantile(x, .75, na.rm = TRUE)
}
p01 <- function(x) {
    stats::quantile(x, .01, na.rm = TRUE)
}
p10 <- function(x) {
    stats::quantile(x, .10, na.rm = TRUE)
}
p90 <- function(x) {
    stats::quantile(x, .90, na.rm = TRUE)
}
p05 <- function(x) {
    stats::quantile(x, .05, na.rm = TRUE)
}
p95 <- function(x) {
    stats::quantile(x, .95, na.rm = TRUE)
}
p99 <- function(x) {
    stats::quantile(x, .99, na.rm = TRUE)
}
