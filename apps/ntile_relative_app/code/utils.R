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
