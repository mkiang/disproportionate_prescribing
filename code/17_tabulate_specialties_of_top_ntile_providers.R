## 17_tabulate_specialties_of_top_ntile_providers.R ----
##
## Look at the top 1% of providers and try to group providers into
## specialties. Note that Optum assigns providers into a huge number of
## groups (~7100), which isn't useful for our purposes. We collapse the 
## providers into specialties that we think are informative. To be as 
## transparent as possible about how we collapse, we use case_when() and a
## dictionary of old ~ new categories below. You can modify these accordingly. 

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(foreach)
library(parallel)
library(doParallel)
source(here::here("code", "utils.R"))

## Data ----
top_ntile_prov <-
    readRDS(here::here("data_private", "top_ntile_providers.RDS"))
prov_st_cat <-
    readr::read_csv(here::here("data_private", "provider_type_state.csv.xz"))
prov_st_cat <- prov_st_cat %>%
    dplyr::filter(duplicated_id == 0) %>%
    dplyr::select(dea_npi, prov_type = prov_cat, desc)

## Make grid ----
param_grid <- expand.grid(
    year_x = unique(top_ntile_prov$year),
    ndc_x = unique(top_ntile_prov$ndc_type),
    state_x = unique(top_ntile_prov$prov_state),
    prov_cat_x = unique(top_ntile_prov$prov_cat),
    w_x = unique(top_ntile_prov$w),
    stringsAsFactors = FALSE
)

if (!fs::file_exists(here::here("data_private",
                                "top_ntile_provider_categories_full.csv.xz"))) {
    ## Loop through grid and collapse provider types ----
    doParallel::registerDoParallel(cores = 14)
    results <- foreach::foreach(
        i = sample(1:NROW(param_grid)),
        .inorder = FALSE,
        .combine = dplyr::bind_rows
    ) %dopar% {
        w_x <- param_grid$w_x[i]
        state_x <- param_grid$state_x[i]
        prov_cat_x <- param_grid$prov_cat_x[i]
        year_x <- param_grid$year_x[i]
        ndc_x <- param_grid$ndc_x[i]
        
        top_ntile_prov %>%
            dplyr::filter(
                w == w_x,
                prov_state == state_x,
                ndc_type == ndc_x,
                prov_cat == prov_cat_x,
                year == year_x
            ) %>%
            tabulate_provider_types(prov_st_cat_x = prov_st_cat) %>%
            dplyr::mutate(
                w = w_x,
                prov_state = state_x,
                ndc_type = ndc_x,
                prov_cat = prov_cat_x,
                year = year_x
            )
    }
    doParallel::stopImplicitCluster()
    
    ## Collapse some of the categories ----
    ## Yes, there are more concise ways of coding this, but I want to be as
    ## explicit about recodes as possible.
    results <- results %>%
        dplyr::mutate(
            desc_big = dplyr::case_when(
                desc == "family nurse practitioner" ~ "Family Medicine",
                desc == "family practice specialist" ~ "Family Medicine",
                desc == "family practice/capitated clinic" ~ "Family Medicine",
                desc == "family practice/general practice" ~ "Family Medicine",
                desc == "family practitioner" ~ "Family Medicine",
                desc == "general practitioner" ~ "General Medicine",
                desc == "general preventive medicine specialist" ~ "General Medicine",
                desc == "internal med pediatrics" ~ "Pediatrician",
                desc == "internal medicine specialist" ~ "Internal Medicine",
                desc == "internist/general internist" ~ "Internal Medicine",
                desc == "pediatrician" ~ "Pediatrician",
                desc == "emergency medicine (physicians)" ~ "Emergency Medicine",
                desc == "pediatric critical care medicine specialist" ~ "Critical Care",
                desc == "critical care medicine specialist" ~ "Critical Care",
                desc == "anesthesiologist" ~ "Anesthesiologist",
                desc == "pediatric anesthesiology" ~ "Anesthesiologist",
                desc == "general surgeon" ~ "Surgeon",
                desc == "back and spine surgery" ~ "Surgeon",
                desc == "neuro-surgeon" ~ "Surgeon",
                desc == "orthopedic surgeon" ~ "Surgeon",
                desc == "plastic surgeon" ~ "Surgeon",
                desc == "vascular surgeon" ~ "Surgeon",
                desc == "pediatric neurosurgery" ~ "Surgeon",
                desc == "general surgeon - trauma specialist" ~ "Surgeon",
                desc == "orthopedic surgeon - hand specialist" ~ "Surgeon",
                desc == "general surgeon - oral & maxilofacial specialist" ~ "Surgeon",
                desc == "colon & rectal surgeon" ~ "Surgeon",
                desc == "hospice and palliative medicine" ~ "Hospice",
                desc == "pain management specialist" ~ "PM and R",
                desc == "physical medicine specialist" ~ "PM and R",
                desc == "rehabilitation medicine specialist" ~ "PM and R",
                desc == "obstetrician/gynecologist" ~ "Ob/Gyn",
                # desc == "gynecological oncologist" ~ "Ob/Gyn",
                desc == "obstetrics/gynecology specialist" ~ "Ob/Gyn",
                desc == "gynecologist" ~ "Ob/Gyn",
                desc == "dentist - dmd" ~ "Dentist",
                desc == "general dentist" ~ "Dentist",
                desc == "pharmacy" ~ "Pharmacy",
                desc == "addiction medicine" ~ "Addiction Medicine",
                desc == "addiction medicine specialist" ~ "Addiction Medicine",
                desc == "addiction psychologist" ~ "Addiction Medicine",
                desc == "alcohol and drug abuse counselor" ~ "Addiction Medicine",
                is.na(desc) ~ "Unknown",
                TRUE ~ "Other"
            )
        ) %>%
        dplyr::mutate(
            desc_bigger = dplyr::case_when(
                desc_big == "Family Medicine" ~ "Primary Care",
                desc_big == "Internal Medicine" ~ "Primary Care",
                desc_big == "Pediatrician" ~ "Primary Care",
                desc_big == "General Medicine" ~ "Primary Care",
                # desc_big == "Critical Care" ~ "Other",
                # desc_big == "Hospice" ~ "Other",
                desc_big == "Pharmacy" ~ "Other",
                desc_big == "Addiction Medicine" ~ "Other",
                desc_big == "Ob/Gyn" ~ "Other",
                desc_big == "Emergency Medicine" ~ "Other",
                TRUE ~ desc_big
            )
        )
    
    readr::write_csv(
        results,
        here::here(
            "data_private",
            "top_ntile_provider_categories_full.csv.xz"
        )
    )
}
