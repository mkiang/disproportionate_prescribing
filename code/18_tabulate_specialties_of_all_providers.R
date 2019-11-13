## 18_tabulate_specialties_of_all_providers.R ----
##
## Look at all providers across our NDCs and try to group providers into
## specialties. Note that Optum assigns providers into a huge number of
## groups (~7100), which isn't useful for our purposes. We collapse the
## providers into specialties that we think are informative. To be as
## transparent as possible about how we collapse, we use case_when() and a
## dictionary of old ~ new categories below. You can modify these accordingly.
## 
## NOTE: At the end of this file, we group with the top centile (created in 17),
## so you *must* run that file first. 

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(foreach)
library(parallel)
library(doParallel)
source(here::here("code", "utils.R"))

## Data ----
prov_st_cat <-
    readr::read_csv(here::here("data_private", "provider_type_state.csv.xz"))
prov_st_cat <- prov_st_cat %>%
    dplyr::filter(duplicated_id == 0) %>%
    dplyr::select(dea_npi, prov_type = prov_cat, desc)

## Make grid ----
param_grid <- expand.grid(
    year_x = 2003:2017,
    ndc_x = return_drug_names(),
    state_x = "US",
    prov_cat_x = "all_types",
    w_x = NA,
    stringsAsFactors = FALSE
)

## Loop through grid and collapse provider types ----
if (!fs::file_exists(here::here(
    "data_private",
    "all_ntiles_provider_categories_full.csv.xz"
))) {
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
        
        all_dea_npis <- import_deanpis(ndc_x, year_x)
        
        all_dea_npis %>%
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
            "all_ntiles_provider_categories_full.csv.xz"
        )
    )
}

## Collapse down to larger categories and remove the small cells to
## comply with our data use agreement.
prov_cats <-
    dplyr::bind_rows(
        readr::read_csv(
            here::here("data_private", "top_ntile_provider_categories_full.csv.xz")
        ) %>%
            dplyr::mutate(ntile = "top1"),
        readr::read_csv(
            here::here(
                "data_private",
                "all_ntiles_provider_categories_full.csv.xz"
            )
        ) %>%
            dplyr::mutate(ntile = "all",
                          w = 0)
    )

prov_cats <- prov_cats %>%
    dplyr::group_by(desc_big,
                    ntile,
                    w,
                    prov_state,
                    ndc_type,
                    prov_cat,
                    year) %>%
    dplyr::summarize(freq = sum(freq)) %>%
    dplyr::group_by(w,
                    ntile,
                    prov_state,
                    ndc_type,
                    prov_cat,
                    year) %>%
    dplyr::mutate(prop = freq / sum(freq)) %>%
    dplyr::ungroup()

prov_cats <- prov_cats %>%
    categorize_w() %>%
    categorize_prov_cat() %>%
    categorize_ndc() %>%
    categorize_prov_state() %>%
    categorize_desc_big() %>%
    dplyr::mutate(ntile_cat = factor(
        ntile,
        levels = c("top1", "all"),
        labels = c("Top 1% providers",
                   "All providers"),
        ordered = TRUE
    ))

prov_cats <- prov_cats %>%
    dplyr::mutate(freq = ifelse(freq < 10, NA, freq),
                  prop = ifelse(freq < 10, NA, prop))

saveRDS(prov_cats,
        here::here("data", "provider_categories_collapsed.RDS"),
        compress = "xz")
