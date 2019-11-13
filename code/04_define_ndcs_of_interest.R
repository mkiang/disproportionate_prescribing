## 04_define_ndcs_of_interest.R ----
##
## This file will go through and extract the NDCs of drugs we will use in
## the analysis, which will allow us to subset the full prescription files
## into (more manageable) smaller files.
##
## The NDCs we are interested in are defined in `utils.R`` under the
## return_ndc_keypairs() function. You can add and remove drugs of interest
## there. Do NOT attempt to do so in this file.
##
## Each set of NDCs will be saved separately and we will also save one larger
## file with all NDCs and "conversion" factors --- noting that conversion
## factor for non-opioids and non-benzos is always 1.

## Imports ----
library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(fs)
source(here::here("code", "utils.R"))

## Create folders ----
fs::dir_create(here::here("data", "ndcs"))
fs::dir_create(here::here("data_raw"))
fs::dir_create(here::here("data_private"))

## Constants ----
ndc_dict <- return_ndc_keypairs()

## Load data ----
## Load up all opioids, removing buprenorphine
opioid_mme_df <- dplyr::bind_rows(
    readxl::read_xlsx(
        here::here(
            "data_raw",
            "CDC_Oral_Morphine_Milligram_Equivalents_Sept_2017.xlsx"
        ),
        "Opioids"
    ),
    readxl::read_xlsx(
        here::here(
            "data_raw",
            "CDC_Oral_Morphine_Milligram_Equivalents_Sept_2017.xlsx"
        ),
        "Abuse-Deterrent Opioids"
    ) %>%
        dplyr::mutate(
            NDC_Numeric = as.numeric(NDC_Numeric),
            DEAClassCode = as.character(DEAClassCode)
        )
) %>%
    janitor::clean_names(., case = "snake") %>%
    dplyr::filter(!is.na(mme_conversion_factor))  ## Remove tx buprenorphine

## Extract and save NDCs of (pain) opioids
opioid_ndcs <- opioid_mme_df %>%
    dplyr::pull(ndc) %>%
    unique(.)
saveRDS(opioid_ndcs,
        here::here("data", "ndcs", "ndc_opioids.RDS"),
        compress = "xz")

## Schedule II opioids
sch_ii_ndcs <- opioid_mme_df %>%
    dplyr::filter(dea_class_code == "2") %>%
    dplyr::pull(ndc) %>%
    unique(.)
saveRDS(sch_ii_ndcs,
        here::here("data", "ndcs", "ndc_schii_opioids.RDS"),
        compress = "xz")

## Load up the Optum NDC Lookup table ----
## (downloading it if necessary)
if (!fs::file_exists(here::here("data_private", "zip5_lu_ndc.csv"))) {
    ndc_info <- return_bucket_objects(f_regex = "zip5_lu_ndc")
    googleCloudStorageR::gcs_get_object(
        ndc_info$name,
        ndc_info$bucket,
        saveToDisk = here::here("data_private", "zip5_lu_ndc.csv")
    )
}
ndc_lookup <-
    readr::read_csv(here::here("data_private", "zip5_lu_ndc.csv"),
                    progress = FALSE) %>%
    janitor::clean_names(., case = "snake")

## Extract the NDCs of all non-opioid drugs using dictionary ----
ndc_codes <- list()
for (n in names(ndc_dict)) {
    ndc_codes[[n]] <- ndc_lookup %>%
        dplyr::filter(gnrc_nm == ndc_dict[[n]],
                      dosage_fm_desc %in% c("TABLET", "CAPSULE")) %>%
        dplyr::pull(ndc) %>%
        unique(.)
    
    saveRDS(ndc_codes[[n]],
            here::here("data", "ndcs", sprintf("ndc_%s.RDS", n)),
            compress = "xz")
}

## Make a dataframe with MME conversions for all NDCs ----
## It obviously doesn't make sense for statins or other non-opioids to have
## MME conversions so we just assign them a strength_per_unit of 1 and
## rename the columns to be the same shape.
mme_conversions <- opioid_mme_df %>%
    dplyr::filter(ndc %in% opioid_ndcs) %>%
    dplyr::select(
        ndc,
        generic_name = generic_drug_name,
        strength_per_unit,
        unit_of_meas = uom,
        mme_conversion_factor
    ) %>%
    dplyr::mutate(drug_type = "opioid")

for (n in names(ndc_codes)) {
    mme_conversions <- dplyr::bind_rows(
        mme_conversions,
        ndc_lookup %>%
            dplyr::filter(ndc %in% ndc_codes[[n]]) %>%
            dplyr::select(
                ndc,
                generic_name = gnrc_nm,
                strength_per_unit = drg_strgth_nbr,
                dplyr::ends_with("unit_desc")
            ) %>%
            dplyr::mutate(
                ## Ugly patch to fix the plural
                drug_type = ifelse(n == "statin", "statins", n),
                mme_conversion_factor = 1,
                unit_of_meas = ifelse(
                    is.na(drg_strgth_unit_desc),
                    drg_strgth_vol_unit_desc,
                    drg_strgth_unit_desc
                )
            ) %>%
            dplyr::select(-drg_strgth_vol_unit_desc,
                          -drg_strgth_unit_desc)
    )
}

## Stanardized benzos ----
## Now we want a standardized benzodiazepine version so we subset the benzo
## NDCs and then convert to lorazepam (ativan) standardized doses using this:
##      https://www.mdcalc.com/benzodiazepine-conversion-calculator#evidence
benzo_conversion <- mme_conversions %>%
    dplyr::filter(
        drug_type %in% c(
            "alprazolam",
            "chlordiazepoxide",
            "diazepam",
            "clonazepam",
            "lorazepam",
            "oxazepam",
            "temazepam",
            "triazolam"
        )
    ) %>%
    dplyr::mutate(
        mme_conversion_factor = dplyr::case_when(
            drug_type == "alprazolam" ~ 2,
            drug_type == "chlordiazepoxide" ~ 1 / 10,
            drug_type == "diazepam" ~ 1 / 6,
            drug_type == "clonazepam" ~  2,
            drug_type == "lorazepam" ~ 1,
            drug_type == "oxazepam" ~ 1 / 10,
            drug_type == "temazepam" ~ 1 / 10,
            drug_type == "triazolam" ~ 4
        )
    ) %>%
    dplyr::mutate(drug_type = "benzos")

## Save just the NDCs for looping later ----
benzos_ndc <- benzo_conversion %>%
    dplyr::pull(ndc) %>%
    unique(.)
saveRDS(benzos_ndc,
        here::here("data", "ndcs", "ndc_benzos.RDS"),
        compress = "xz")

## Save all drug info with conversions ----
readr::write_csv(
    dplyr::bind_rows(mme_conversions, benzo_conversion),
    here::here("data", "ndcs", "all_ndcs_with_mme_conversion.csv")
)
