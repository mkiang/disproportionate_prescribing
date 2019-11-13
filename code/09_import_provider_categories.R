## 09_import_provider_categories.R ----
##
## We need provider information in order to get specialty and state. We
## use the Optum provider bridge and provider detail files and munge them
## into a more usable format. Later, we'll combine this cleaned and munged
## data to get more information (e.g., provider specialty).

## Imports ----
library(tidyverse)
library(readxl)
library(fs)
library(here)
library(janitor)
source(here::here("code", "utils.R"))

## Make sure we downloaded provider data, then load it ----
if (any(!fs::file_exists(here::here("data_private", "zip5_provider.csv")),
        !fs::file_exists(here::here(
            "data_private", "zip5_provider_bridge.csv"
        )))) {
    prov_objs <- return_bucket_objects(f_regex = "provider")
    
    for (i in 1:NROW(prov_objs)) {
        f_name <- here::here("data_private", prov_objs$name[i])
        
        if (!file.exists(f_name)) {
            googleCloudStorageR::gcs_get_object(
                object_name = prov_objs$name[i],
                bucket = prov_objs$bucket[i],
                saveToDisk = f_name
            )
        }
    }
}

## Load provider data ----
provider <-
    readr::read_csv(here::here("data_private", "zip5_provider.csv")) %>%
    janitor::clean_names(., case = "snake")
provider_bridge <-
    readr::read_csv(here::here("data_private", "zip5_provider_bridge.csv")) %>%
    janitor::clean_names(., case = "snake")

## Read in provider lookup codes and clean up a bit ----
if (!fs::file_exists(here::here("data", "provider_category_df.csv"))) {
    provcat_df <-
        readxl::read_xls(
            here::here("data_private", "CDM_Data_Dictionary_7.1_ZIP5.xls"),
            sheet = "PROVCAT",
            range = "A6:F7114"
        ) %>%
        janitor::clean_names(., case = "snake") %>%
        dplyr::select(
            prov_cat = provcat,
            desc = description,
            cat1 = catgy_rol_up_1_desc,
            cat2 = catgy_rol_up_2_desc,
            cat3 = catgy_rol_up_3_desc
        ) %>%
        dplyr::mutate_all(tolower)
    
    ## Make indicators for specialties we want to compare
    provcat_df <- provcat_df %>%
        dplyr::mutate(
            critical_care = is_in(cat1, c("critical care medicine")),
            emergency = is_in(cat1, c(
                "emergency medicine", "emergency care"
            )),
            family = is_in(cat1, c("family practice")),
            gen_surgery = is_in(cat1, c("general surgery")),
            hospice = is_in(cat1, c(
                "hospice and palliative medicine", "hospice"
            )),
            internal = is_in(cat1, c("internal medicine")),
            obgyn = is_in(
                cat1,
                c("obstetrics & gynecology", "gynecology", "obstetrics")
            ),
            ortho = is_in(cat1, c("orthopedics")),
            pediatrics = is_in(cat1, c("pediatrics")),
            pmnr = is_in(cat1, c("physical medicine & rehabilitation")),
            plastics = is_in(cat1, c("plastic & reconstructive surgery")),
            prev_internal_family = is_in(
                cat1,
                c(
                    "preventive medicine",
                    "internal medicine",
                    "family pracatice"
                )
            ),
            all_surgery = is_in(
                cat1,
                c(
                    "colon & rectal surgery",
                    "general surgery",
                    "neurological surgery",
                    "orthopedics",
                    "plastic & reconstructive surgery",
                    "thoracic surgery",
                    "vascular surgery"
                )
            )
        )
    
    ## Indicator for any provider category of interest
    provcat_df <- provcat_df %>%
        dplyr::mutate(cat_of_interest = (
            provcat_df %>%
                dplyr::select(critical_care:all_surgery) %>%
                rowSums() > 0
        ) + 0)
    
    readr::write_csv(provcat_df,
                     here::here("data", "provider_category_df.csv"))
}
prov_cats <-
    readr::read_csv(here::here("data", "provider_category_df.csv"))

## Read in taxonomy lookup codes and clean up a bit ----
if (!fs::file_exists(here::here("data", "provider_taxonomy_df.csv"))) {
    tax_df <-
        readxl::read_xls(
            here::here("data_private", "CDM_Data_Dictionary_7.1_ZIP5.xls"),
            sheet = "TAXONOMY",
            range = "A6:B872"
        ) %>%
        janitor::clean_names(., case = "snake") %>%
        dplyr::select(taxonomy,
                      tax_desc = description) %>%
        dplyr::mutate_all(tolower)
    
    ## Split out some of the taxonomy codes
    tax_df <- tax_df %>%
        tidyr::separate(
            tax_desc,
            into = c("tax_1", "tax_2", "tax_3"),
            sep = " - ",
            remove = FALSE
        )
    
    readr::write_csv(tax_df, here::here("data", "provider_taxonomy_df.csv"))
}
tax_df <-
    readr::read_csv(here::here("data", "provider_taxonomy_df.csv"))

## Subset provider bridge -- we only want rows that will provide the
## data we need (i.e., are unique by DEA and NPI).
provider_min <- provider_bridge %>%
    tidyr::replace_na(list(dea = "NoID", npi = "NoID")) %>%
    dplyr::mutate(dea_npi = paste0(dea, "_", npi)) %>%
    dplyr::select(prov_unique, dea_npi) %>%
    dplyr::filter(dea_npi != "NoID_NoID") %>%
    dplyr::distinct()

## Join with state and provider type info and drop missings
provider_min <- provider_min %>%
    dplyr::left_join(
        provider %>%
            dplyr::select(prov_unique,
                          prov_cat = provcat,
                          prov_state,
                          taxonomy1,
                          taxonomy2) %>%
            dplyr::filter(
                prov_state != "UN",
                prov_state != "PR",
                prov_unique != 0,
                prov_cat != "0000"
            ),
        by = "prov_unique"
    ) %>%
    dplyr::filter((!is.na(prov_state) & !is.na(prov_cat)))  %>%
    dplyr::select(-prov_unique) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dea_npi)

## Some providers appear to have multiple categories/states
dupe_ids <- provider_min %>%
    dplyr::mutate(dupe_id = dea_npi == dplyr::lag(dea_npi)) %>%
    dplyr::filter(dupe_id) %>%
    dplyr::pull(dea_npi)

provider_min <- provider_min %>%
    dplyr::mutate(duplicated_id = (dea_npi %in% dupe_ids) + 0) %>%
    dplyr::left_join(prov_cats %>%
                         dplyr::select(-cat1, -cat2, -cat3),
                     by = "prov_cat")

provider_min <- provider_min %>%
    dplyr::left_join(
        tax_df %>%
            dplyr::mutate(taxonomy = toupper(taxonomy)) %>%
            dplyr::select(taxonomy, tax_desc1 = tax_desc),
        by = c("taxonomy1" = "taxonomy")
    ) %>%
    dplyr::left_join(
        tax_df %>%
            dplyr::mutate(taxonomy = toupper(taxonomy)) %>%
            dplyr::select(taxonomy, tax_desc2 = tax_desc),
        by = c("taxonomy2" = "taxonomy")
    )

readr::write_csv(provider_min,
                 here::here("data_private", "provider_type_state.csv.xz"))
