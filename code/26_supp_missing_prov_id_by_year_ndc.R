## Supplemental analysis --- tabulate the number of missing provider IDs ----
##
## One reviewer would like us to confirm the prevalence of missing provider
## IDs in our data. We used the Optum-recommended process of concatenating
## DEA and NPI into a single provider ID. This means that we cannot track a
## provider who added information in a later year, but it is largely not
## applicable to our project since we stratify by year. It does, however, make
## our estimate of overlap too conservative. The magnitude of this issue is
## likely to be small.

## Imports
library(tidyverse)
library(here)
library(furrr)
source(here::here("code", "utils.R"))

param_grid <- expand.grid(year = 2003:2017,
                          ndc = return_drug_names(),
                          stringsAsFactors = FALSE)

if (!fs::file_exists(here::here("data", "descriptives", "missing_provider_ids.csv"))) {
    future::plan(future::multisession)
    
    missing_df <- furrr::future_map(
        .x = 1:NROW(param_grid),
        .f = ~ {
            temp_df <- import_mini_rx_data(param_grid$year[.x],
                                           param_grid$ndc[.x])
            dplyr::tibble(
                year = param_grid$year[.x],
                ndc_type = param_grid$ndc[.x],
                no_deanpi = sum(temp_df$dea_npi == "NoID_NoID"),
                total = NROW(temp_df)
            ) %>%
                dplyr::mutate(prop = no_deanpi / total)
        }) %>% 
        dplyr::bind_rows()
    
    readr::write_csv(
        missing_df,
        here::here("data", "descriptives", "missing_provider_ids.csv")
        )
}
