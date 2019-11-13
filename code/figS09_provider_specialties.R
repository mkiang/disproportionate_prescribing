## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "utils.R"))
source(here::here("code", "plotting_utils", "plot_top_centile_specialties.R"))

## Get data and clean a bit ----
prov_cats <-
    bind_rows(
        readr::read_csv(
            here::here("data_private", "top_ntile_provider_categories_full.csv.xz")
        ) %>%
            mutate(ntile = "top1"),
        readr::read_csv(
            here::here(
                "data_private",
                "all_ntiles_provider_categories_full.csv.xz"
            )
        ) %>%
            mutate(ntile = "all",
                   w = 0)
    ) %>%
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
    dplyr::ungroup() %>%
    categorize_w() %>%
    categorize_prov_cat() %>%
    categorize_ndc() %>%
    categorize_prov_state() %>%
    categorize_desc_big() %>%
    mutate(ntile_cat = factor(
        ntile,
        levels = c("top1", "all"),
        labels = c("Top 1% providers",
                   "All providers"),
        ordered = TRUE
    ))

## Plot it ----
p1 <- plot_top_centile_specialties(
    prov_cats %>%
        filter(prop > .005),
    min_year = 2008,
    w_x = 0,
    ndc_type_x = "opioids",
    label_ends = TRUE
) +
    facet_grid(ntile_cat ~ .)

## Save them ----
fs::dir_create(here::here("plots"))
fs::dir_create(here::here("grobs"))

ggplot2::ggsave(
    here::here("plots", "figS09_top_provider_specialties.pdf"),
    p1,
    width = 6,
    height = 7,
    scale = 1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS09_top_provider_specialties.png"),
    p1,
    width = 6,
    height = 7,
    scale = 1,
    dpi = 300
)
saveRDS(p1,
        here::here("grobs", "figS09_top_provider_specialties.RDS"),
        compress = "xz")
