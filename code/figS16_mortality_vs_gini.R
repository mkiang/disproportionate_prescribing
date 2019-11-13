## Imports ----
library(here)
library(tidyverse)
library(ggrepel)
source(here::here("code", "utils.R"))
source(here::here("code", "mk_nytimes.R"))

## Get data ----
gini_coefs  <-
    readr::read_csv(here::here("data", "ineq_estimates_all.csv.xz"))
mme_summary <- readr::read_csv(here::here("data",
                                          "descriptives",
                                          "descriptives_mme_summaries.csv.xz"))
opioid_rates <- readr::read_csv(
    here::here("data_raw", "kff_opioid_raw_data.csv"),
    skip = 3,
    n_max = 52,
    col_names = c("name",
                  paste0("y", 1999:2017)),
    na = c("NR", "NSD")
)

## Munge mortality data
opioid_rates <- opioid_rates %>%
    tidyr::gather(year, std_rate, y1999:y2017) %>%
    dplyr::mutate(year = as.numeric(gsub("y", "", year)),
                  name = ifelse(name == "United States", "Whole US", name)) %>%
    dplyr::filter(year >= 2003)

## Merge data ----
all_df <- opioid_rates %>%
    dplyr::left_join(return_st_info())

all_df <- all_df %>%
    dplyr::left_join(
        gini_coefs %>%
            dplyr::filter(w == 0,
                          prov_cat == "all_types",
                          ndc_type == "opioids",
                          meas == "mme") %>%
            dplyr::select(year,
                          prov_state,
                          gini,
                          n_patients,
                          n_prescriptions,
                          n_deanpi),
        by = c("abbrev" = "prov_state", "year")
    )

all_df <- all_df %>%
    dplyr::left_join(
        mme_summary %>%
            dplyr::filter(w == 0,
                          prov_cat == "all_types",
                          ndc_type == "opioids",
                          meas == "mme") %>%
            dplyr::select(prov_state,
                          year,
                          sum, mean),
        by = c("abbrev" = "prov_state", "year")
    )

all_df <- all_df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
        label_outlier = dplyr::case_when(gini < .6 ~ 1,
                                         std_rate > 38 ~ 1,
                                         TRUE ~ 0),
        dot_label = sprintf("%s (%s)", abbrev, year)
    )

p1 <- ggplot2::ggplot(all_df,
                      ggplot2::aes(
                          x = gini,
                          y = std_rate,
                          size = sum / 10 ^ 6,
                          color = sum / 10 ^ 6
                      )) +
    ggplot2::geom_point(alpha = .8) +
    # geom_smooth(aes(group = 1), method = "lm", se = FALSE) +
    ggplot2::scale_size_area("MMEs dispensed (millions)",
                             max_size = 8,
                             trans = "log10") +
    ggplot2::scale_color_distiller(NULL,
                                   palette = "Spectral",
                                   direction = -1,
                                   trans = "log10") +
    mk_nytimes(legend.position = "bottom") +
    ggplot2::scale_y_continuous("Age-standrdized opioid mortality rate (per 100,000)") +
    ggplot2::scale_x_continuous("Gini coefficient") +
    ggrepel::geom_text_repel(
        data = all_df %>% dplyr::filter(label_outlier == 1),
        ggplot2::aes(label = dot_label),
        color = "black",
        size = 3.5
    )

## Save them ----
fs::dir_create(here::here("plots"))
fs::dir_create(here::here("grobs"))

ggplot2::ggsave(
    "./plots/figS16_mortality_vs_gini.pdf",
    p1,
    width = 4,
    height = 2.5,
    scale = 1.75,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    "./plots/figS16_mortality_vs_gini.png",
    p1,
    width = 4,
    height = 2.5,
    scale = 1.75,
    dpi = 300
)

saveRDS(p1,
        here::here("grobs", "figS16_mortality_vs_gini.RDS"),
        compress = "xz")
