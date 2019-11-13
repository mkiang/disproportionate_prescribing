## Imports ----
library(tidyverse)
library(here)
library(patchwork)
source(here::here("code", "utils.R"))
source(here::here("code", "plotting_utils", "plot_ntile_transactions.R"))

## Data and munging ----
transactions_df <-
    readr::read_csv(here::here("data", "ntile_to_ntile_transactions.csv")) %>%
    mutate(
        dea_ntile = case_when(
            dea_ntile == 1 ~ 1,
            between(dea_ntile, 2, 10) ~ 2,
            between(dea_ntile, 11, 100) ~ 3
        ),
        patid_ntile = case_when(
            patid_ntile == 1 ~ 1,
            between(patid_ntile, 2, 10) ~ 2,
            between(patid_ntile, 11, 100) ~ 3
        )
    ) %>%
    group_by(year,
             ndc_type,
             prov_cat,
             prov_state,
             dea_ntile,
             patid_ntile) %>%
    summarize_all(sum) %>%
    ungroup() %>% 
    categorize_ndc() %>%
    categorize_prov_cat() %>%
    categorize_prov_state() %>%
    mutate(
        dea_ntile_cat = factor(
            dea_ntile,
            levels = 1:3,
            labels = c("1st", "2nd-10th", "Bottom 90"),
            ordered = TRUE
        ),
        patid_ntile_cat = factor(
            patid_ntile,
            levels = 1:3,
            labels = c("1st", "2nd-10th", "Bottom 90"),
            ordered = TRUE
        )
    )

## Subset ----
sub_df <- transactions_df %>%
    filter(
        year == 2017,
        ndc_type %in% c("opioids", "benzos"),
        !is.na(dea_ntile),
        !is.na(patid_ntile)
    ) %>%
    mutate(mme = mme / 1000000000)

p1_left <- ggplot(
    sub_df %>%
        filter(ndc_type == "opioids"),
    aes(y = mme,
        axis1 = dea_ntile_cat,
        axis2 = patid_ntile_cat),
    clip = "off"
) +
    geom_alluvium(aes(fill = dea_ntile),
                  color = NA,
                  alpha = .8) +
    geom_stratum(
        width = 1 / 3,
        # fill = "grey15",
        color = "white",
        alpha = 1,
        aes(fill = dea_ntile)
    ) +
    geom_label(stat = "stratum",
               label.strata = TRUE,
               size = 3) +
    mk_nytimes(
        panel.grid.major = element_blank(),
        legend.position = "none"
    ) +
    theme(plot.title = element_text(size = 12)) + 
    scale_x_continuous(
        NULL,
        expand = c(0, 0),
        breaks = c(1, 2),
        labels = c("Provider\nCentile Group", "Patient\nCentile Group")
    ) +
    scale_y_continuous("Standardized doses transacted (billions)",
                       expand = c(0, 0)) +
    scale_fill_viridis_c(
        name = NULL,
        option = "D",
        end = .8,
        na.value = "grey30"
    ) +
    labs(title = "Opioids")

p1_right <- ggplot(
    sub_df %>%
        filter(ndc_type == "benzos"),
    aes(y = mme,
        axis1 = dea_ntile_cat,
        axis2 = patid_ntile_cat),
    clip = "off"
) +
    geom_alluvium(aes(fill = dea_ntile),
                  color = NA,
                  alpha = .8) +
    geom_stratum(
        width = 1 / 3,
        color = "white",
        alpha = 1,
        aes(fill = dea_ntile)
    ) +
    geom_label(stat = "stratum",
               label.strata = TRUE,
               size = 3) +
    mk_nytimes(
        panel.grid.major = element_blank(),
        legend.position = "none"
    ) +
    theme(plot.title = element_text(size = 12)) + 
    scale_x_continuous(
        NULL,
        expand = c(0, 0),
        breaks = c(1, 2),
        labels = c("Provider\nCentile Group", "Patient\nCentile Group")
    ) +
    scale_y_continuous(" ",
                       expand = c(0, 0)) +
    scale_fill_viridis_c(
        name = NULL,
        option = "D",
        end = .8,
        na.value = "grey30"
    ) +
    labs(title = "Benzodiazepines")

p1 <- p1_left + p1_right + plot_layout(nrow = 1)

## Save them ----
fs::dir_create(here::here("plots"))
fs::dir_create(here::here("grobs"))

ggplot2::ggsave(
    here::here("plots",
               "fig03_ntile_transactions_2017.pdf"),
    p1,
    width = 7,
    height = 3.25,
    scale = 1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots",
               "fig03_ntile_transactions_2017.png"),
    p1,
    width = 7,
    height = 3.25,
    scale = 1,
    dpi = 300
)
saveRDS(p1,
        here::here("grobs", "fig03_ntile_transactions_2017.RDS"),
        compress = "xz")
