## Plot the mirrored lorenz curve of opioid doses and prescriptions for 2017,
## with the selected comparison drugs as grey lines. 

## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "utils.R"))
source(here::here(
    "code",
    "plotting_utils",
    "plot_lorenz_one_year_same_facet_color.R"
))

## Data ----
lorenz_df <-
    import_lorenz_curves(
        state_x = "US",
        ndc_x = c(
            "benzos", 
            "opioids"
        ),
        year_x = 2017,
        prov_cat_x = "all_types"
    )

## Clean up ----
lorenz_df <- lorenz_df %>%
    categorize_w() %>%
    categorize_ndc() %>%
    categorize_prov_cat() %>%
    categorize_meas() %>%
    categorize_prov_state() %>%
    mirror_lorenz_curve() %>%
    dplyr::group_by(ndc_type, year, prov_state, meas, prov_cat, w) %>%
    flag_landmarks() %>%
    make_Lp1_labels() %>%
    dplyr::ungroup()

## Plot ----
p1 <- plot_lorenz_one_year_same_facet_color(
    lorenz_df %>%
        dplyr::filter(w == 0,
                      meas %in% c("mme", "n_prescriptions"))
    ) +
    ggplot2::facet_wrap( ~ meas_cat, nrow = 1) + 
    scale_color_brewer(name = NULL, palette = "Set1")

## Save them ----
fs::dir_create(here::here("plots"))
fs::dir_create(here::here("grobs"))

ggplot2::ggsave(
    here::here("plots", "fig01_lorenz_US_2017.pdf"),
    p1,
    width = 5.5,
    height = 3.25,
    scale = 1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "fig01_lorenz_US_2017.png"),
    p1,
    width = 5.5,
    height = 3.25,
    scale = 1,
    dpi = 300
)
saveRDS(p1, here::here("grobs", "fig01_lorenz_US_2017.RDS"), compress = "xz")
