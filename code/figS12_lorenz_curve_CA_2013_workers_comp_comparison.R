library(tidyverse)
library(here)
source(here::here("code", "utils.R"))
source(here::here("code", "plotting_utils", "plot_lorenz_one_year.R"))

lorenz_df <- import_lorenz_curves(
    state_x = c("CA"),
    ndc_x = c("schii_opioids"),
    year_x = 2009,
    prov_cat_x = "all_types"
) %>% 
    dplyr::filter(w == 0, 
           meas == "mme")

lorenz_df <- lorenz_df %>%
    categorize_w() %>%
    categorize_ndc() %>%
    categorize_prov_cat() %>%
    categorize_meas() %>%
    categorize_prov_state() %>%
    mirror_lorenz_curve() %>%
    dplyr::group_by(ndc_type, year, prov_state, meas, prov_cat, w) %>%
    flag_landmarks() %>%
    dplyr::ungroup() %>% 
    make_Lp1_labels()

p1 <- plot_lorenz_one_year(lorenz_df, text_labels = TRUE) + 
    ggplot2::facet_null()
ggplot2::ggsave(
    here::here("plots", "figS11_comparison_to_CA_workers_comp.pdf"),
    p1,
    width = 3,
    height = 3,
    scale = 1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS11_comparison_to_CA_workers_comp.png"),
    p1,
    width = 3,
    height = 3,
    scale = 1,
    dpi = 300
)
