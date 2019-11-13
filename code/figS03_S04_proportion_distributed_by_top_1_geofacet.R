## Figure S03 and S04 - Proportion of (a) total dose and (b) prescriptions 
## by the top 1% by state.

## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "utils.R"))
source(here::here("code", "plotting_utils", "plot_lorenz_top_p_geofacet.R"))

## Data ----
top_lorenz_df <-
    readRDS(here::here("data", "lorenz_top_p_all.RDS")) %>%
    categorize_meas() %>%
    make_p1_labels() %>%
    make_Lp1_labels() %>%
    categorize_ndc()

## Plot ----
p1 <- plot_lorenz_top_p_geofacet(
    lorenz_top_p_df = top_lorenz_df,
    w_x = 0, 
    ndc_x = c("benzos",
              "opioids"),
    meas_x = c("mme"),
    p1_x = c("0.01"),
    cat_x = "all_types"
) 

p2 <- plot_lorenz_top_p_geofacet(
    lorenz_top_p_df = top_lorenz_df,
    w_x = 0, 
    ndc_x = c("benzos",
              "opioids"),
    meas_x = c("n_prescriptions"),
    p1_x = c("0.01"),
    cat_x = "all_types"
) 

## Save them ----
fs::dir_create(here::here("plots"))
fs::dir_create(here::here("grobs"))

ggplot2::ggsave(
    here::here("plots", "figS03_proportion_of_top1_geofacet.pdf"),
    p1,
    width = 9,
    height = 6,
    scale = 1.25,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS03_proportion_of_top1_geofacet.png"),
    p1,
    width = 9,
    height = 6,
    scale = 1.25,
    dpi = 300
)
saveRDS(p1,
        here::here("grobs", "figS03_proportion_of_top1_geofacet.RDS"),
        compress = "xz")

ggplot2::ggsave(
    here::here("plots", "figS04_proportion_of_top1_rx_geofacet.pdf"),
    p2,
    width = 9,
    height = 6,
    scale = 1.25,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS04_proportion_of_top1_rx_geofacet.png"),
    p2,
    width = 9,
    height = 6,
    scale = 1.25,
    dpi = 300
)
saveRDS(p2,
        here::here("grobs", "figS04_proportion_of_top1_rx_geofacet.RDS"),
        compress = "xz")
