## Figure 2 - Proportion of (a) total dose and (b) prescriptions by the top 1%.

## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "utils.R"))
source(here::here("code", "plotting_utils", "plot_lorenz_top_p.R"))

## Data ----
top_lorenz_df <-
    readRDS(here::here("data", "lorenz_top_p_all.RDS")) %>%
    categorize_meas() %>%
    make_p1_labels() %>%
    make_Lp1_labels() %>%
    categorize_ndc()

## Plot ----
p1 <- plot_lorenz_top_p(
    lorenz_top_p_df = top_lorenz_df,
    w_x = 0.000,
    ndc_x = c("benzos",
              "opioids"),
    meas_x = c("mme", "n_prescriptions"),
    p1_x = c("0.01"),
    state_x = "US",
    cat_x = "all_types",
    by_x = "meas"
) +
    theme(
        legend.position = c(1, 1),
        legend.justification = c(1, 1),
        axis.text.y = element_text(vjust = c(0, rep(.5, 3), 1))
    )

## Save them ----
fs::dir_create(here::here("plots"))
fs::dir_create(here::here("grobs"))

ggplot2::ggsave(
    here::here("plots", "fig02_proportion_of_top1.pdf"),
    p1,
    width = 5.5,
    height = 3.5,
    scale = 1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "fig02_proportion_of_top1.png"),
    p1,
    width = 5.5,
    height = 3.5,
    scale = 1,
    dpi = 300
)
saveRDS(p1,
        here::here("grobs", "fig02_proportion_of_top1.RDS"),
        compress = "xz")
