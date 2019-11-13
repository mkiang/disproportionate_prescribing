## Imports ----
library(tidyverse)
library(here)
library(patchwork)
source(here::here("code", "utils.R"))
source(here::here("code", "plotting_utils", "plot_ntile_absolute_lines.R"))

## Data ----
ntile_mme <- import_ntile_mme_file(prov_state_x = "US",
                                   prov_cat_x = "all_types")

## Subset ----
## Note that adding all the comparison drugs is too much to see, so we instead
## pick the one that is highest among all the comparisons.
sub_df <- ntile_subset(ntile_mme,
                       w_x = 0,
                       ndc_type_x = c("opioids", "benzos"))

## Plot ----
p1 <- plot_ntile_absolute_lines(
    sub_df %>% filter(ndc_type == "opioids"),
    bottom_ntile = 5,
    stat_type_x = "mean",
    meas_x = c("mme", "n_prescriptions")
) +
    facet_wrap(meas_cat ~ ., scales = "free", ncol = 1)
p2 <- plot_ntile_absolute_lines(
    sub_df %>% filter(ndc_type == "benzos"),
    bottom_ntile = 5,
    stat_type_x = "mean",
    meas_x = c("mme", "n_prescriptions")
) +
    facet_wrap(meas_cat ~ ., scales = "free", ncol = 1) + 
    theme(legend.position = "none")

## Save them ----
fs::dir_create(here::here("plots"))
fs::dir_create(here::here("grobs"))

p3 <- p1 + p2 + plot_layout(nrow = 1)

ggplot2::ggsave(
    here::here("plots", "figS07_ntile_absolute_US.pdf"),
    p3,
    width = 6.5,
    height = 6,
    scale = 1.1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS07_ntile_absolute_US.png"),
    p3,
    width = 6.5,
    height = 6,
    scale = 1.1,
    dpi = 300
)
saveRDS(p3,
        here::here("grobs", "figS07_ntile_absolute_US.RDS"),
        compress = "xz")
