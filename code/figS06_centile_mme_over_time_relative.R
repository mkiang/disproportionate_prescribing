## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "utils.R"))
source(here::here("code", "plotting_utils", "plot_ntile_relative_lines.R"))

## Data ----
ntile_mme <- import_ntile_mme_file(prov_state_x = "US",
                                   prov_cat_x = "all_types")

## Subset ----
## Note that adding all the comparison drugs is too much to see, so we instead
## pick the one that is highest among all the comparisons.
sub_df <- ntile_subset(ntile_mme,
                       w_x = 0,
                       ndc_type_x = c("benzos", "opioids"))

## Plot ----
p1 <- plot_ntile_relative_lines(
    sub_df,
    bottom_ntile = 10,
    comparison_ntile = 50,
    stat_type_x = "mean",
    meas_x = c("mme", "n_prescriptions")
)

## Save them ----
fs::dir_create(here::here("plots"))
fs::dir_create(here::here("grobs"))

ggplot2::ggsave(
    here::here("plots", "figS06_ntile_relative_US.pdf"),
    p1,
    width = 5,
    height = 5,
    scale = 1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS06_ntile_relative_US.png"),
    p1,
    width = 5,
    height = 5,
    scale = 1,
    dpi = 300
)
saveRDS(p1,
        here::here("grobs", "figS06_ntile_relative_US.RDS"),
        compress = "xz")
