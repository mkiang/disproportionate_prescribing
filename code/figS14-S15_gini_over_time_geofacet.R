library(tidyverse)
library(here)
source(here::here("code", "utils.R"))
source(here::here("code", "plotting_utils", "plot_gini_over_time_geofacet.R"))

ineq_df <- readr::read_csv(here::here("data", "gini_with_ci.csv"))

ineq_df <- ineq_df %>%
    categorize_meas() %>%
    categorize_ndc() %>%
    categorize_prov_cat() %>%
    categorize_prov_state()

p1 <- plot_gini_over_time_geofacet(
    ineq_df,
    cat_x = "all_types",
    y_lim = c(NA, NA),
    y_expand = c(0, 0),
    meas_x = "mme",
    ndc_x = c(# "alprazolam",
        # "cyclobenzaprine",
        # "gabapentin",
        # "buspirone",
        # "lorazepam",
        # "dextroamphetamine",
        # "methlyphenidate",
        "benzos",
        "opioids"),
    drop_x = TRUE,
    legend_title = "Doses"
)

p2 <- plot_gini_over_time_geofacet(
    ineq_df,
    cat_x = "all_types",
    y_lim = c(NA, NA),
    y_expand = c(0, 0),
    meas_x = "n_prescriptions",
    ndc_x = c(# "alprazolam",
        # "cyclobenzaprine",
        # "gabapentin",
        # "buspirone",
        # "lorazepam",
        # "dextroamphetamine",
        # "methlyphenidate",
        "benzos",
        "opioids"),
    drop_x = TRUE,
    legend_title = "Prescriptions"
)

## Save them ----
fs::dir_create(here::here("plots"))
fs::dir_create(here::here("grobs"))

ggplot2::ggsave(
    here::here("plots", "figS04_gini_doses_by_state.pdf"),
    p1,
    width = 9,
    height = 6,
    scale = 1.25,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS04_gini_doses_by_state.png"),
    p1,
    width = 9,
    height = 6,
    scale = 1.25,
    dpi = 300
)

saveRDS(p1,
        here::here("grobs", "figS04_gini_doses_by_state.RDS"),
        compress = "xz")

ggplot2::ggsave(
    here::here("plots", "figS05_gini_prescriptions_by_state.pdf"),
    p2,
    width = 9,
    height = 6,
    scale = 1.25,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS05_prescriptions_doses_by_state.png"),
    p2,
    width = 9,
    height = 6,
    scale = 1.25,
    dpi = 300
)

saveRDS(p2,
        here::here("grobs", "figS05_prescriptions_doses_by_state.RDS"),
        compress = "xz")