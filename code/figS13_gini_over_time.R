library(tidyverse)
library(here)
source(here::here("code", "utils.R"))
source(here::here("code", "plotting_utils", "plot_gini_over_time.R"))

ineq_df <- readr::read_csv(here::here("data", "gini_with_ci.csv"))

ineq_df <- ineq_df %>%
    categorize_meas() %>%
    categorize_ndc() %>%
    categorize_prov_cat() %>%
    categorize_prov_state()

p1 <- plot_gini_over_time(
    ineq_df,
    state_x = "US",
    cat_x = "all_types",
    y_lim = c(NA, NA),
    y_expand = c(0, 0),
    meas_x = c("mme", "n_prescriptions"),
    ndc_x = c(# "alprazolam",
        # "cyclobenzaprine",
        # "gabapentin",
        # "buspirone",
        # "lorazepam",
        # "dextroamphetamine",
        # "methlyphenidate",
        "benzos",
        "opioids"),
    drop_x = TRUE
)

## Save them ----
fs::dir_create(here::here("plots"))
fs::dir_create(here::here("grobs"))

ggplot2::ggsave(
    here::here("plots", "figS3_gini_2003to2017.pdf"),
    p1,
    width = 4.5,
    height = 3,
    scale = 1.2,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS3_gini_2003to2017.png"),
    p1,
    width = 4.5,
    height = 3,
    scale = 1.2,
    dpi = 300
)

saveRDS(p1,
        here::here("grobs", "figS3_gini_2003to2017.RDS"),
        compress = "xz")
