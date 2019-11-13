library(tidyverse)
library(here)
source(here::here("code", "utils.R"))
source(here::here("code", "plotting_utils", "plot_gini_by_specialty.R"))

gini_df <- read_csv(here::here("data", "gini_with_ci.csv"))

gini_df <- gini_df %>%
    categorize_meas() %>%
    categorize_ndc() %>%
    categorize_prov_cat() %>%
    categorize_prov_state() %>%
    filter(prov_cat != "cat_of_interest")

p1 <- plot_gini_by_specialty(
    gini_df,
    state_x = "US",
    year_x = 2017,
    y_lim = c(NA, NA),
    y_expand = c(0, .05),
    meas_x = c("mme",
               "n_prescriptions"),
    ndc_x = c(
        "benzos", 
        "opioids"
    ),
    drop_x = TRUE
)

## Save them ----
fs::dir_create(here("plots"))
fs::dir_create(here("grobs"))

ggsave(
    here("plots", "figS17_gini_2017_by_specialty.pdf"),
    p1,
    width = 5,
    height = 5,
    scale = 1.1,
    device = cairo_pdf
)
ggsave(
    here("plots", "figS17_gini_2017_by_specialty.png"),
    p1,
    width = 5,
    height = 5,
    scale = 1.1,
    dpi = 300
)

saveRDS(p1,
        here("grobs", "figS17_gini_2017_by_specialty.RDS"),
        compress = "xz")
