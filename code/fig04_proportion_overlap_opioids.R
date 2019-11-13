library(tidyverse)
library(here)
source(here("code", "utils.R"))
source(here("code", "plotting_utils", "plot_ntile_overlap_notext.R"))

overlap_df <- readRDS(here("data", "overlap_all.RDS"))

overlap_df <- overlap_df %>%
    categorize_w() %>%
    categorize_ndc() %>%
    categorize_prov_state() %>%
    categorize_prov_cat() %>%
    mutate(type_cat = factor(
        overlap_object,
        levels = c("dea_npi", "patid", "pair_id"),
        labels = c("Providers", "Patients", "Provider-Patient Pairs"),
        ordered = TRUE
    ))

p1 <- plot_ntile_overlap_notext(overlap_df,
                                ndc_x = "opioids",
                                jaccard = FALSE)

## Save them ----
fs::dir_create(here("plots"))
fs::dir_create(here("grobs"))

ggsave(
    here("plots",
         "fig04_topntile_overlap_opioids.pdf"),
    p1,
    width = 6,
    height = 3.5,
    scale = 1.1,
    device = cairo_pdf
)
ggsave(
    here("plots",
         "fig04_topntile_overlap_opioids.png"),
    p1,
    width = 6,
    height = 3.5,
    scale = 1.1,
    dpi = 300
)

saveRDS(p1,
        here("grobs", "fig04_topntile_overlap_opioids.RDS"),
        compress = "xz")
