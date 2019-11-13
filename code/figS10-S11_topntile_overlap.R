library(tidyverse)
library(here)
source(here("code", "utils.R"))
source(here("code", "plotting_utils", "plot_ntile_overlap.R"))

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

p1 <- plot_ntile_overlap(overlap_df,
                         ndc_x = "opioids",
                         jaccard = FALSE)

p2 <- plot_ntile_overlap(overlap_df,
                         ndc_x = "opioids",
                         jaccard = TRUE)

## Save them ----
fs::dir_create(here("plots"))
fs::dir_create(here("grobs"))

ggsave(
    here("plots",
         "figS09_topntile_overlap_opioids.pdf"),
    p1,
    width = 6,
    height = 3.25,
    scale = 1.4,
    device = cairo_pdf
)
ggsave(
    here("plots",
         "figS09_topntile_overlap_opioids.png"),
    p1,
    width = 6,
    height = 3.25,
    scale = 1.4,
    dpi = 300
)

saveRDS(p1,
        here("grobs", "figS09_topntile_overlap_opioids.RDS"),
        compress = "xz")


ggsave(
    here("plots",
         "figS11_topntile_jaccard_opioids.pdf"),
    p2,
    width = 6,
    height = 3.25,
    scale = 1.4,
    device = cairo_pdf
)
ggsave(
    here("plots",
         "figS11_topntile_jaccard_opioids.png"),
    p2,
    width = 6,
    height = 3.25,
    scale = 1.4,
    dpi = 300
)

saveRDS(p2,
        here("grobs", "figS11_topntile_jaccard_opioids.RDS"),
        compress = "xz")

p1 <- plot_ntile_overlap(overlap_df,
                         ndc_x = "benzos",
                         jaccard = FALSE)

p2 <- plot_ntile_overlap(overlap_df,
                         ndc_x = "benzos",
                         jaccard = TRUE)

## Save them ----
ggsave(
    here("plots",
         "figS10_topntile_overlap_benzos.pdf"),
    p1,
    width = 6,
    height = 3.25,
    scale = 1.4,
    device = cairo_pdf
)
ggsave(
    here("plots",
         "figS10_topntile_overlap_benzos.png"),
    p1,
    width = 6,
    height = 3.25,
    scale = 1.4,
    dpi = 300
)

saveRDS(p1,
        here("grobs", "figS10_topntile_overlap_benzos.RDS"),
        compress = "xz")


ggsave(
    here("plots",
         "figS12_topntile_jaccard_benzos.pdf"),
    p2,
    width = 6,
    height = 3.25,
    scale = 1.4,
    device = cairo_pdf
)
ggsave(
    here("plots",
         "figS12_topntile_jaccard_benzos.png"),
    p2,
    width = 6,
    height = 3.25,
    scale = 1.4,
    dpi = 300
)

saveRDS(p2,
        here("grobs", "figS12_topntile_jaccard_benzos.RDS"),
        compress = "xz")
