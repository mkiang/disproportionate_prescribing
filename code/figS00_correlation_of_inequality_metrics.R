library(tidyverse)
library(here)
library(GGally)
source(here::here("code", "utils.R"))
source(here::here("code", "mk_nytimes.R"))

ineq_df <- readr::read_csv(here::here("data", "ineq_estimates_all.csv.xz"))

ineq_df <- ineq_df %>% 
    categorize_meas() %>% 
    categorize_ndc() %>% 
    categorize_prov_cat() %>% 
    categorize_prov_state() %>% 
    categorize_w()

ineq_df %>% dplyr::select(gini:entropy) %>% as.matrix(.) %>% stats::cor(.)

p1 <- GGally::ggpairs(
    ineq_df %>%
        dplyr::filter(w == 0,
                      prov_state == "US",
                      prov_cat == "all_types") %>%
        dplyr::select(gini:entropy),
    upper = list(continuous = "density"),
    lower = list(continuous = wrap("points", alpha = .3, size = .2)),
    columnLabels = c("Gini", "Ricci-Schutz", "Atkinson", "Theil", "Entropy")
) +
    mk_nytimes() 

## Save them ----
fs::dir_create(here::here("plots"))
fs::dir_create(here::here("grobs"))

ggplot2::ggsave(
    here::here("plots", "figS0_ineq_pairs_plot.pdf"),
    p1,
    width = 4.5,
    height = 4,
    scale = 1.2,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS0_ineq_pairs_plot.png"),
    p1,
    width = 4.5,
    height = 4,
    scale = 1.2,
    dpi = 300
)

saveRDS(p1, here::here("grobs", "figS0_ineq_pairs_plot.RDS"), compress = "xz")
