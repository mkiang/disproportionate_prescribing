library(tidyverse)
library(here)
library(ggrepel)
source(here::here("code", "utils.R"))
source(here::here("code", "mk_nytimes.R"))

ranks_df <- dplyr::bind_rows(
    readr::read_csv(here::here("data", "opioid_prescription_ranks.csv")) %>%
        dplyr::mutate(ndc_type = "opioids"),
    readr::read_csv(here::here("data", "benzos_prescription_ranks.csv")) %>%
        dplyr::mutate(ndc_type = "benzos")
) %>%
    categorize_ndc()

## Flag all generics that have ever been in top 5 ----
top5 <- ranks_df %>%
    dplyr::group_by(ndc_type) %>%
    dplyr::filter(mme_rank <= 5) %>%
    dplyr::pull(generic_name) %>%
    unique(.)

top10 <- ranks_df %>%
    dplyr::group_by(ndc_type) %>%
    dplyr::filter(mme_rank <= 10) %>%
    dplyr::pull(generic_name) %>%
    unique(.)

ranks_df <- ranks_df %>%
    dplyr::mutate(
        top5 = generic_name %in% top5,
        top10 = generic_name %in% top10,
        subset_cat =
            factor(
                subset,
                levels = c("top_ntile", "all_ntiles"),
                labels = c("Top 1%", "All providers"),
                ordered = TRUE
            ),
        mme = mme / 1000000
    )

p1 <- ggplot2::ggplot(
    ranks_df %>%
        dplyr::filter(top5),
    ggplot2::aes(
        x = year,
        y = mme,
        color = generic_name,
        group = interaction(ndc_type, generic_name)
    ),
    clip = "off"
) +
    ggplot2::geom_line() +
    ggrepel::geom_text_repel(
        data = ranks_df %>%
            dplyr::filter(top5, !is.na(n_prescriptions)) %>%
            dplyr::group_by(generic_name) %>%
            dplyr::filter(year == max(year)) %>%
            dplyr::ungroup(),
        ggplot2::aes(label = generic_name),
        nudge_x      = 0.2,
        direction    = "y",
        hjust        = 0,
        segment.size = 0
    ) +
    colorspace::scale_color_discrete_qualitative(name = "Drug",
                                                 palette = "Dark 3") +
    # ggplot2::facet_grid(subset_cat ~ ., scales = "free") +
    mk_nytimes(legend.position = "none") +
    ggplot2::scale_x_continuous(NULL, limits = c(2003, 2028), expand = c(0, 0)) +
    ggplot2::scale_y_continuous("Amount (mg; millions)") +
    ggplot2::facet_grid(ndc_cat ~ subset_cat, scales = "free")

## Save them ----
fs::dir_create(here::here("plots"))
fs::dir_create(here::here("grobs"))

ggplot2::ggsave(
    here::here("plots", "figS05_benzo_opioids_prescribed_by_type.pdf"),
    p1,
    width = 5,
    height = 6,
    scale = 1.2,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS05_benzo_opioids_prescribed_by_type.png"),
    p1,
    width = 8,
    height = 8,
    scale = 1.2,
    dpi = 300
)

saveRDS(p1,
        here::here("grobs", "figS05_benzo_opioids_prescribed_by_type.RDS"),
        compress = "xz")
