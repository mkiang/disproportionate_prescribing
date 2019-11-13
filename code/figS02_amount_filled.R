library(tidyverse)
library(ggrepel)
library(here)
library(colorspace)
source(here::here("code", "mk_nytimes.R"))
source(here::here("code", "utils.r"))

mme_summary <-
    readr::read_csv(here::here("data",
                               "descriptives",
                               "descriptives_mme_summaries.csv.xz"))

mme_summary <- mme_summary %>%
    categorize_meas() %>%
    categorize_ndc() %>%
    categorize_prov_cat() %>%
    categorize_prov_state() %>%
    categorize_w() %>%
    dplyr::mutate(sum = sum / 1000000000) %>%
    dplyr::mutate(ndc_cat_label =
                      sprintf("%s (%0.2f)",
                              ndc_cat,
                              round(sum, 2)))

sub_df <- mme_summary %>%
    dplyr::filter(
        w == 0,
        prov_cat == "all_types",
        prov_state == "US",
        meas == "mme",
        ndc_type %in% c(
            # "alprazolam",
            # "cyclobenzaprine",
            # "gabapentin",
            # "buspirone",
            # "lorazepam",
            # "dextroamphetamine",
            # "methlyphenidate",
            "benzos",
            "opioids"
        )
    )

p1 <- ggplot2::ggplot(sub_df,
                      ggplot2::aes(x = year,
                                   y = sum,
                                   color = ndc_cat)) +
    ggplot2::annotation_logticks(alpha = .5) +
    colorspace::scale_color_discrete_qualitative(palette = "Dark 3") +
    # scale_color_brewer(palette = "Set1") +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(NULL,
                                limits = c(2003, 2023),
                                expand = c(0, 0.25)) +
    ggplot2::scale_y_log10("Total amount dispensed (billions)") +
    ggplot2::geom_point(data = sub_df %>%
                            dplyr::filter(year == max(year))) +
    ggrepel::geom_text_repel(
        data = sub_df %>%
            dplyr::filter(year == max(year)),
        ggplot2::aes(label = ndc_cat_label),
        nudge_x      = 0.2,
        direction    = "y",
        hjust        = 0,
        segment.size = 0.05,
        size = 5
    ) +
    mk_nytimes(legend.position = "none")

## Save them ----
fs::dir_create(here::here("plots"))
fs::dir_create(here::here("grobs"))

ggplot2::ggsave(
    here::here("plots",
               "figS02_amount_filled.pdf"),
    p1,
    width = 6,
    height = 4,
    scale = 1.25,
    device = grDevices::cairo_pdf
)

ggplot2::ggsave(
    here::here("plots",
               "figS02_amount_filled.png"),
    p1,
    width = 6,
    height = 4,
    scale = 1.25,
    dpi = 300
)

saveRDS(p1, here::here("grobs", "figS2_amount_filled.RDS"), compress = "xz")
