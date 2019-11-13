library(tidyverse)
library(here)
source(here::here("code", "mk_nytimes.R"))
source(here::here("code", "utils.R"))

deanpi_df <- dplyr::bind_rows(readr::read_csv(
    here::here(
        "data",
        "descriptives",
        "descriptives_deanpi_tabulations_mini_files.csv"
    )
),
readr::read_csv(
    here::here(
        "data",
        "descriptives",
        "descriptives_deanpi_tabulations.csv"
    )
) %>%
    dplyr::mutate(ndc_type = "all_prescriptions")) %>%
    dplyr::select(ndc_type, year, type, n_rows, n_unique)

deanpi_df <- dplyr::bind_rows(
    deanpi_df %>%
        dplyr::select(ndc_type, year, n_unique = n_rows) %>%
        dplyr::distinct() %>%
        dplyr::mutate(type = "prescriptions"),
    deanpi_df %>%
        dplyr::select(ndc_type, year, n_unique, type)
) %>%
    dplyr::filter(year < 2018)

deanpi_df <- deanpi_df %>%
    dplyr::mutate(type_cat = factor(
        type,
        levels = c("prescriptions", "dea_npi", "patid"),
        labels = c("Prescriptions",
                   "Unique Providers",
                   "Unique Patients"),
        ordered = TRUE
    )) %>%
    categorize_ndc()

p1 <- ggplot2::ggplot(
    deanpi_df %>%
        dplyr::filter(
            ndc_type %in% c(
                "all_prescriptions",
                "benzos", 
                "opioids"
            )
        ),
    ggplot2::aes(
        x = year,
        y = n_unique,
        group = ndc_cat,
        color = ndc_cat
    )
) +
    ggplot2::annotation_logticks(alpha = .7) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2.5, color = "white") +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::facet_grid(type_cat ~ ., scales = "free") +
    ggplot2::scale_y_log10("Count (log)") +
    ggplot2::scale_x_continuous(NULL, expand = c(0, 0.05)) +
    ggplot2::scale_color_brewer(name = NULL, 
                       palette = "Dark2") + 
    # colorspace::scale_color_discrete_qualitative(
    #     name = NULL,
    #     palette = "Set 2",
    #     guide = guide_legend(ncol = 5)
    #     ) +
    mk_nytimes(legend.position = "bottom")

ggplot2::ggsave(
    here::here("plots", "figS01_n_prescription_provider_pt_per_year.pdf"),
    p1, 
    width = 4.5,
    height = 7,
    scale = 1.1,
    device = grDevices::cairo_pdf
)

ggplot2::ggsave(
    here::here("plots", "figS01_n_prescription_provider_pt_per_year.png"),
    p1, 
    width = 4.5,
    height = 7,
    scale = 1.1,
    dpi = 300
)
