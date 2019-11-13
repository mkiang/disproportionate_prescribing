library(tidyverse)
library(here)
library(patchwork)
source(here::here("code", "mk_nytimes.R"))

cdc_guidelines <-
    readr::read_csv(here::here("data", "ntile_cdc_guidelines.csv")) %>%
    dplyr::filter(ndc_type == "opioids",
           prov_state == "US",
           prov_cat == "all_types",
           !is.na(dea_ntile))

sub_df <- cdc_guidelines %>%
    dplyr::select(
        year,
        dea_ntile,
        n_prescriptions,
        n_deanpi,
        n_patients,
        prop_days_sup_7:prop_mme_per_day_90
    ) %>%
    tidyr::gather(meas, value, prop_days_sup_7:prop_mme_per_day_90) %>%
    dplyr::mutate(meas_cat = factor(
        meas,
        levels = c(
            "prop_days_sup_7",
            "prop_days_sup_30",
            "prop_mme_per_day_50",
            "prop_mme_per_day_90"
        ),
        labels = c(">7 days", ">30 days", "50+ MME/day", "90+ MME/day"),
        ordered = TRUE
    ))

p1 <- ggplot2::ggplot(sub_df %>%
                 dplyr::filter(meas == "prop_days_sup_7"),
             ggplot2::aes(x = year,
                 y = dea_ntile,
                 fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_y_reverse("Provider centile group", expand = c(0, 0)) +
    ggplot2::scale_x_continuous(NULL, expand = c(0, 0)) +
    ggplot2::scale_fill_viridis_c(
        "Proportion of prescriptions for > 7 days",
        trans = "log1p",
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barwidth = ggplot2::unit(7.5, "cm"),
            barheight = ggplot2::unit(.5, "cm")
        )
    ) +
    mk_nytimes(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figSX08_cdc_guidelines_A.pdf"),
    p1,
    width = 3,
    height = 3,
    device = grDevices::cairo_pdf,
    scale = 1.25
)
ggplot2::ggsave(
    here::here("plots", "figSX08_cdc_guidelines_A.png"),
    p1,
    width = 3,
    height = 3,
    dpi = 300,
    scale = 1.25
)

p2 <- ggplot2::ggplot(sub_df %>%
                 dplyr::filter(meas == "prop_days_sup_21"),
             ggplot2::aes(x = year,
                 y = dea_ntile,
                 fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_y_reverse("Provider centile group", expand = c(0, 0)) +
    ggplot2::scale_x_continuous(NULL, expand = c(0, 0)) +
    ggplot2::scale_fill_viridis_c(
        "Proportion of prescriptions for > 21 days",
        trans = "log1p",
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barwidth = ggplot2::unit(7.5, "cm"),
            barheight = ggplot2::unit(.5, "cm")
        )
    ) +
    mk_nytimes(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figSX08_cdc_guidelines_B.pdf"),
    p2,
    width = 3,
    height = 3,
    device = grDevices::cairo_pdf,
    scale = 1.25
)
ggplot2::ggsave(
    here::here("plots", "figSX08_cdc_guidelines_B.png"),
    p2,
    width = 3,
    height = 3,
    dpi = 300,
    scale = 1.25
)

p3 <- ggplot2::ggplot(sub_df %>%
                 dplyr::filter(meas == "prop_mme_per_day_50"),
             ggplot2::aes(x = year,
                 y = dea_ntile,
                 fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_y_reverse("Provider centile group", expand = c(0, 0)) +
    ggplot2::scale_x_continuous(NULL, expand = c(0, 0)) +
    ggplot2::scale_fill_viridis_c(
        "Proportion of prescriptions for 50+ MME/day",
        trans = "log1p",
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barwidth = ggplot2::unit(7.5, "cm"),
            barheight = ggplot2::unit(.5, "cm")
        )
    ) +
    mk_nytimes(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figSX08_cdc_guidelines_C.pdf"),
    p3,
    width = 3,
    height = 3,
    device = grDevices::cairo_pdf,
    scale = 1.25
)
ggplot2::ggsave(
    here::here("plots", "figSX08_cdc_guidelines_C.png"),
    p3,
    width = 3,
    height = 3,
    dpi = 300,
    scale = 1.25
)

p4 <- ggplot2::ggplot(sub_df %>%
                 dplyr::filter(meas == "prop_mme_per_day_90"),
             ggplot2::aes(x = year,
                 y = dea_ntile,
                 fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_y_reverse("Provider centile group", expand = c(0, 0)) +
    ggplot2::scale_x_continuous(NULL, expand = c(0, 0)) +
    ggplot2::scale_fill_viridis_c(
        "Proportion of prescriptions for 90+ MME/day",
        trans = "log1p",
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barwidth = ggplot2::unit(7.5, "cm"),
            barheight = ggplot2::unit(.5, "cm")
        )
    ) +
    mk_nytimes(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figSX08_cdc_guidelines_D.pdf"),
    p4,
    width = 3,
    height = 3,
    device = grDevices::cairo_pdf,
    scale = 1.25
)
ggplot2::ggsave(
    here::here("plots", "figSX08_cdc_guidelines_D.png"),
    p4,
    width = 3,
    height = 3,
    dpi = 300,
    scale = 1.25
)

p_all <- p1 + p2 + p3 + p4 + patchwork::plot_layout(ncol = 2)
ggplot2::ggsave(
    here::here("plots", "figS08_cdc_guidelines_all.pdf"),
    p_all,
    width = 6,
    height = 6,
    device = grDevices::cairo_pdf,
    scale = 1.5
)
ggplot2::ggsave(
    here::here("plots", "figS08_cdc_guidelines_all.png"),
    p_all,
    width = 6,
    height = 6,
    dpi = 300,
    scale = 1.5
)

cdc_guidelines %>%
    filter(prov_state == "US", 
           prov_cat == "all_types", 
           ndc_type == "opioids", 
           dea_ntile > 1) %>%
    transmute(
        year = year, 
        dea_ntile = dea_ntile, 
        n_prescriptions = n_prescriptions, 
        n_deanpi = n_deanpi, 
        n_patients = n_patients, 
        n_prov_over_7_days = n_deanpi * (prop_days_sup_7),
        n_prov_over_50mme = n_deanpi * (prop_mme_per_day_50)) %>% 
    group_by(year) %>% 
    summarize(total_prov = sum(n_deanpi), 
              prov_over_7_days = sum(n_prov_over_7_days), 
              prov_over_50mme = sum(n_prov_over_50mme)) %>% 
    mutate(prop_over_7_days = prov_over_7_days / total_prov, 
           prop_over_50mme = prov_over_50mme / total_prov)

cdc_guidelines %>% 
    filter(dea_ntile == 1) %>% 
    arrange(year) %>% 
    select(year, n_prescriptions:prop_days_sup_7, prop_mme_per_day_50)

cdc_guidelines %>%
    filter(prov_state == "US", 
           prov_cat == "all_types", 
           ndc_type == "opioids") %>%
    transmute(
        year = year, 
        dea_ntile = dea_ntile, 
        n_prescriptions = n_prescriptions, 
        n_deanpi = n_deanpi, 
        n_patients = n_patients, 
        n_prov_over_7_days = n_deanpi * (prop_days_sup_7),
        n_prov_over_50mme = n_deanpi * (prop_mme_per_day_50)) %>% 
    group_by(year) %>% 
    summarize(total_prov = sum(n_deanpi), 
              prov_over_7_days = sum(n_prov_over_7_days), 
              prov_over_50mme = sum(n_prov_over_50mme)) %>% 
    mutate(prop_over_7_days = prov_over_7_days / total_prov, 
           prop_over_50mme = prov_over_50mme / total_prov)
