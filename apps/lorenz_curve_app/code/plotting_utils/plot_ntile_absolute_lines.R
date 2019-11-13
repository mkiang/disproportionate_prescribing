library(ggplot2)
library(here)
source(here::here("code", "utils.R"))
source(here::here("code", "mk_nytimes.R"))

plot_ntile_absolute_lines <-
    function(sub_ntile,
             bottom_ntile = 25,
             stat_type_x = "mean",
             meas_x = c("mme",
                        "mme_per_day",
                        "mme_per_pt",
                        "mme_per_prescription")) {
        y_lab <- case_when(
            stat_type_x == "mean" ~ "Average", 
            stat_type_x == "median" ~ "Median", 
            stat_type_x == "total" ~ "Total", 
            stat_type_x == "sd" ~ "Standard deviation"
        )
        
        ggplot(
            sub_ntile %>%
                filter(
                    stat_type == stat_type_x,
                    meas %in% meas_x,
                    top_ntile %in% c(1:bottom_ntile)
                ),
            aes(
                x = year,
                color = top_ntile,
                group = top_ntile,
                y = value
            )
        ) +
            geom_line() +
            scale_y_continuous(
                y_lab,
                # labels = function(x)
                #     sprintf("%i", round(x / 100)),
                labels = function(x) scales::scientific(x, digits = 2), 
                expand = c(.01, 0)
            ) +
            scale_x_continuous(NULL, expand = c(0, 0)) +
            scale_color_distiller(
                "Provider centile group",
                palette = "Blues",
                breaks = scales::pretty_breaks(),
                guide = guide_colorbar(
                    title.position = "top",
                    barwidth = unit(7.5, "cm"),
                    barheight = unit(.3, "cm")
                )
            ) +
            mk_nytimes(legend.position = "bottom") +
            facet_grid(meas_cat ~ ndc_cat, scales = "free")
    }
