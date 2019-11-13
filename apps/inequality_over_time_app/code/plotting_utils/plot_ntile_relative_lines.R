library(ggplot2)
library(here)
source(here::here("code", "utils.R"))
source(here::here("code", "mk_nytimes.R"))

plot_ntile_relative_lines <-
    function(sub_ntile,
             bottom_ntile = 25,
             comparison_ntile = 50, 
             stat_type_x = "mean",
             meas_x = c("mme", "mme_per_day", "n_prescriptions")) {
        if (bottom_ntile > comparison_ntile) {
            stop("Comparison group must be lower or equal to bottom group.")
        }
        
        y_lab <- case_when(
            stat_type_x == "mean" ~ sprintf(
                "Average relative to centile group %s",
                comparison_ntile
            ),
            stat_type_x == "median" ~ sprintf(
                "Median relative to centile group %s",
                comparison_ntile
            ),
            stat_type_x == "total" ~ sprintf(
                "Total relative to centile group %s",
                comparison_ntile
            ),
            stat_type_x == "sd" ~ "Standard deviation of amount prescribed"
        )
        
        
        sub_ntile <- sub_ntile %>% 
            left_join(
                extract_ntile_comparison(sub_ntile, 
                                         comp_ntile_x = comparison_ntile)
            ) %>% 
            mutate(rel_value = value / comp_value)
        
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
                y = rel_value
            )
        ) +
            geom_line() +
            scale_y_continuous(
                y_lab,
                expand = c(.01, 0)
            ) +
            scale_x_continuous(NULL, expand = c(0, 0)) +
            scale_color_distiller(
                "Provider centile group",
                palette = "Greens",
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
