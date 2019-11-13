library(tidyverse)
library(ggrepel)
library(here)
source(here::here("code", "mk_nytimes.R"))

plot_lorenz_one_year_same_facet_color <-
    function(lorenz_df, invert_facets = FALSE) {
        p1 <- ggplot() +
            geom_segment(
                data = lorenz_df %>%
                    filter(top_presc == 1,
                           ndc_type == "opioids"),
                aes(
                    x = p1,
                    xend = p1,
                    y = 0,
                    yend = Lp1,
                ),
                linetype = "dotted",
                alpha = .75
            ) +
            geom_segment(
                data = lorenz_df %>%
                    filter(top_presc == 1,
                           ndc_type == "opioids"),
                aes(
                    x = 0,
                    xend = p1,
                    y = Lp1,
                    yend = Lp1
                ),
                linetype = "dotted",
                alpha = .75
            ) +
            geom_line(
                data = lorenz_df,
                aes(
                    x = p1,
                    y = Lp1,
                    # alpha = ndc_type == "opioids",
                    group = ndc_cat,
                    color = ndc_cat
                ),
                size = 1,
                alpha = .9
            ) +
            geom_point(
                data = lorenz_df %>%
                    filter(top_presc == 1,
                           ndc_type == "opioids"),
                aes(x = p1, y = Lp1),
                size = 3.15,
                color = "white"
            ) +
            geom_point(
                data = lorenz_df %>%
                    filter(top_presc == 1,
                           ndc_type == "opioids"),
                aes(x = p1, y = Lp1,
                    color = ndc_cat)
            ) +
            scale_alpha_manual(NULL, values = c(.35, 1)) +
            geom_label_repel(
                data = lorenz_df %>%
                    filter(top_presc == 1,
                           ndc_type == "opioids"),
                aes(x = p1,
                    y = Lp1,
                    label = Lp1_lab),
                alpha = .9,
                nudge_x = .1,
                nudge_y = -.1
            ) +
            coord_equal() +
            scale_x_continuous(
                "Top percentile of prescribers (cumulative)",
                labels = function(x)
                    sprintf("%i", (x * 100)),
                expand = c(0, 0),
                limits = c(-.005, 1.005)
            ) +
            facet_wrap(~ meas_cat) +
            scale_y_continuous("Proportion",
                               expand = c(0, 0), 
                               limits = c(0, 1.005)) +
            mk_nytimes(
                legend.position = c(1, 0),
                legend.justification = c(1, 0), 
                axis.text.x = element_text(hjust = c(0, rep(.5, 3), 1)),
                axis.text.y = element_text(vjust = c(0, rep(.5, 3), 1))
            )
        return(p1)
    }
