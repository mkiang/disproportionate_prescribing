library(tidyverse)
library(ggrepel)
library(here)
source(here::here("code", "mk_nytimes.R"))

plot_lorenz_one_year <-
    function(lorenz_df,
             invert_facets = FALSE,
             color_lines = FALSE, 
             text_labels = FALSE) {
        p1 <- ggplot() +
            geom_segment(
                data = lorenz_df %>%
                    filter(top_presc == 1),
                aes(
                    x = p1,
                    xend = p1,
                    y = 0,
                    yend = Lp1
                ),
                linetype = "dotted",
                alpha = .75
            ) +
            geom_segment(
                data = lorenz_df %>%
                    filter(top_presc == 1),
                aes(
                    x = 0,
                    xend = p1,
                    y = Lp1,
                    yend = Lp1
                ),
                linetype = "dotted",
                alpha = .75
            ) +
            geom_line(data = lorenz_df,
                      aes(x = p1, y = Lp1, color = ndc_cat)) +
            geom_point(
                data = lorenz_df %>%
                    filter(top_presc == 1),
                aes(x = p1, y = Lp1, color = ndc_cat),
                size = 3.25,
                color = "white"
            ) +
            geom_point(data = lorenz_df %>%
                           filter(top_presc == 1),
                       aes(x = p1, y = Lp1, color = ndc_cat)) +
            coord_equal() +
            scale_x_continuous(
                "Top percentile of prescribers (cumulative)",
                labels = function(x)
                    sprintf("%i", (x * 100)),
                expand = c(.01, 0)
            ) +
            scale_y_continuous("Proportion",
                               expand = c(.01, 0)) +
            scale_color_brewer(NULL, palette = "Dark2") +
            mk_nytimes(
                legend.position = "none",
                axis.text.x = element_text(hjust = c(0, rep(.5, 3), 1)),
                axis.text.y = element_text(vjust = c(0, rep(.5, 3), 1))
            )
        
        if (text_labels) {
            p1 <- p1  +
                geom_label_repel(
                    data = lorenz_df %>%
                        filter(top_presc == 1),
                    aes(
                        x = p1,
                        y = Lp1,
                        label = Lp1_lab,
                        color = ndc_cat
                    ),
                    alpha = .8,
                    nudge_x = .1,
                    nudge_y = -.1
                )
        }
        
        if (invert_facets) {
            p1 <- p1 + facet_grid(ndc_cat ~ meas_cat)
        } else {
            p1 <- p1 + facet_grid(meas_cat ~ ndc_cat)
        }
        
        if (color_lines) {
            return(p1)
        } else {
            p1 + scale_color_manual(
                values = rep("black",n_distinct(lorenz_df$ndc_cat)))
        }
    }
