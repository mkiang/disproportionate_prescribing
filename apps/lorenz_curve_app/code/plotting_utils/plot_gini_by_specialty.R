library(tidyverse)
library(here)
source(here::here("code", "mk_nytimes.R"))

plot_gini_by_specialty <-
    function(gini_df,
             state_x = "US",
             year_x = 2017,
             y_lim = c(0, 1),
             y_expand = c(0, 0),
             meas_x = c("mme",
                        "n_prescriptions"),
             ndc_x = c("benzos",
                       "opioids"),
             drop_x = TRUE) {
        sub_gini <- gini_df %>%
            filter(prov_state == state_x,
                   year == year_x,
                   meas %in% meas_x,
                   ndc_type %in% ndc_x) %>%
            arrange(ndc_type, prov_cat, meas)
        
        ggplot(
            sub_gini,
            aes(
                x = specialty_cat,
                y = gini,
                ymax = gini_upper,
                ymin = gini_lower,
                fill = ndc_cat,
                color = ndc_cat
            ),
            clip = "off"
        ) +
            geom_errorbar(position = position_dodge(width = .4), width = .2) +
            geom_point(position = position_dodge(width = .4),
                       alpha = .8) +
            scale_color_brewer(NULL, palette = "Set1", drop = drop_x) +
            scale_fill_brewer(NULL, palette = "Set1", drop = drop_x) +
            scale_x_discrete("Provider specialty",
                             expand = c(0, .5)) +
            scale_y_continuous("Gini Coefficient (95% CI)",
                               expand = y_expand,
                               limits = y_lim) +
            mk_nytimes(
                axis.text.x = element_text(
                    angle = 90,
                    hjust = 1,
                    vjust = .5
                ),
                axis.text.y = element_text(vjust = c(0, .5, .5, .5, 1)),
                legend.position = "bottom"
            ) +
            facet_grid(meas_cat ~ .)
    }
