library(tidyverse)
library(here)
source(here::here("code", "mk_nytimes.R"))
source(here::here("code", "utils.R"))

plot_gini_over_time <-
    function(gini_df,
             state_x = "US",
             cat_x = "all_types", 
             y_lim = c(0, 1), 
             y_expand = c(0, 0), 
             meas_x = c("mme", "n_prescriptions"), 
             ndc_x = c(
                 # "alprazolam",
                 # "cyclobenzaprine",
                 # "gabapentin",
                 # "buspirone",
                 # "lorazepam",
                 # "dextroamphetamine",
                 # "methlyphenidate",
                 "benzos", 
                 "opioids"
             ),
             drop_x = TRUE) {
        
        sub_gini <- gini_df %>%
            filter(
                prov_state == state_x,
                prov_cat == cat_x,
                meas %in% meas_x,
                ndc_type %in% ndc_x
            )
        
        ggplot(
            sub_gini,
            aes(
                x = year,
                y = gini,
                ymin = gini_lower, 
                ymax = gini_upper, 
                fill = ndc_cat,
                color = ndc_cat, 
                group = ndc_cat
            ),
            clip = "off"
        ) + 
            geom_ribbon(alpha = .3, color = NA) + 
            geom_line(alpha = .95) + 
            # geom_point(color = "white", size = 2.75) +
            geom_point(alpha = 1) +
            scale_color_brewer(NULL, palette = "Set1", drop = drop_x) +
            scale_fill_brewer(NULL, palette = "Set1", drop = drop_x) +
            scale_x_continuous(NULL,
                               breaks = c(2003, 2010, 2017),
                               expand = c(0, 0)) +
            scale_y_continuous("Gini (95% CI)", 
                               expand = y_expand, 
                               limits = y_lim) + 
            mk_nytimes(axis.text.x = element_text(hjust = c(0, rep(.5, 1), 1)),
                       legend.position = c(1, 1), 
                       legend.justification = c(1, 1)) + 
            facet_grid(~ meas_cat)
    }
