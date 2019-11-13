library(tidyverse)
library(here)
library(geofacet)
source(here::here("code", "mk_nytimes.R"))
source(here::here("code", "utils.R"))

plot_gini_over_time_geofacet <-
    function(gini_df,
             cat_x = "all_types", 
             y_lim = c(0, 1), 
             y_expand = c(0, 0), 
             meas_x = "mme", 
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
             drop_x = TRUE,
             legend_title = NULL) {
        
        sub_gini <- gini_df %>%
            filter(
                prov_state != "US",
                prov_cat == cat_x,
                meas == meas_x,
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
            scale_color_brewer(legend_title, palette = "Set1", drop = drop_x) +
            scale_fill_brewer(legend_title, palette = "Set1", drop = drop_x) +
            scale_x_continuous(NULL,
                               breaks = c(2003, 2010, 2017),
                               expand = c(0, 0)) +
            scale_y_continuous("Gini (95% CI)", 
                               expand = y_expand, 
                               limits = y_lim) + 
            mk_nytimes(axis.text.x = element_text(hjust = c(0, rep(.5, 1), 1)),
                       legend.position = c(1, 0), 
                       legend.justification = c(1, 0)) + 
            facet_geo(~ prov_state)
    }
