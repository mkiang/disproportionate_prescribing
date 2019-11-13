library(tidyverse)
library(here)
source(here::here("code", "mk_nytimes.R"))

plot_ineq_by_state <-
    function(ineq_df,
             prov_cat_x = "all_types",
             year_x = 2017,
             w_x = 0,
             y_lim = c(0, 1),
             y_expand = c(0, 0),
             meas_x = c("mme", "n_prescriptions"), 
             ndc_x = c(
                 "alprazolam",
                 "cyclobenzaprine",
                 "gabapentin",
                 "buspirone",
                 "lorazepam",
                 "dextroamphetamine",
                 "methlyphenidate",
                 "opioids"
             ),
             target_col = "gini", 
             drop_x = TRUE) {
        
        sub_ineq <- ineq_df %>%
            filter(prov_cat == prov_cat_x,
                   year == year_x,
                   w == w_x,
                   meas %in% meas_x,
                   ndc_type %in% ndc_x) %>%
            arrange(ndc_type, prov_cat, meas) %>%
            rename(y_val = target_col)
        
        ggplot(
            sub_ineq,
            aes(
                x = name_cat,
                y = y_val,
                fill = ndc_cat,
                color = ndc_cat
            ),
            clip = "off"
        ) +
            geom_point(position = position_dodge(width = .4),
                       alpha = .8) +
            scale_color_brewer(NULL, palette = "Dark2", drop = drop_x) +
            scale_fill_brewer(NULL, palette = "Dark2", drop = drop_x) +
            scale_x_discrete("Provider state (sorted by longitude)",
                             expand = c(0, .5)) +
            scale_y_continuous(return_ineq_name(target_col), 
                               expand = y_expand, 
                               limits = y_lim) + 
            mk_nytimes(
                axis.text.x = element_text(
                    angle = 90,
                    hjust = 1,
                    vjust = .5
                ),
                axis.text.y = element_text(
                    vjust = c(0, .5, .5, .5, 1)
                ),
                legend.position = "bottom"
            ) +
            facet_grid(meas_cat ~ .)
    }
