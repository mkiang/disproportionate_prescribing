library(tidyverse)
library(here)
source(here::here("code", "mk_nytimes.R"))
source(here::here("code", "utils.R"))

plot_ineq_over_time <-
    function(ineq_df,
             state_x = "US",
             cat_x = "all_types",
             w_x = 0,
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
             drop_x = TRUE,
             target_col = "gini") {
        
        sub_ineq <- ineq_df %>%
            filter(
                prov_state == state_x,
                prov_cat == cat_x,
                w == w_x,
                meas %in% meas_x,
                ndc_type %in% ndc_x
            ) %>%
            rename(y_val = target_col)
        
        ggplot(
            sub_ineq,
            aes(
                x = year,
                y = y_val,
                fill = ndc_cat,
                color = ndc_cat, 
                group = ndc_cat
            ),
            clip = "off"
        ) + 
            geom_line(alpha = .9) + 
            # geom_point(color = "white", size = 2.75) +
            geom_point(alpha = .9) +
            scale_color_brewer(NULL, palette = "Dark2", drop = drop_x) +
            scale_fill_brewer(NULL, palette = "Dark2", drop = drop_x) +
            scale_x_continuous(NULL,
                               breaks = c(2003, 2010, 2017),
                               expand = c(0, 0)) +
            scale_y_continuous(return_ineq_name(target_col), 
                               expand = y_expand, 
                               limits = y_lim) + 
            mk_nytimes(axis.text.x = element_text(hjust = c(0, rep(.5, 1), 1)),
                       legend.position = "bottom") + 
            facet_grid(~ meas_cat)
    }
