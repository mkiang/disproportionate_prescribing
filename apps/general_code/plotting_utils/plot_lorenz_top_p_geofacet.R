library(ggplot2)
library(here)
library(geofacet)
source(here::here("code", "mk_nytimes.R"))

plot_lorenz_top_p_geofacet <-
    function(lorenz_top_p_df,
             w_x = 0,
             ndc_x = c("benzos",
                       "opioids"),
             meas_x = "mme",
             cat_x = "all_types",
             p1_x = c("0.01")) {
        sub_df <- top_lorenz_df %>%
            dplyr::filter(
                w == w_x,
                ndc_type %in% ndc_x,
                meas == meas_x,
                prov_cat == cat_x,
                prov_state != "US",
                p1_round %in% p1_x
            )
        
        if (NROW(sub_df) == 0) {
            return(NULL)
        }
        
        if (meas_x == "mme") {
            y_lab <- "Proportion of doses"
        } else if (meas_x == "n_prescriptions") {
            y_lab <- "Proportion of prescriptions"
        } else {
            y_lab <- NULL
        }
        
        p1 <- ggplot(sub_df,
                     aes(
                         x = year,
                         y = Lp1,
                         color = ndc_cat,
                         group = ndc_cat
                     )) +
            geom_line(size = 1, alpha = .9) +
            scale_x_continuous(
                NULL,
                expand = c(0, 0),
                breaks = c(2006, 2010, 2014, 2017),
                labels = c("'06", "'10", "'14", "'17")
            ) +
            scale_y_continuous(y_lab,
                               expand = c(0, 0),
                               limits = c(0, 1)) +
            scale_color_brewer(NULL, palette = "Set1") +
            facet_geo( ~ prov_state) +
            mk_nytimes(
                legend.position = c(1, 0),
                legend.justification = c(1, 0),
                axis.text.x = element_text(hjust = c(0, .5, .5, 1))
            )
        
        return(p1)
    }
