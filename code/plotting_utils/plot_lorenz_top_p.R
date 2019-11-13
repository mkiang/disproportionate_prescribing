library(ggplot2)
library(here)
source(here::here("code", "mk_nytimes.R"))

plot_lorenz_top_p <- function(lorenz_top_p_df,
                              w_x = 0,
                              ndc_x = c(
                                  "benzos", 
                                  "opioids"
                              ),
                              meas_x = "mme",
                              cat_x = "all_types",
                              state_x = "US",
                              p1_x = c("0.01"),
                              by_x = "ndc") {
    sub_df <- top_lorenz_df %>%
        dplyr::filter(
            w == w_x,
            ndc_type %in% ndc_x,
            meas %in% meas_x,
            prov_cat == cat_x,
            prov_state == state_x,
            p1_round %in% p1_x
        )
    
    if (NROW(sub_df) == 0) {
        return(NULL)
    }
    
    if (by_x == "ndc") {
        y_lab <- "Percent of providers"
        
        p1 <- ggplot(sub_df,
                     aes(x = year,
                         y = Lp1,
                         color = p1_lab)) +
            mk_nytimes(legend.position = c(1, 1),
                       legend.justification = c(1, 1)) + 
            facet_grid( ~ ndc_cat)
    } else if (by_x == "p1") {
        y_lab <- "Proportion dispensed"
        
        p1 <- ggplot(sub_df,
               aes(
                   x = year,
                   y = Lp1,
                   color = ndc_cat
               )) +
            facet_grid(~ p1_lab) +
            mk_nytimes(legend.position = "bottom")
    } else if (by_x == "meas") {
        y_lab <- "Proportion"
        
        p1 <- ggplot(sub_df,
               aes(
                   x = year,
                   y = Lp1,
                   color = ndc_cat
               )) +
            facet_grid(~ meas_cat) +
            mk_nytimes(legend.position = "bottom")
    }
    
    p1 <- p1 + geom_line(size = 1, alpha = .9) +
        scale_x_continuous(NULL, expand = c(0, 0)) +
        scale_y_continuous(y_lab,
                           expand = c(0, 0),
                           limits = c(0, 1))
    
    if (NROW(ndc_x) < 9) {
        p1 + scale_color_brewer(NULL, palette = "Set1") 
    } else {
        p1 + colorspace::scale_color_discrete_qualitative(
            name = NULL, 
            palette = "Dark 2")
    }
}
