library(ggplot2)
library(ggalluvial)
library(patchwork)
source(here("code", "utils.R"))
source(here("code", "mk_nytimes.R"))

plot_ntile_transactions <- function(transactions_df,
                                    year_x = 2017,
                                    ndc_x = "opioids",
                                    panel = "both") {
    if (!(panel %in% c("both", "prescriptions", "mme"))) {
        stop("panel must be one of: 'both', 'mme', or 'prescriptions'")
    }
    
    sub_df <- transactions_df %>%
        filter(year == year_x,
               ndc_type == ndc_x,
               !is.na(dea_ntile),
               !is.na(patid_ntile))
    
    if (panel %in% c("both", "mme")) {
        p1l_ylab <-
            sprintf("%s (%s)",
                    return_proper_name(ndc_x),
                    return_dose(ndc_x))
        
        p1_left_mme <- ggplot(sub_df,
                              aes(
                                  y = mme,
                                  axis1 = dea_ntile_cat,
                                  axis2 = patid_ntile_cat
                              ),
                              clip = "off") +
            geom_alluvium(aes(color = dea_ntile_cat,
                              fill = dea_ntile_cat),
                          alpha = .6) +
            geom_stratum(
                width = 1 / 2.5,
                fill = "black",
                alpha = 1,
                color = "white"
            ) +
            geom_label(stat = "stratum",
                       label.strata = TRUE,
                       size = 2.5) +
            mk_nytimes(panel.grid.major = element_blank(),
                       legend.position = "none") +
            scale_x_continuous(
                NULL,
                expand = c(0, 0),
                breaks = c(1, 2),
                labels = c("Provider\ncentile group", "Patient\ncentile group")
            ) +
            scale_y_continuous(
                p1l_ylab,
                labels = function(x)
                    scales::scientific(x, digits = 1),
                expand = c(0, 0)
            )
        
        x <- p1_left_mme
    }
    
    if (panel  %in% c("both", "mme")) {
        p1r_ylab <- sprintf("%s prescriptions (N)",
                            return_proper_name(ndc_x))
        
        p1_right_prescriptions <- ggplot(
            sub_df,
            aes(
                y = prescriptions,
                axis1 = dea_ntile_cat,
                axis2 = patid_ntile_cat
            ),
            clip = "off"
        ) +
            geom_alluvium(aes(color = dea_ntile_cat,
                              fill = dea_ntile_cat),
                          alpha = .6) +
            geom_stratum(
                width = 1 / 2.5,
                fill = "black",
                alpha = 1,
                color = "white"
            ) +
            geom_label(stat = "stratum",
                       label.strata = TRUE,
                       size = 2.5) +
            mk_nytimes(panel.grid.major = element_blank(),
                       legend.position = "none") +
            scale_x_continuous(
                NULL,
                expand = c(0, 0),
                breaks = c(1, 2),
                labels = c("Provider\ncentile group", "Patient\ncentile group")
            ) +
            scale_y_continuous(
                p1r_ylab,
                labels = function(x)
                    scales::scientific(x, digits = 1),
                expand = c(0, 0)
            )
        
        x <- p1_right_prescriptions
    }
    
    if (panel == "both") {
        x <- p1_left_mme + p1_right_prescriptions + plot_layout(ncol = 2)
    }
    
    return(x)
}
