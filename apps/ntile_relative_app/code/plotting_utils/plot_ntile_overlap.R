library(ggplot2)
library(here)
source(here("code", "mk_nytimes.R"))

plot_ntile_overlap <- function(overlap_df,
                               prov_state_x = "US",
                               prov_cat_x = "all_types",
                               w_x = 0,
                               ndc_x = "opioids",
                               jaccard = FALSE) {
    sub_df <- overlap_df %>%
        filter(prov_state == prov_state_x,
               prov_cat == prov_cat_x,
               w == w_x,
               ndc_type == ndc_x)
    
    if (jaccard) {
        legend_label <- "Jaccard index"
        sub_df <- sub_df %>%
            filter(year_i < year_j)
        p1 <-
            ggplot(sub_df, aes(
                x = year_j,
                y = year_i,
                fill = jaccard_ij
            ))
    } else {
        legend_label <- "Overlap of Year i in Year j"
        p1 <- ggplot(sub_df, aes(
            x = year_j,
            y = year_i,
            fill = overlap_ij
        ))
    }
    p1 <- p1 +
        geom_tile() +
        scale_fill_viridis_c(legend_label,
                             guide = guide_colorbar(
                                 title.position = "top",
                                 barwidth = unit(10, "cm"),
                                 barheight = unit(.5, "cm")
                             )) +
        coord_equal() +
        scale_x_continuous("Year j",
                           expand = c(0, 0),
                           breaks = c(2008, 2011, 2014, 2017)) +
        scale_y_continuous("Year i",
                           expand = c(0, 0),
                           breaks = c(2008, 2011, 2014, 2017)) +
        facet_wrap(~ type_cat, ncol = 3) +
        mk_nytimes(legend.position = "bottom",
                   panel.grid.major = element_blank()) 
    
    if (jaccard) {
        p1 <- p1 +
            geom_text(aes(label = sprintf(".%02d", round(jaccard_ij * 100)),
                          color = jaccard_ij > .5)) 
    } else {
        p1 <- p1 +
            geom_text(aes(label = sprintf(".%02d", round(overlap_ij * 100)),
                          color = overlap_ij > .5)) 
    }
    p1 + scale_color_manual(values = c("white", "black"), guide = FALSE)
           
}
