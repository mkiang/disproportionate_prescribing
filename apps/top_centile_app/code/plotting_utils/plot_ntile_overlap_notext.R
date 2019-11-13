library(ggplot2)
library(here)
source(here("code", "mk_nytimes.R"))

plot_ntile_overlap_notext <- function(overlap_df,
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
            filter(year_i < year_j) %>%
            mutate(jaccard_cut = cut(
                jaccard_ij,
                breaks = seq(0, 75, 5) / 100,
                paste0(c(0, seq(6, 75, 5)) / 100, "-", seq(5, 75, 5) / 100)
            ))
        p1 <-
            ggplot(sub_df, aes(
                x = year_j,
                y = year_i,
                fill = overlap_ij
            ))
    } else {
        sub_df <- sub_df %>%
            filter(year_i > year_j) %>%
            mutate(overlap_cut = cut(
                overlap_ij,
                breaks = seq(0, 75, 5) / 100,
                paste0(c(0, seq(6, 75, 5)) / 100, "-", seq(5, 75, 5) / 100)
            ))
        legend_label <- "Proportion from Year i in Year j"
        p1 <- ggplot(sub_df, aes(
            x = year_j,
            y = year_i,
            fill = overlap_ij
        ))
    }
    p1 <- p1 +
        geom_tile(color = "white") +
        scale_fill_viridis_c(legend_label,
                             guide = guide_colorbar(
                                 title.position = "top",
                                 barwidth = unit(10, "cm"),
                                 barheight = unit(.5, "cm")
                             ), 
                             breaks = c(0, .25, .5, .75), 
                             limits = c(0, .75)) +
        coord_equal() +
        scale_x_continuous("Year j",
                           expand = c(0, 0),
                           breaks = c(2008, 2010, 2012, 2014, 2016)) +
        scale_y_continuous("Year i",
                           expand = c(0, 0),
                           breaks = c(2009, 2011, 2013, 2015, 2017)) +
        facet_wrap( ~ type_cat, ncol = 3) +
        mk_nytimes(legend.position = "bottom",
                   panel.grid.major = element_blank())
    return(p1)
}
