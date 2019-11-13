library(ggplot2)
library(here)
library(ggrepel)
source(here::here("code", "mk_nytimes.R"))

plot_top_centile_specialties <-
    function(prov_cats,
             min_year = 2008,
             w_x = 0,
             ndc_type_x = "opioids",
             label_ends = FALSE) {
        sub_cats <- prov_cats %>%
            filter(year >= min_year,
                   w == w_x) %>%
            group_by(year, w, desc_big_cat, ndc_type, ntile_cat) %>%
            summarize(freq = sum(freq),
                      prop = sum(prop)) %>%
            ungroup() %>%
            filter(ndc_type == ndc_type_x) %>%
            mutate(desc_big_label =
                       sprintf(
                           "%s (%s)",
                           desc_big_cat,
                           ifelse(prop >= .005,
                                  round(prop, 2),
                                  "< 0.005")
                       ))
        
        p1 <- ggplot(sub_cats,
                     aes(
                         x = year,
                         y = prop,
                         fill = desc_big_cat,
                         color = desc_big_cat
                     ),
                     clip = "off") +
            geom_line(size = .8, alpha = .9) +
            scale_y_continuous(
                "Proportion",
                expand = c(0, 0)
                # limits = c(0, 1)
            ) +
            # scale_color_brewer("Provider Specialty", palette = "Dark2")
            colorspace::scale_color_discrete_qualitative(name = NULL,
                                                         palette = "Dark 2")
        
        if (label_ends) {
            p1 <- p1 +
                geom_point(data = sub_cats %>% filter(year == max(year))) +
                geom_text_repel(
                    data = sub_cats %>% filter(year == max(year)),
                    aes(label = desc_big_label),
                    nudge_x      = 0.2,
                    direction    = "y",
                    hjust        = 0,
                    segment.size = 0
                ) +
                scale_x_continuous(
                    NULL,
                    limits = c(2008, 2023),
                    breaks = c(2008, 2011, 2014, 2017, 2020),
                    expand = c(0, 0)
                ) +
                mk_nytimes(legend.position = "none")
        } else {
            p1 <- p1 +
                scale_x_continuous(
                    NULL,
                    limits = c(2008, 2017),
                    breaks = c(2008, 2011, 2014, 2017),
                    expand = c(0, 0)
                ) +
                mk_nytimes(legend.position = "right")
        }
        return(p1)
    }
