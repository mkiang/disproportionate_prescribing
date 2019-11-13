library(shiny)
library(DT)
R.utils::sourceDirectory(here::here("code", "plotting_utils"),
                         modifiedOnly = FALSE)
R.utils::sourceDirectory(here::here("code"), modifiedOnly = FALSE)

## Import data ----
prov_cats <- readRDS(here::here("data", "provider_categories_collapsed.RDS"))

overlap_df <- readRDS(here("data", "overlap_all.RDS"))
overlap_df <- overlap_df %>%
    categorize_w() %>%
    categorize_ndc() %>%
    categorize_prov_state() %>%
    categorize_prov_cat() %>%
    mutate(type_cat = factor(
        overlap_object,
        levels = c("dea_npi", "patid", "pair_id"),
        labels = c("Providers", "Patients", "Provider-Patient Pairs"),
        ordered = TRUE
    ))

## Data and munging ----
transactions_df <-
    readr::read_csv(here::here("data", "ntile_to_ntile_transactions.csv")) %>%
    mutate(
        dea_ntile = case_when(
            dea_ntile == 1 ~ 1,
            between(dea_ntile, 2, 10) ~ 2,
            between(dea_ntile, 11, 100) ~ 3
        ),
        patid_ntile = case_when(
            patid_ntile == 1 ~ 1,
            between(patid_ntile, 2, 10) ~ 2,
            between(patid_ntile, 11, 100) ~ 3
        )
    ) %>%
    group_by(year,
             ndc_type,
             prov_cat,
             prov_state,
             dea_ntile,
             patid_ntile) %>%
    summarize_all(sum) %>%
    ungroup() %>% 
    categorize_ndc() %>%
    categorize_prov_cat() %>%
    categorize_prov_state() %>%
    mutate(
        dea_ntile_cat = factor(
            dea_ntile,
            levels = 1:3,
            labels = c("1st", "2nd-10th", "Bottom 90"),
            ordered = TRUE
        ),
        patid_ntile_cat = factor(
            patid_ntile,
            levels = 1:3,
            labels = c("1st", "2nd-10th", "Bottom 90"),
            ordered = TRUE
        )
    )

# Define server logic required to draw a histogram ----
shinyServer(function(input, output) {
    ## Figure 4 ----
    output$centcat_plot <- renderCachedPlot({
        plot_top_centile_specialties(
            prov_cats %>%
                filter(prop > .005),
            min_year = 2008,
            w_x = as.numeric(input$centcat_panel_w),
            ndc_type_x = input$centcat_panel_ndcs,
            label_ends = TRUE
        ) +
            facet_grid(~ ntile_cat)
    },
    cacheKeyExpr = list(
        input$centcat_panel_ndcs,
        input$centcat_panel_w
    ),
    res = round(72 * 1.25)
    )
    
    output$centcat_table <- DT::renderDataTable({
        sub_cats <- prov_cats %>%
            filter(year >= 2008,
                   w == as.numeric(input$centcat_panel_w)) %>%
            mutate(desc_big_label = case_when(
                is.na(prop) ~ "< 0.005", 
                prop < .005 ~ "< 0.005", 
                TRUE ~ as.character(round(prop, 2))
            ))
        
        sub_cats %>% 
            select(year, ndc_cat, w_cat, desc_big_cat, prop) %>%
            DT::datatable(
                .,
                rownames = FALSE,
                colnames = c(
                    "Year",
                    "Drug",
                    "Trimming",
                    "Provider Description",
                    "Proportion"
                ),
                filter = list(
                    position = 'top',
                    clear = TRUE,
                    plain = FALSE
                )
            ) %>%
            DT::formatRound(., 
                            columns = c("prop"),
                            digits = 3)
    })
    
    ## Figure 4 ----
    output$transactions_plot <- renderCachedPlot({
        plot_ntile_transactions(
            transactions_df,
            year_x = as.numeric(input$transactions_year),
            ndc_x = input$transactions_ndcs
        )
    },
    cacheKeyExpr = list(
        input$transactions_year,
        input$transactions_ndcs
    ),
    res = round(72 * 1.5))
    
    output$transactions_table <- renderDataTable({
        transactions_df %>%
            filter(year == as.numeric(input$transactions_year), 
                   ndc_type == input$transactions_ndcs) %>%
            select(prov_state,
                   ndc_cat,
                   year,
                   dea_ntile_cat,
                   patid_ntile_cat,
                   mme,
                   prescriptions) %>%
            DT::datatable(
                rownames = FALSE,
                colnames = c(
                    "State",
                    "Drug",
                    "Year",
                    "Provider centile",
                    "Patient centile",
                    "Dose",
                    "Prescriptions"
                )
            )
    })
    
    output$overlap_plot <- renderCachedPlot({
        plot_ntile_overlap(
            overlap_df,
            prov_state_x = "US",
            prov_cat_x = "all_types",
            w_x = as.numeric(input$overlap_w),
            ndc_x = input$overlap_ndcs,
            jaccard = input$overlap_jaccard
        )
    },
    cacheKeyExpr = list(
        input$overlap_w,
        input$overlap_jaccard, 
        input$overlap_ndcs
    ),
    res = round(72 * 1.2))
})
