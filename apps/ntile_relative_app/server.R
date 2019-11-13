library(shiny)
R.utils::sourceDirectory(here::here("code", "plotting_utils"),
                         modifiedOnly = FALSE)
R.utils::sourceDirectory(here::here("code"), modifiedOnly = FALSE)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    ## Figure 3 ----
    output$reldiff_one_year <- renderCachedPlot({
        ntile_mme <- import_ntile_mme_file(
            parse_state(input$reldiff_panel_ss),
            use_vroom = TRUE)
        
        sub_df <- ntile_subset(
            ntile_mme,
            w_x = as.numeric(input$reldiff_panel_w),
            ndc_type_x = input$reldiff_panel_ndcs,
            prov_state_x = parse_state(input$reldiff_panel_ss),
            prov_cat_x =  parse_specialty(input$reldiff_panel_ss)
        )
        
        plot_ntile_relative_lines(
            sub_df,
            bottom_ntile = 25,
            comparison_ntile = 50,
            stat_type_x = "mean",
            meas_x = input$reldiff_panel_meas
        )
    },
    cacheKeyExpr = list(
        input$reldiff_panel_meas,
        input$reldiff_panel_ndcs,
        input$reldiff_panel_ss,
        input$reldiff_panel_w
    ), 
    res = round(72 * 1.5))
    
    output$reldiff_panel_table <- DT::renderDataTable({
        ntile_mme <- import_ntile_mme_file(
            parse_state(input$reldiff_panel_ss),
            use_vroom = TRUE)
        
        sub_df <- ntile_subset(
            ntile_mme,
            w_x = as.numeric(input$reldiff_panel_w),
            ndc_type_x = input$reldiff_panel_ndcs,
            prov_state_x = parse_state(input$reldiff_panel_ss),
            prov_cat_x =  parse_specialty(input$reldiff_panel_ss)
        ) %>% 
            filter(top_ntile %in% c(1:25, 50))
        
        sub_df %>%
            select(
                year,
                ndc_cat,
                state_cat,
                specialty_cat,
                w_cat,
                meas_cat,
                top_ntile,
                n_prov,
                value
            ) %>%
            DT::datatable(
                .,
                rownames = FALSE,
                colnames = c(
                    "Year",
                    "Drug",
                    "Provider State",
                    "Provider Specialty",
                    "Trimming",
                    "Measure",
                    "Provider Centile",
                    "Number Providers",
                    "Average Amount"
                ),
                filter = list(
                    position = 'top',
                    clear = TRUE,
                    plain = FALSE
                )
            ) %>%
            DT::formatRound(., 
                            columns = c("value"),
                            digits = 0)
    })
})
