library(shiny)
R.utils::sourceDirectory(here::here("code", "plotting_utils"),
                         modifiedOnly = FALSE)
R.utils::sourceDirectory(here::here("code"), modifiedOnly = FALSE)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    ## Figure 2 ----
    output$lorenz_one_year <- renderCachedPlot({
        ## Lorenz data
        lorenz_df <- import_lorenz_curves(
            state_x = parse_state(input$lorenz_panel_ss),
            ndc_x = input$lorenz_panel_ndcs,
            year_x = as.numeric(input$lorenz_panel_year),
            prov_cat_x = parse_specialty(input$lorenz_panel_ss)
        ) %>%
            categorize_w() %>%
            categorize_ndc() %>%
            categorize_prov_cat() %>%
            categorize_meas() %>%
            categorize_prov_state() %>%
            mirror_lorenz_curve() %>%
            dplyr::group_by(ndc_type, year, prov_state, meas, prov_cat, w) %>%
            flag_landmarks() %>%
            make_Lp1_labels() %>%
            dplyr::ungroup()

        plot_lorenz_one_year(lorenz_df %>% 
                                 filter(w == as.numeric(input$lorenz_panel_w), 
                                        meas %in% input$lorenz_panel_meas),
                             invert_facets = FALSE,
                             text_labels = FALSE)
    },
    cacheKeyExpr = list(
        input$lorenz_panel_year,
        input$lorenz_panel_meas,
        input$lorenz_panel_ndcs,
        input$lorenz_panel_ss,
        input$lorenz_panel_w
    ), 
    res = 72 * 1.5)
    
    output$lorenz_panel_table <- DT::renderDataTable({
        ## Lorenz data
        lorenz_df <- import_lorenz_curves(
            state_x = parse_state(input$lorenz_panel_ss),
            ndc_x = input$lorenz_panel_ndcs,
            year_x = as.numeric(input$lorenz_panel_year),
            prov_cat_x = parse_specialty(input$lorenz_panel_ss)
        ) %>%
            filter(w == as.numeric(input$lorenz_panel_w)) %>% 
            categorize_w() %>%
            categorize_ndc() %>%
            categorize_prov_cat() %>%
            categorize_meas() %>%
            categorize_prov_state() %>%
            mirror_lorenz_curve() %>%
            dplyr::group_by(ndc_type, year, prov_state, meas, prov_cat, w) %>%
            flag_landmarks() %>%
            make_Lp1_labels() %>%
            dplyr::ungroup()

        lorenz_df %>%
            select(year,
                   ndc_cat,
                   meas_cat,
                   specialty_cat,
                   state_cat,
                   w_cat,
                   p1,
                   Lp1) %>%
            arrange(year,
                    ndc_cat,
                    meas_cat,
                    specialty_cat,
                    state_cat,
                    w_cat,
                    p1) %>%
            DT::datatable(
                .,
                rownames = FALSE,
                colnames = c(
                    "Year",
                    "Drug",
                    "Measure",
                    "Provider Specialty",
                    "Provider State",
                    "Trimming",
                    "Proportion of top providers",
                    "Proportion of drugs dispensed"
                ),
                filter = list(
                    position = 'top',
                    clear = TRUE,
                    plain = FALSE
                )
            ) %>%
            DT::formatRound(.,
                            columns = c('p1', 'Lp1'),
                            digits = 4)
    })
})
