library(shiny)
R.utils::sourceDirectory(here::here("code", "plotting_utils"),
                         modifiedOnly = FALSE)
R.utils::sourceDirectory(here::here("code"), modifiedOnly = FALSE)

## Import data ----
top_lorenz_df <-
    readRDS(here::here("data", "lorenz_top_p_all.RDS")) %>% 
    categorize_ndc() %>% 
    categorize_prov_cat() %>% 
    categorize_prov_state() %>% 
    categorize_w() %>% 
    make_p1_labels() %>% 
    categorize_meas() %>% 
    make_Lp1_labels()

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    ## Figure 1 ----
    output$topP_trend_title <- renderText(sprintf(
        "Prescribing patterns among the top %i%% of providers",
        round(as.numeric(input$topP_p1) * 100)
    ))
    
    output$topP_trend <- renderCachedPlot({
        plot_lorenz_top_p(
            top_lorenz_df,
            state_x = parse_state(input$topP_panel_ss),
            cat_x = parse_specialty(input$topP_panel_ss),
            w_x = as.numeric(input$topP_panel_w),
            meas_x = input$topP_panel_meas,
            ndc_x = input$topP_panel_ndcs,
            by_x = "meas", 
            p1_x = input$topP_p1
        )
    },
    cacheKeyExpr = list(
        input$topP_panel_ndcs,
        input$topP_panel_ss,
        input$topP_panel_w,
        input$topP_panel_meas,
        p1_x = input$topP_p1
    ), 
    res = round(72 * 1.5)
    )
    
    output$topP_table <- DT::renderDataTable({
        make_table_lorenz_top_p(
            top_lorenz_df,
            state_x = parse_state(input$topP_panel_ss),
            cat_x = parse_specialty(input$topP_panel_ss),
            w_x = as.numeric(input$topP_panel_w),
            meas_x = input$topP_panel_meas,
            ndc_x = input$topP_panel_ndcs,
            p1_x = input$topP_p1
        )
    })
})
