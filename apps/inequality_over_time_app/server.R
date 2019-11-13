library(shiny)
R.utils::sourceDirectory(here::here("code", "plotting_utils"),
                         modifiedOnly = FALSE)
R.utils::sourceDirectory(here::here("code"), modifiedOnly = FALSE)

## Import data ----
ineq_df <- read_csv(here::here("data", "ineq_estimates_all.csv.xz"))
ineq_df <- ineq_df %>%
    categorize_meas() %>%
    categorize_ndc() %>%
    categorize_prov_cat() %>%
    categorize_prov_state() %>%
    categorize_w()

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$topP_ineq_title <- renderText(return_ineq_name(input$topP_ineq))

    output$topP_gini <- renderCachedPlot({
        plot_ineq_over_time(
            ineq_df,
            state_x = parse_state(input$topP_panel_ss),
            cat_x = parse_specialty(input$topP_panel_ss),
            y_lim = c(NA, NA),
            y_expand = c(0, .05),
            w_x = as.numeric(input$topP_panel_w),
            meas_x = input$topP_panel_meas,
            ndc_x = input$topP_panel_ndcs,
            drop_x = TRUE,
            target_col = input$topP_ineq
        )
    },
    cacheKeyExpr = list(
        input$topP_panel_ndcs,
        input$topP_panel_ss,
        input$topP_panel_w,
        input$topP_panel_meas,
        target_col = input$topP_ineq
    ),
    res = round(72 * 1.5)
    )

    output$topP_ineq_table <- renderDataTable({
        make_ineq_table(
            ineq_df,
            state_x = parse_state(input$topP_panel_ss),
            prov_cat_x = parse_specialty(input$topP_panel_ss),
            w_x = as.numeric(input$topP_panel_w),
            meas_x = input$topP_panel_meas,
            ndc_x = input$topP_panel_ndcs
        )
    })
})
