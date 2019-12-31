gini_title <- "Global inequality over time"
gini_sub   <- "About this app"
gini_desc  <- HTML(
    "This is a companion app to explore the results of our paper. This section
    shows different global measures of inequality across all providers
    from 2003 to 2017 for different drugs and measures. 
    
    <p> The inequality indices can be
    influenced by (1) extreme outliers at the high end of the distribution or
    (2) a large number of low-prescribing providers. We test the robustness
    of our results by removing the top prescribing providers or by removing
    providers with fewer than a certain number of prescriptions per year. 
    Note that we do not have the whole prescribing history of every provider, 
    thus a provider may prescribe many more prescriptions than what we observe. 
    
    <p>Note that in compliance with our data use agreement, we remove all
    observations based on fewer than ten patients or ten providers. Thus, when
    subsetting to small specialties or states in earlier years, some 
    observations may be missing.")


gini_panel <- function() {
    fluidPage(
        titlePanel(gini_title),
        
        ## Top row
        fluidRow(
            info_panel(), 
            column(
                width = 8,
                h4(gini_sub),
                p(gini_desc),
                more_info
            )
        ),
        hr(),
        ## How to subset the data ----
        fluidRow(
            column(
                width = 4,
                ndc_picker("topP_panel_ndcs"),
                prov_state_specialty_selector("topP_panel_ss")
            ),
            column(
                width = 4,
                ineq_selector("topP_ineq"),
                meas_checkboxes("topP_panel_meas", 
                                selected_x = c("mme", "n_prescriptions"))
            ),
            column(
                width = 4, 
                trim_selector("topP_panel_w"), 
                submitButton(
                    text = "Submit",
                    icon = NULL,
                    width = NULL
                )
            )
        ),
        hr(), 
        
        h3(textOutput("topP_ineq_title"), align = "center"),
        ## Plotting section ----
        fluidRow(column(
            width = 10,
            offset = 1,
            # h3("TO DO"),
            align = "center",
            plotOutput("topP_gini", width = "auto")
        )),
        hr(),
        # fluidRow(column(
        #     width = 10,
        #     offset = 1,
        #     align = "center",
        #     DT::dataTableOutput("topP_ineq_table")
        # )),
        # hr(),
        # 
        ## Footer ----
        fluidRow(
            p(), 
            br(), 
            column(
            width = 12,
            align = 'center',
            footer_tag, 
            br(),
            p()
        ))
    )
}
