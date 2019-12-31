topP_title <- "Prescribing patterns over time"
topP_sub   <- "About this app"
topP_desc  <- HTML(
    "This is a companion app to explore the results of our paper. This section
    shows the prescribing patterns of the top providers from 2003 to 2017 for
    different drugs and measures. 
    
    <p> The distribution of prescriptions can be
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

topP_panel <- function() {
    fluidPage(
        titlePanel(topP_title),
        
        ## Top row
        fluidRow(
            info_panel(), 
            column(
                width = 8,
                h4(topP_sub),
                p(topP_desc),
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
                # ineq_selector("topP_ineq"), 
                meas_checkboxes("topP_panel_meas", 
                                selected_x = c("mme", "n_prescriptions"))
            ),
            column(
                width = 4, 
                p1_selector("topP_p1"), 
                trim_selector("topP_panel_w"), 
                submitButton(
                    text = "Submit",
                    icon = NULL,
                    width = NULL
                )
            )
        ),
        hr(), 
        h3(textOutput("topP_trend_title"), align = "center"),

        ## Plotting section ----
        fluidRow(column(
            width = 10,
            offset = 1,
            # h3("TO DO"),
            align = "center",
            plotOutput("topP_trend", width = "auto")
        )),
        hr(),

        # # Table row ----
        # fluidRow(
        #     column(
        #         width = 10,
        #         offset = 1,
        #         align = "center",
        #         DT::dataTableOutput("topP_table")
        #     )
        # ),
        # hr(),
        
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
