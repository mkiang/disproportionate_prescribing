lorenz_title <- "Prescribing patterns within a single year"
lorenz_sub   <- "About this app"
lorenz_desc_mini  <- HTML(
    "This is a companion app to explore the results of our paper. This section
    shows focuses on the prescription patterns within a single year. Below, we
    plot the empirical cumulative distribution function of prescriptions by 
    ranked providers. The x-axis is the cumulative percentile of providers, 
    ranked by the volume of drugs prescribed and the y-axis is the total
    proportion prescribed for each drug. 
    
    <p>Due to server limitations, <b>the online version of this app does <u>not</u> 
    have all comparison drugs</b>. To view all comparison drugs, please use the
    local version of this app. See the Github repo for more information. 
    
    <p> These empirical distributions can be
    influenced by (1) extreme outliers at the high end of the distribution or
    (2) a large number of low-prescribing providers. We test the robustness
    of our results by removing the top prescribing providers or by removing
    providers with fewer than a certain number of prescriptions per year. 
    Note that we do not have the whole prescribing history of every provider, 
    thus a provider may prescribe many more prescriptions than what we observe. 
    
    <p>Note that in compliance with our data use agreement, we remove all
    observations based on fewer than ten patients or ten providers. Thus, when
    subsetting to small specialties or states in earlier years, some 
    observations may be missing."
)

lorenz_panel_mini <- function() {
    fluidPage(
        titlePanel(lorenz_title),
        
        ## Top row
        fluidRow(info_panel(),
                 column(
                     width = 8,
                     h4(lorenz_sub),
                     p(lorenz_desc_mini),
                     more_info
                 )),
        hr(),
        ## How to subset the data ----
        fluidRow(
            column(
                width = 4,
                year_slider("lorenz_panel_year"),
                ndc_picker_sub("lorenz_panel_ndcs", list(`max-options` = 4), 
                           selected_x = c("opioids", "benzos")
                )
            ), 
            column(
                width = 4,
                meas_checkboxes("lorenz_panel_meas",
                                selected_x = c("mme", "n_prescriptions"))
            ),
            column(
                width = 4,
                prov_state_specialty_selector("lorenz_panel_ss"),
                trim_selector("lorenz_panel_w"),
                submitButton(
                    text = "Submit",
                    icon = NULL,
                    width = NULL
                )
            )
        ),
        hr(), 
        
        ## Plotting section ----
        fluidRow(column(
            width = 12,
            offset = 0,
            align = "center",
            plotOutput("lorenz_one_year", height = "500px",
                       width = "auto")
        )), 
        hr(),
        # br(),
        # 
        # # Table row ----
        # fluidRow(
        #     column(
        #         width = 10,
        #         offset = 1, 
        #         align = "center",
        #         # h3("Mortality Rate and APC Table"),
        #         DT::dataTableOutput("lorenz_panel_table")
        #     )
        # ),
        # hr(), 
        ## Footer ----
        fluidRow(column(
            width = 12,
            align = 'center',
            footer_tag,
            br()
        ))
    )
}
