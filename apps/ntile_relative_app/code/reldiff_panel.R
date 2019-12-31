reldiff_title <- "Relative prescribing by provider centiles"
reldiff_sub   <- "About this app"
reldiff_desc  <- HTML(
    "This is a companion app to explore the results of our paper. This section
    shows the prescribing patterns of the top 25 centile groups of providers 
    relative to the median (50th) centile group, from 2003 to 2017 for
    different drugs and measures. To form a centile group, we ranked all providers
    based on their prescribing patterns and then created 100 (approximately)
    equally sized groups. 
    
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
    observations may be missing."
)

reldiff_panel <- function() {
    fluidPage(
        titlePanel(reldiff_title),
        
        ## Top row
        fluidRow(info_panel(),
                 column(
                     width = 8,
                     h4(reldiff_sub),
                     p(reldiff_desc),
                     more_info
                 )
                 ),
        
        hr(),
        ## How to subset the data ----
        fluidRow(
            column(
                width = 4,
                ndc_picker("reldiff_panel_ndcs",
                           selected_x = c(
                               "opioids",
                               "benzos"), 
                           options_x = list(`max-options` = 4)),
                prov_state_specialty_selector("reldiff_panel_ss")
            ), 
            column(
                width = 4,
                meas_checkboxes("reldiff_panel_meas")
            ),
            column(
                width = 4,
                trim_selector("reldiff_panel_w"),
                submitButton(
                    text = "Submit",
                    icon = NULL,
                    width = NULL
                )
            )
            ),
        hr(), 
        ## Plotting section ----
        fluidRow(
            column(
                width = 12,
                offset = 0,
                align = "center",
                plotOutput("reldiff_one_year",
                           height = "750px", width = "auto")
            )
        ),
        hr(),
        # # Table row ----
        # fluidRow(
        #     column(
        #         width = 10,
        #         offset = 1, 
        #         align = "center",
        #         # h3("Mortality Rate and APC Table"),
        #         DT::dataTableOutput("reldiff_panel_table")
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