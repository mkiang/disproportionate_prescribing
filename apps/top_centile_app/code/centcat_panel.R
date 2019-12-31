centcat_title <- "Specialties, transactions, and overlap"
centcat_sub   <- "About this app"
centcat_desc  <- HTML(
    "This is a companion app to explore the results of our paper. This section
    shows the medical specialties of the top centile of providers from 2008 to
    2017 for different drugs. For clarity, we remove all specialties with 
    proportions of less than .005 from the figure. They are retained in the
    associated table (unless doing so violates our data use agreement). 
    
    <p> In addition, we show the transcations between provider and patient
    centile groups for each drug and year. For clarity, we divide the centile
    groups into the top centile (1%), centiles 2-10, and the bottom 90
    centiles. 
    
    <p> Lastly, we show the overlap between the top centile providers, 
    patients, and provider-patient pairs over time.
    
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

centcat_panel <- function() {
    fluidPage(
        titlePanel(centcat_title),
        ## Top row
        fluidRow(info_panel(),
                 column(
                     width = 8,
                     h4(centcat_sub),
                     p(centcat_desc),
                     more_info
                 )),
        
        hr(),
        
        ## Plotting section ----
        fluidRow(
            h3("Provider specialties"), 
            ## How to subset the data ----
                column(
                    width = 4,
                    ndc_selector("centcat_panel_ndcs"),
                    trim_selector("centcat_panel_w"),
                    submitButton(
                        text = "Submit",
                        icon = NULL,
                        width = NULL
                    )
                ), 
            column(
                width = 8,
                offset = 0,
                align = "center",
                plotOutput("centcat_plot",
                           width = "auto")
            )
        ),
        hr(),
        # fluidRow(
        #     column(
        #         width = 10,
        #         offset = 1, 
        #         align = "center",
        #         # h3("Mortality Rate and APC Table"),
        #         DT::dataTableOutput("centcat_table")
        #     )
        # ),
        # hr(), 
        ## Plotting section ----
        fluidRow(
            h3("Provider to patient transactions"), 
            ## How to subset the data ----
            column(
                width = 4,
                ndc_selector("transactions_ndcs", selected_x = c("opioids")),
                year_slider("transactions_year"), 
                submitButton(
                    text = "Submit",
                    icon = NULL,
                    width = NULL
                )
            ), 
            column(
                width = 8,
                offset = 0,
                align = "center",
                plotOutput("transactions_plot",
                           width = "auto")
            )
        ),
        hr(),
        # fluidRow(
        #     column(
        #         width = 10,
        #         offset = 1, 
        #         align = "center", 
        #         DT::dataTableOutput("transactions_table")
        #     )
        # ),
        # hr(),
        ## Plotting section ----
        fluidRow(
            h3("Overlap of the top centile over time"),
            ## How to subset the data ----
            column(
                width = 4,
                ndc_selector("overlap_ndcs", selected_x = c("opioids")),
                trim_selector("overlap_w"),
                jaccard_checkbox("overlap_jaccard"),
                submitButton(
                    text = "Submit",
                    icon = NULL,
                    width = NULL
                )
            ),
            column(
                width = 8,
                offset = 0,
                align = "center",
                plotOutput("overlap_plot",
                           width = "auto")
            )
        ),
        # hr(),
        # fluidRow(
        #     column(
        #         width = 10,
        #         offset = 1,
        #         align = "center",
        #         DT::dataTableOutput("transactions_table")
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