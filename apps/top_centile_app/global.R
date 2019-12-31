library(shiny)
library(shinyWidgets)
## Globals for the prolific prescribing companion application ----
R.utils::sourceDirectory(here::here("code"), modifiedOnly = FALSE)

## Textual constants ----
## Paper title
paper_title <-
  "Opioid prescribing among United States medical providers, 2003-17: retrospective, observational study"

footer_tag  <- HTML(
  "Created in <a href='https://shiny.rstudio.com/'>Shiny</a> by
      <a href='https://mathewkiang.com'>Mathew Kiang</a>.
      Source code is available on
      <a href='https://github.com/mkiang/disproportionate_prescribing'>this
      paper's
      Github repository</a>."
)

## More info
more_info <- list(
  h4("More information"),
  HTML(
    "For more information, please see our article on the BMJ website (<a href='https://www.doi.org/10.1136/bmj.l6968'>doi: 10.1136/bmj.l6968</a>). More information about this 
        companion app or the other companion apps is available at 
        <a href='https://github.com/mkiang/disproportionate_prescribing/apps'>the 
        repo README page</a>. 
        
        All source code is available at the
          <a href='https://github.com/mkiang/disproportionate_prescribing'>
          associated Github repository</a>. "
  )
)

## Biographical tags ----
mvk_tag <-
  tags$li(a(href = "https://mathewkiang.com",
            "Mathew V Kiang"),
          HTML(paste0(
            "(",
            a(href = "https://twitter.com/mathewkiang",
              "@mathewkiang"),
            ")"
          )))
kh_tag <-
  tags$li(
    a(href = "https://profiles.stanford.edu/keith-humphreys",
      "Keith Humphreys"),
    HTML(paste0(
      "(",
      a(href = "https://twitter.com/KeithNHumphreys",
        "@KeithNHumphreys"),
      ")"
    ))
  )
mc_tag <-
  tags$li(
    a(href = "https://profiles.stanford.edu/mark-cullen",
      "Mark R Cullen"),
    HTML(paste0(
      "(",
      a(href = "https://twitter.com/markcullen_phs",
        "@markcullen_phs"),
      ")"
    ))
  )
sb_tag <-
  tags$li(a(
    href = paste0(
      "https://primarycare.hms.harvard.edu/faculty-staff/sanjay-basu"
    ),
    "Sanjay Basu"
  ))

## Helper functions ----
return_drug_list_mini <- function() {
  return_drug_list()[!(names(return_drug_list()) %in% c(
    "Warfarin",
    "Simvatatin",
    "Metformin",
    "Levothyroxine",
    "Citalopram",
    "Temazepam",
    "Triazolam"
  ))]
}

info_panel <- function() {
  column(width = 4,
         wellPanel(
           tags$blockquote(em(paper_title)),
           tags$ul(mvk_tag, kh_tag, mc_tag, sb_tag)
         ))
}

trim_selector <- function(value_id,
                          selected_x = 0) {
  selectInput(
    value_id,
    label = "Trim Data:",
    choices = list(
      "Untrimmed" = 0,
      "Top 0.1% of providers" = 0.001,
      "Top 0.5% of providers" = 0.005,
      "Top 1.0% of providers" = 0.010,
      "Top 2.5% of providers" = 0.025,
      "Top 5.0% of providers" = 0.050,
      "Top 10% of providers"  = 0.100,
      "<3 perscriptions/year" = 3,
      "<6 prescriptions/year" = 6,
      "<12 prescriptions/year" = 12,
      "<24 prescriptions/year" = 24,
      "<50 prescriptions/year" = 50,
      "<100 prescriptions/year" = 100,
      "<200 prescriptions/year" = 200
    ),
    selected = selected_x
  )
}

p1_selector <- function(value_id,
                        selected_x = "0.01") {
  selectInput(
    value_id,
    label = "Top percentile of providers:",
    choices = list(
      "Top 1%" = "0.01",
      "Top 5%" = "0.05",
      "Top 10%" = "0.10",
      "Top 25%" = "0.25",
      "Top 50%" = "0.50"
    ),
    selected = selected_x
  )
}

meas_selector <-
  function(value_id,
           selected_x = "mme") {
    selectInput(
      value_id,
      label = "Measure:",
      choices = list(
        "Dose" = "mme",
        "Dose per day" = "mme_per_day",
        "Dose per patient" = "mme_per_pt",
        "Dose per prescription" = "mme_per_prescription",
        "Number of prescriptions" = "n_prescriptions"
      ),
      selected = selected_x
    )
  }

meas_checkboxes <-
  function(value_id,
           selected_x = c("mme", "n_prescriptions")) {
    checkboxGroupInput(
      value_id,
      label = "Measure:",
      choices = list(
        "Dose" = "mme",
        "Dose per day" = "mme_per_day",
        "Dose per patient" = "mme_per_pt",
        "Dose per prescription" = "mme_per_prescription",
        "Number of prescriptions" = "n_prescriptions"
      ),
      selected = selected_x
    )
  }

ndc_checkboxes <- function(value_id,
                           selected_x = c("opioids", "benzos")) {
  checkboxGroupInput(value_id,
                     label = "Drug Types:",
                     choices = return_drug_list(),
                     selected = selected_x)
}

jaccard_checkbox <- function(value_id,
                             value_x = FALSE) {
  checkboxInput(value_id,
                label = "Use Jaccard index:",
                value = value_x)
}

ndc_selector <- function(value_id,
                         selected_x = "opioids") {
  selectInput(value_id,
              label = "Drug Types:",
              choices = return_drug_list(),
              selected = selected_x)
}

ndc_selector_sub <- function(value_id,
                             selected_x = "opioids") {
  selectInput(
    value_id,
    label = "Drug Types:",
    choices = list(
      "Opioids" = "opioids",
      "Alprazolam" = "alprazolam",
      "Cyclobenzaprine" = "cyclobenzaprine",
      "Gabapentin" = "gabapentin",
      "Buspirone" = "buspirone",
      "Lorazepam" = "lorazepam",
      "Dextroamphetamine" = "dextroamphetamine",
      "Methylphenidate" = "methlyphenidate",
      "Schedule II opioids" = "schii_opioids"
    ),
    selected = selected_x
  )
}

ndc_picker <- function(value_id,
                       selected_x = c("opioids",
                                      "benzos"),
                       options_x = list(`max-options` = 8)) {
  pickerInput(
    value_id,
    label = "Drug Types:",
    choices = return_drug_list(),
    selected = selected_x,
    multiple = TRUE,
    options = options_x
  )
}

ndc_picker_sub <- function(value_id,
                           selected_x = c(
                             "opioids",
                             "alprazolam",
                             "cyclobenzaprine",
                             "gabapentin",
                             "buspirone",
                             "lorazepam",
                             "dextroamphetamine",
                             "methlyphenidate"
                           ),
                           options_x = list(`max-options` = 8)) {
  pickerInput(
    value_id,
    label = "Drug Types:",
    choices = list(
      "Opioids" = "opioids",
      "Alprazolam" = "alprazolam",
      "Cyclobenzaprine" = "cyclobenzaprine",
      "Gabapentin" = "gabapentin",
      "Buspirone" = "buspirone",
      "Lorazepam" = "lorazepam",
      "Dextroamphetamine" = "dextroamphetamine",
      "Methylphenidate" = "methlyphenidate",
      "Schedule II opioids" = "schii_opioids"
    ),
    selected = selected_x,
    multiple = TRUE,
    options = options_x
  )
}

prov_cat_selector <- function(value_id,
                              selected_x = "all_types") {
  selectInput(
    value_id,
    label = "Specialties",
    choices = list(
      "All" = "all_types",
      "Family medicine" = "family",
      "Internal medicine" = "internal",
      "Pediatrics" = "prev_internal_family",
      "Ob / Gyn" = "obgyn",
      "Emergency medicine" = "emergency",
      "PM and R" = "pmnr",
      "Critical care" = "critical_care",
      "General surgery" = "gen_surgery",
      "Orthopedic surgery" = "ortho",
      "Plastic surgery" = "plastics",
      "All surgery" = "all_surgery",
      "Preventive / Internal / Family" = "prev_internal_family"
    ),
    selected = selected_x
  )
}

prov_state_selector <- function(value_id,
                                selected_x = "US") {
  selectInput(
    inputId = value_id,
    label = "State",
    choices = list(
      "All States" = "US",
      "Alabama" = "AL",
      "Alaska" = "AK",
      "Arizona" = "AZ",
      "Arkansas" = "AR",
      "California" = "CA",
      "Colorado" = "CO",
      "Connecticut" = "CT",
      "Delaware" = "DE",
      "District of Columbia" = "DC",
      "Florida" = "FL",
      "Georgia" = "GA",
      "Hawaii" = "HI",
      "Idaho" = "ID",
      "Illinois" = "IL",
      "Indiana" = "IN",
      "Iowa" = "IA",
      "Kansas" = "KS",
      "Kentucky" = "KY",
      "Louisiana" = "LA",
      "Maine" = "ME",
      "Maryland" = "MD",
      "Massachusetts" = "MA",
      "Michigan" = "MI",
      "Minnesota" = "MN",
      "Mississippi" = "MS",
      "Missouri" = "MO",
      "Montana" = "MT",
      "Nebraska" = "NE",
      "Nevada" = "NV",
      "New Hampshire" = "NH",
      "New Jersey" = "NJ",
      "New Mexico" = "NM",
      "New York" = "NY",
      "North Carolina" = "NC",
      "North Dakota" = "ND",
      "Ohio" = "OH",
      "Oklahoma" = "OK",
      "Oregon" = "OR",
      "Pennsylvania" = "PA",
      "Rhode Island" = "RI",
      "South Carolina" = "SC",
      "South Dakota" = "SD",
      "Tennessee" = "TN",
      "Texas" = "TX",
      "Utah" = "UT",
      "Vermont" = "VT",
      "Virginia" = "VA",
      "Washington" = "WA",
      "West Virginia" = "WV",
      "Wisconsin" = "WI",
      "Wyoming" = "WY"
    ),
    selected = selected_x
  )
}

prov_state_specialty_selector <- function(value_id,
                                          selected_x = "all_types") {
  selectInput(
    inputId = value_id,
    label = "State or specialty:",
    choices = list(
      "US, All specialties" = "all_types",
      "US, Family medicine" = "family",
      "US, Internal medicine" = "internal",
      "US, Pediatrics" = "prev_internal_family",
      "US, Ob / Gyn" = "obgyn",
      "US, Emergency medicine" = "emergency",
      "US, PM and R" = "pmnr",
      "US, Critical care" = "critical_care",
      "US, General surgery" = "gen_surgery",
      "US, Orthopedic surgery" = "ortho",
      "US, Plastic surgery" = "plastics",
      "US, All surgery" = "all_surgery",
      "US, Preventive / Internal / Family" = "prev_internal_family",
      "Alabama, All specialties" = "AL",
      "Alaska, All specialties" = "AK",
      "Arizona, All specialties" = "AZ",
      "Arkansas, All specialties" = "AR",
      "California, All specialties" = "CA",
      "Colorado, All specialties" = "CO",
      "Connecticut, All specialties" = "CT",
      "Delaware, All specialties" = "DE",
      "District of Columbia, All specialties" = "DC",
      "Florida, All specialties" = "FL",
      "Georgia, All specialties" = "GA",
      "Hawaii, All specialties" = "HI",
      "Idaho, All specialties" = "ID",
      "Illinois, All specialties" = "IL",
      "Indiana, All specialties" = "IN",
      "Iowa, All specialties" = "IA",
      "Kansas, All specialties" = "KS",
      "Kentucky, All specialties" = "KY",
      "Louisiana, All specialties" = "LA",
      "Maine, All specialties" = "ME",
      "Maryland, All specialties" = "MD",
      "Massachusetts, All specialties" = "MA",
      "Michigan, All specialties" = "MI",
      "Minnesota, All specialties" = "MN",
      "Mississippi, All specialties" = "MS",
      "Missouri, All specialties" = "MO",
      "Montana, All specialties" = "MT",
      "Nebraska, All specialties" = "NE",
      "Nevada, All specialties" = "NV",
      "New Hampshire, All specialties" = "NH",
      "New Jersey, All specialties" = "NJ",
      "New Mexico, All specialties" = "NM",
      "New York, All specialties" = "NY",
      "North Carolina, All specialties" = "NC",
      "North Dakota, All specialties" = "ND",
      "Ohio, All specialties" = "OH",
      "Oklahoma, All specialties" = "OK",
      "Oregon, All specialties" = "OR",
      "Pennsylvania, All specialties" = "PA",
      "Rhode Island, All specialties" = "RI",
      "South Carolina, All specialties" = "SC",
      "South Dakota, All specialties" = "SD",
      "Tennessee, All specialties" = "TN",
      "Texas, All specialties" = "TX",
      "Utah, All specialties" = "UT",
      "Vermont, All specialties" = "VT",
      "Virginia, All specialties" = "VA",
      "Washington, All specialties" = "WA",
      "West Virginia, All specialties" = "WV",
      "Wisconsin, All specialties" = "WI",
      "Wyoming, All specialties" = "WY"
    ),
    selected = selected_x
  )
}

parse_state_specialty <- function(input_value) {
  if (input_value %in% c(
    "all_types",
    "family",
    "internal",
    "prev_internal_family",
    "obgyn",
    "emergency",
    "pmnr",
    "critical_care",
    "gen_surgery",
    "ortho",
    "plastics",
    "all_surgery",
    "prev_internal_family"
  )) {
    list(prov_state = "US",
         prov_specialty = input_value)
  } else {
    list(prov_state = input_value,
         prov_specialty = "all_types")
  }
}

ineq_selector <-
  function(value_id,
           selected_x = "gini") {
    selectInput(
      value_id,
      label = "Inequality metric:",
      choices = list(
        "Gini coefficient" = "gini",
        "Ricci-Schutz coefficient" = "ricci_schutz",
        "Atkinson index" = "atkinson",
        "Theil entropy" = "theil",
        "Generalized entropy" = "entropy"
      ),
      selected = selected_x
    )
  }

parse_state <- function(input_value) {
  parse_state_specialty(input_value)$prov_state
}

parse_specialty <- function(input_value) {
  parse_state_specialty(input_value)$prov_specialty
}

year_slider <- function(value_id) {
  sliderInput(
    value_id,
    label = "Year",
    min = 2003,
    max = 2017,
    value = 2017,
    step = 1,
    round = TRUE,
    sep = ""
  )
}
