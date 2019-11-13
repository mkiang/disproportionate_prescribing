library(tidyverse)
library(here)
library(knitr)
library(DT)
source(here::here("code", "utils.R"))

make_table_mme_by_state <- function(mme_summary_df,
                                    w_x = 0,
                                    ndc_type_x = "opioids",
                                    prov_cat_x = "all_types",
                                    table_type = "DT") {
    if (ndc_type_x %in% c("opioids", "schii_opioids")) {
        amount_label <- "Amount dispensed / year (MME)"
        per_day_label <- "Total dosage/day / year (MME)"
    } else {
        amount_label <- "Amount dispensed / year (mg)"
        per_day_label <- "Total dosage/day / year (mg)"
    }
    
    mme_per_day_df <- mme_summary_df %>%
        dplyr::filter(w == w_x,
               ndc_type == ndc_type_x,
               prov_cat == prov_cat_x,
               meas == "mme_per_day") %>%
        dplyr::group_by(prov_state) %>%
        dplyr::summarize(avg_mme_per_day = mean(sum))
    
    print_df <- mme_summary_df %>%
        dplyr::filter(w == w_x,
               ndc_type == ndc_type_x,
               prov_cat == prov_cat_x,
               meas == "mme") %>%
        dplyr::group_by(prov_state) %>%
        dplyr::summarize(
            n_years = dplyr::n_distinct(year),
            avg_mme = mean(sum),
            avg_prescriptions = mean(n_prescriptions),
            avg_patid = mean(n_patients),
            avg_deanpi = mean(n_deanpi)
        ) %>%
        dplyr::left_join(mme_per_day_df,
                  by = "prov_state") %>%
        dplyr::left_join(return_st_info() %>%
                      dplyr::select(prov_state = abbrev,
                             st_name = name_cat_alpha)) %>%
        dplyr::select(
            st_name,
            n_years,
            avg_mme,
            avg_mme_per_day,
            avg_prescriptions,
            avg_patid,
            avg_deanpi,
            prov_state
        ) %>%
        dplyr::arrange(st_name) %>%
        dplyr::filter(!is.na(prov_state)) %>%
        dplyr::select(-prov_state)
    
    if (table_type == "kable") {
        print_df %>%
            knitr::kable(
                .,
                format = "html",
                digits = 0,
                format.args = list(big.mark = ','),
                col.names = c(
                    "State",
                    "Num. Years (#)",
                    amount_label,
                    per_day_label,
                    "Prescriptions / year",
                    "Unique patients / year",
                    "Unique providers / year"
                )
            )
    } else if (table_type == "DT") {
        print_df %>%
            DT::datatable(
                colnames = c(
                    "State",
                    "Num. Years (#)",
                    amount_label,
                    per_day_label,
                    "Prescriptions / year",
                    "Unique patients / year",
                    "Unique providers / year"
                )
            ) %>%
            DT::formatRound(2:6, digits = 0, mark = ",")
    } else {
        return(print_df)
    }
}

make_table_lorenz_top_p <- function(lorenz_top_p_df,
                                    w_x = 0,
                                    ndc_x = c("opioids", "statins"),
                                    meas_x = "mme",
                                    cat_x = "all_types",
                                    state_x = "US",
                                    p1_x = c("0.01", "0.05", "0.10")) {
    sub_df <- top_lorenz_df %>%
        dplyr::filter(
            w == w_x,
            ndc_type %in% ndc_x,
            meas %in% meas_x,
            prov_cat == cat_x,
            prov_state == state_x,
            p1_round %in% p1_x
        )
    
    if (NROW(sub_df) == 0) {
        return(NULL)
    } else {
        sub_df %>%
            dplyr::select(year,
                   ndc_cat,
                   meas_cat,
                   specialty_cat,
                   state_cat,
                   w_cat,
                   p1_lab,
                   Lp1_lab) %>%
            dplyr::arrange(meas_cat,
                    ndc_cat,
                    specialty_cat,
                    state_cat,
                    w_cat,
                    p1_lab,
                    year) %>%
            DT::datatable(
                .,
                colnames = c(
                    "Year",
                    "Measure",
                    "Provider Specialty",
                    "Provider State",
                    "Trimming",
                    "Top Providers",
                    "Proportion"
                ),
                filter = list(
                    position = 'top',
                    clear = TRUE,
                    plain = FALSE
                ),
                rownames = FALSE
            )
    }
}

make_ineq_table <- function(ineq_df, 
                            state_x = "US", 
                            prov_cat_x = "all_types",
                            w_x = 0, 
                            meas_x = c("mme", "n_prescriptions"), 
                            ndc_x = c("opioids")) {
    sub_df <- ineq_df %>%
        dplyr::filter(
            prov_state == state_x,
            prov_cat == prov_cat_x,
            ndc_type %in% ndc_x,
            w == w_x,
            meas %in% meas_x
        ) %>%
        dplyr::arrange(meas, year) %>%
        dplyr::select(year,
               ndc_cat,
               meas_cat, 
               n_prescriptions, 
               w_cat, 
               gini:entropy)
    
    sub_df %>%
        DT::datatable(
            .,
            colnames = c(
                "Year",
                "Drug", 
                "Measure",
                "Prescriptions (N)",
                "Trim",
                "Gini",
                "Ricci-Schutz",
                "Atkinson",
                "Theil entropy",
                "Generalized entropy"
            ),
            rownames = FALSE,
            filter = list(
                position = 'top',
                clear = TRUE,
                plain = FALSE
            )
        ) %>%
        DT::formatRound(
            .,
            columns = c("gini",
                        "ricci_schutz",
                        "atkinson",
                        "theil",
                        "entropy")
            ,
            digits = 2
        )
}
