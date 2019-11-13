## Copy necessary files over to the shiny apps ----
## 
## We want every shiny app to be able to stand on its own (rather than
## depend on this entire repository to be present). In order to do that, 
## we need to copy certain data and code files multiple times. 

## Imports
library(fs)
library(here)

## Helper functions
dir_delete_if_exists <- function(path) {
    if (fs::dir_exists(path)) {
        fs::dir_delete(path)
    }
}

## Copy inequality metrics data ----
fs::file_copy(
    here::here("data", "ineq_estimates_all.csv.xz"),
    here::here(
        "apps",
        "inequality_over_time_app", 
        "data",
        "ineq_estimates_all.csv.xz"
    ),
    overwrite = TRUE
)

fs::file_copy(
    here::here("data", "gini_with_ci.csv"),
    here::here(
        "apps",
        "inequality_over_time_app", 
        "data",
        "gini_with_ci.csv"
    ),
    overwrite = TRUE
)

## Copy Lorenz data ----
dir_delete_if_exists(here::here("apps", "lorenz_curve_app", "data", "lorenz"))
fs::dir_copy(
    here::here("data", "lorenz"),
    here::here("apps", "lorenz_curve_app", "data", "lorenz")
)
# We have too many files for a Shiny app so we'll remove the comparison
# drugs that are not used in the paper. Note that this is ONLY for the
# online version. I've commented this out because if the app is run
# locally, it will work just fine. 
# fs::dir_delete(fs::dir_ls(
#     here::here("apps", "lorenz_curve_app", "data", "lorenz"),
#     recurse = TRUE,
#     type = "directory",
#     regexp = paste(
#         c(
#             "warfarin",
#             "statins",
#             "metformin",
#             "chlordiazepoxide",
#             "levothyroxine",
#             "citalopram",
#             "temazepam",
#             "triazolam",
#             "oxazepam",
#             "methylphenidate",
#             "alprazolam",
#             "buspirone",
#             "gabapentin"
#         ),
#         collapse = "|"
#     )
# ))

## Copy n-tile summary data ----
dir_delete_if_exists(
    here::here("apps", "ntile_relative_app", "data", "mme_ntiles")
    )
fs::dir_copy(
    here::here("data", "mme_ntiles"),
    here::here("apps", "ntile_relative_app", "data", "mme_ntiles")
)

## Copy prescribing over time data ----
fs::file_copy(
    here::here("data", "lorenz_top_p_all.RDS"),
    here::here("apps",
               "prescribing_over_time_app", 
               "data",
               "lorenz_top_p_all.RDS"),
    overwrite = TRUE
)

## Provider categories ----
fs::file_copy(
    here("data", "provider_categories_collapsed.RDS"),
    here(
        "apps",
        "top_centile_app", 
        "data",
        "provider_categories_collapsed.RDS"
    ),
    overwrite = TRUE
)

## Ntile to Ntile transactions ----
fs::file_copy(
    here("data", "ntile_to_ntile_transactions.csv"),
    here(
        "apps",
        "top_centile_app", 
        "data",
        "ntile_to_ntile_transactions.csv"
    ),
    overwrite = TRUE
)

## Overlap data ----
fs::file_copy(
    here("data", "overlap_all.RDS"),
    here("apps",
         "top_centile_app",
         "data", "overlap_all.RDS"),
    overwrite = TRUE
)
