## 25_supp_tabulate_members_by_age_sex.R ----
## 
## Supplemental analysis to understand the age/sex distribution of Optum 
## members compared to the US general population.

## Imports ----
library(tidyverse)
library(foreach)
library(fs)
library(here)
source(here::here("code", "utils.R"))

## Download member file if we don't have it ----
if (!fs::file_exists(here::here("data_private", "zip5_mbr.csv"))) {
    googleAuthR::gar_gce_auth()
    mbr_objs <- return_bucket_objects(f_regex = "zip5_mbr.csv")
    
    f <- mbr_objs$name[1]
    bucket <- mbr_objs$bucket[1]
    
    f_priv <- here::here("data_private", "zip5_mbr.csv")
    
    googleCloudStorageR::gcs_get_object(
        object_name = f,
        bucket = bucket,
        saveToDisk = f_priv,
        overwrite = FALSE
    )
}

## Tabulate members by age and sex ----
if (!fs::file_exists(here::here("data", "descriptives", 
                                "mbr_age_sex_count.csv"))) {
    c_only <- readr::cols_only(
        Patid = "c",
        Gdr_Cd = "c",
        Yrdob = "c",
        Eligeff = "c"
    )
    
    member_df <- readr::read_csv(here::here("data_private", "zip5_mbr.csv"), 
                          col_types = c_only)
    
    member_df <- member_df %>%
        dplyr::transmute(
            patid = Patid,
            female = dplyr::case_when(Gdr_Cd == "M" ~ 0,
                                      Gdr_Cd == "F" ~ 1), 
            dob_yr = as.integer(Yrdob),
            enroll_yr = lubridate::year(lubridate::ymd(Eligeff))
        )
    
    ## Remove duplicated patients (e.g., enrolled, dropped, enrolled)
    member_df <- member_df[!duplicated(member_df$patid), ]
    
    ## Calculate age at first enrollment
    member_df <- member_df %>% 
        dplyr::mutate(age = enroll_yr - dob_yr) %>% 
        dplyr::mutate(
            age_grp = dplyr::case_when(
                dplyr::between(age,  0, 17) ~ 0,
                dplyr::between(age, 18, 24) ~ 18,
                dplyr::between(age, 25, 34) ~ 25,
                dplyr::between(age, 35, 44) ~ 35,
                dplyr::between(age, 45, 54) ~ 45,
                dplyr::between(age, 55, 64) ~ 55,
                dplyr::between(age, 65, 120) ~ 65
            )
        ) %>% 
        dplyr::filter(!is.na(female), 
                      !is.na(age_grp))
    
    # > member_df %>% filter(female == 1) %>% pull(age) %>% summary()
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    # 0.0    19.0    34.0    35.7    52.0    89.0 
    # > member_df %>% filter(female == 0) %>% pull(age) %>% summary()
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    # 0.00   18.00   33.00   34.16   49.00   89.00 
    
    # > summary(member_df$age)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    # 0.00   18.00   34.00   34.93   51.00   89.00 
    
    # > mean(member_df$female)
    # [1] 0.5051447
    
    age_sex_summary <- member_df %>%
        dplyr::count(female, age_grp)
    
    readr::write_csv(age_sex_summary, 
              here::here("data", "descriptives", "mbr_age_sex_count.csv"))
}
