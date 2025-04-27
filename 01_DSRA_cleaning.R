rm(list=ls())
# load up our packages

library(cleaningtools)
library(tidyverse)
library(readxl)
library(openxlsx)
library(ImpactFunctions)

## get the timestamp to more easily keep track of different versions
date_time_now <- format(Sys.time(), "%b_%d_%Y_%H%M%S")

raw_kobo <- ImpactFunctions::get_kobo_data(asset_id = "aBfWuR6hn3cdMJDLRyTVKD", un = "abdirahmanaia")


raw_kobo_data <- raw_kobo %>%
  pluck("main")

raw_kobo_roster <- raw_kobo %>%
  pluck("hh_roster")

raw_kobo_roster %>%
  write_csv(., "03_output/raw_data/raw_roster_output.csv")

site_data <- read_csv("04_tool/239_site_lookup.csv")

version_count <- n_distinct(raw_kobo_data$`__version__`)
if (version_count > 1) {
  print("There are multiple versions of the tool in use")
}

## manual recoding due to enum error

raw_kobo_data <- raw_kobo_data %>% 
  mutate(village = if_else(village == "Kismaayo" & enum_name == "27407" & today == "2025-04-20", "Luglaaw", village)) %>% 
  mutate(idp_code = ifelse(idp_code == "CCCM-SO2401-0415", "CCCM-SO2401-0416", idp_code))



###renaming uuid
raw_kobo_data<- raw_kobo_data %>%
  dplyr::rename(survey_uuid = uuid,
                uuid =`_uuid`) %>%
  mutate(idp_hc_code = ifelse(is.na(idp_code), village, idp_code)) %>%
  left_join(site_data, by = "idp_code") %>%
  relocate(site_name, .after = idp_code) %>%
  relocate(idp_hc_code, .before = idp_code) %>% 
  mutate(site_name = ifelse(is.na(site_name), village, site_name))


raw_kobo_data %>%
  write_csv(., "03_output/raw_data/raw_kobo_output.csv")


###getting hh size from roster
roster_count<-raw_kobo_roster %>% 
  group_by(parent_instance_name) %>% 
  dplyr::summarise(hh_size_roster=n())

###left_join to main data set
data_in_processing <-raw_kobo_data %>% left_join(roster_count,by= join_by("instance_name" == "parent_instance_name"))


kobo_tool_name <- "02_input/DSRA_II_Tool.xlsx"

kobo_tool_name <- "../../02_Data_Collection/02_Tool/REACH_2025_SOM_DSRA_II_Survey_Tool.xlsx"


# read in the survey questions / choices
kobo_survey <- read_excel(kobo_tool_name, sheet = "survey")
kobo_choice <- read_excel(kobo_tool_name, sheet = "choices")

# read in the FO/district mapping
fo_district_mapping <- read_excel("02_input/fo_base_assignment_DSRA_II.xlsx") %>%
  select(district = district_p_code, fo_in_charge = fo_in_charge_for_code)

# # join the fo to the dataset
 data_in_processing <- data_in_processing %>%
   left_join(fo_district_mapping, by = "district")
 
# set the minimum and maximum "reasonable" survey times. Anything outside of this will be flagged
####Calculate time
uuid <- "uuid"
mindur <- 15
mindur_flag <- 20
maxdur <- 80
maxdur_flag <- 80

# Survey time check function

kobo_settings_output <- robotoolbox::kobo_settings()

kobo_data_metadata <- get_kobo_metadata(dataset = data_in_processing, un = "abdirahmanaia", asset_id = "aBfWuR6hn3cdMJDLRyTVKD")

data_in_processing <- kobo_data_metadata$df_and_duration

raw_metadata_length <- kobo_data_metadata$audit_files_length

write_rds(raw_metadata_length, "03_output/raw_data/raw_metadata.rds")



data_in_processing <- data_in_processing %>%
  mutate(length_valid = case_when(
    interview_duration < mindur ~ "Too short",
    interview_duration > maxdur ~ "Too long",
    TRUE ~ "Okay"
  ))


## produce an output for tracking how many surveys are being deleted
data_in_processing %>%
  count(fo_in_charge, length_valid) %>%
  pivot_wider(names_from = length_valid, values_from = n) %>%
  mutate(Okay = replace_na(Okay, 0),
         `Too long` = replace_na(`Too long`, 0),
         `Too short` = replace_na(`Too short`, 0),
         total = Okay + `Too long` + `Too short`) %>%
  writexl::write_xlsx(., paste0('03_output/time_checks/time_check.xlsx'))


################update the above code and added site name and enumerator code to facilitate FO to identify easily.(date changes occured/10/04/25)
data_in_processing %>%
  filter(length_valid != "Okay") %>% 
  select(uuid, site_name, enum_name, interview_duration) %>%
  mutate(comment = 'Interview length too short or too long') %>%
  writexl::write_xlsx(., paste0("03_output/deletion_log/deletion_log.xlsx"))

## filter only valid surveys and for the specific date
data_in_processing <- data_in_processing %>%
  filter(length_valid == "Okay") %>%
  filter(today == "2025-04-26")

# ## Create GPS file

gps <- data_in_processing %>% 
  select(uuid, contains("region"), contains("district"), contains("idp_hc"), contains("gps"), fo_in_charge,enum_name) %>%
  select(-district_origin, -region_origin)
writexl::write_xlsx(gps,paste0("03_output/gps/gps_check_",today(),".xlsx"))


## Specify logical checks

## start by adding variables to see if the edu score given in the roster is bigger than the highest edu question

data_in_processing <- data_in_processing %>%
  mutate(highest_edu_numeric = case_when(
    highest_hh_education_level == "preschool_(kindergarten)" ~ 0,
    highest_hh_education_level == "primary_education_Level_I" ~ 1,
    highest_hh_education_level == "secondary_completed_Level_II" ~ 3,
    highest_hh_education_level == "secondary_completed_Level_III" ~ 3,
    highest_hh_education_level == "technical_vocational_qualification" ~ 3,
    highest_hh_education_level == "undergraduate_completed" ~ 4,
    highest_hh_education_level == "post-graduate_completed" ~ 5,
    highest_hh_education_level == "phD" ~ 6,
    TRUE ~ 0)) 


roster_school_data <- raw_kobo_roster %>% 
  mutate(school_attend_numeric = case_when(
    school_attend == "yes_ttendingprimar_school" ~ 1,
    school_attend == "yes_attendingsecondary_school" ~ 3,
    school_attend == "yes_attending_professional_training" ~ 3,
    TRUE ~ 0
  )) %>%
  group_by(parent_instance_name) %>%
  slice_max(school_attend_numeric, n = 1, with_ties = FALSE) %>%
  select(instance_name = parent_instance_name, roster_max_school_attend = school_attend, roster_max_school_attend_numeric = school_attend_numeric)

data_in_processing <- data_in_processing %>%
  left_join(roster_school_data) %>% 
  mutate(edu_roster_flag = ifelse(roster_max_school_attend_numeric > highest_edu_numeric, TRUE, FALSE))

## logical checks
{
check_list<-data.frame(name=c("healthcare_coverage is yes and no obstacles",
                              "securit_considerations in main_cause_displacement and no  relative_safety in main_reasons ",
                              "main_cause_displacement&obstacles_access_hcs_xx",
                              "main_cause_displacement&obstacles_access_hcs_yy",
                              "main_cause_displacement&obstacles_access_hcs_zz",
                              "main_cause_displacement&obstacles_access_hcs_vv",
                              "main_cause_displacement&obstacles_access_hcs_mm",
                              "main_cause_displacement&obstacles_access_hcs_oo",
                              "hh_size and roster size",
                              "HH_roster_edu_vs_highest_edu_level"),
                       check_to_perform =c("grepl(\"*no*\", healthcare_coverage) & grepl(\"*no_issues*\", obstacles_access_hcs)",
                                           "grepl(\"*securit_considerations*\", main_cause_displacement) &!grepl(\"*relative_safety*\", main_reasons)",
                                           "grepl(\"*economic_migration*\", main_cause_displacement) &!grepl(\"*economic_migration_current_settlement_name*\", main_reasons)",
                                           "grepl(\"*bad_standards_living*\", main_cause_displacement) &!grepl(\"*better_standard_living*\", main_reasons)",
                                           "grepl(\"*discrimination*\", main_cause_displacement) &!grepl(\"*feeling_community*\", main_reasons)",
                                           "grepl(\"*lack_humanitarian_aid*\", main_cause_displacement) &!grepl(\"*availability_humanitarian_assistance*\", main_reasons)",
                                           "grepl(\"*far_friend_family*\", main_cause_displacement) &!grepl(\"*location_friends_family*\", main_reasons)",
                                           "grepl(\"*other*\", main_cause_displacement) &!grepl(\"*other*\", main_reasons)",
                                         "hh_size!=hh_size_roster",
                                         "edu_roster_flag == TRUE"),
                       columns_to_clean = c(
                                            ### include all of the potential binaries from obstacles_access_hcs, which may need updating
                                            "healthcare_coverage,
                                            obstacles_access_hcs/no_issues,
                                            obstacles_access_hcs/unable_access_medical,
                                            obstacles_access_hcs/cost_services_medicine,
                                            obstacles_access_hcs/no_access_qualifi_health_acility,
                                            obstacles_access_hcs/problem_civil_documents,
                                            obstacles_access_hcs/public_health_clinic,
                                            obstacles_access_hcs/public_healthclinic_notopen,
                                            obstacles_access_hcs/treatment_faraway,
                                            obstacles_access_hcs/medical_staff_refuse_treatment_groups,
                                            obstacles_access_hcs/medical_staff_refuse, 
                                            obstacles_access_hcs/medica_staff_disrespectful_rude,	
                                            obstacles_access_hcs/no_medicine_pharmacy, 
                                            obstacles_access_hcs/no_treatment_health_facility, 
                                            obstacles_access_hcs/health_servicesnot_accessible, 
                                            obstacles_access_hcs/fear_harassment_violence_healthcare_servic, 
                                            obstacles_access_hcs/barriers_languag_health_services",#
                                            
                                            
                                            
                                            "main_cause_displacement/securit_considerations, main_reasons/relative_safety",
                                            "main_cause_displacement/economic_migration, main_reasons/economic_migration_current_settlement_name",
                                            "main_cause_displacement/bad_standards_living, main_reasons/better_standard_living",
                                            "main_cause_displacement/discrimination ,main_reasons/feeling_community",
                                            "main_cause_displacement/lack_humanitarian_aid,main_reasons/availability_humanitarian_assistance",
                                            "main_cause_displacement/far_friend_family,main_reasons/location_friends_family",
                                            "main_cause_displacement/other,main_reasons/other",
                                            
                                            "hh_size,hh_size_roster",
                                            
                                            "highest_hh_education_level"),
                       description =c( "All members of the HH doesnt have access to Healtcare but no obstacles in accessing healthcare",
                                       "lack of security is a reason for leaving a site yet relative safety is not selected in reasons for coming to site",
                                       "economic_migration is a reason for leaving a site yet Economic migration is not selected in reasons for coming to site",
                                       "bad_standards_living is a reason for leaving a site yet better_standard_living is not selected in reasons for coming to site",
                                       "discrimination is a reason for leaving a site yet feeling_community is not selected in reasons for coming to site",
                                       "lack_humanitarian_aid is a reason for leaving a site yet availability_humanitarian_assistance is not selected in reasons for coming to site",
                                       "far_friend_family is a reason for leaving a site yet location_friends_family is not selected in reasons for coming to site",
                                       "other is a reason for leaving a site yet other is not selected in reasons for coming to site",
                                       "hh_size is different  from the number of people in the HH roster",
                                       "The highest level of education provided in the roster is higher than the highest level of education given for the household. See column 'roster_max_school_attend', and change the answer to be less than this")
)
}

##group data by FO
group_by_fo <- data_in_processing %>%
  dplyr::group_by(fo_in_charge)


output <- group_by_fo %>%
  dplyr::group_split() %>%
  purrr::map( ~cleaningtools::check_others(
  dataset = .,
  uuid_column = "uuid",
  columns_to_check = names(data_in_processing|>
                             dplyr::select(ends_with("_other")) |>
                             dplyr::select(-contains(".")))) %>% 
  check_duration(column_to_check ="interview_duration",
                 uuid_column ="uuid",
                 log_name ="duration_log",
                 lower_bound = mindur_flag,
                 higher_bound = maxdur_flag) %>%
  cleaningtools::check_logical_with_list(.,
                                         uuid_column = "uuid",
                                         list_of_check = check_list,
                                         check_id_column = "name",
                                         check_to_perform_column = "check_to_perform",
                                         columns_to_clean_column = "columns_to_clean",
                                         description_column = "description"))



cleaning_log <- output %>%
  purrr::map(~ .[] %>%
               create_combined_log() %>% 
               add_info_to_cleaning_log(
                 dataset = "checked_dataset",
                 cleaning_log = "cleaning_log",
                 information_to_add = c("roster_max_school_attend", "idp_hc_code", "site_name", "localisation_region_label", "localisation_district_label", "enum_name", "fo_in_charge")
               )
  )

# write an excel file summarizing the the data issues found. this will include 3 tabs

# checked_dataset: the original dataset with 5 additional check columns (two for duration, one each for the 3 logic checks above)
# cleaning_log: all of the issues spotted with the original value, question, uuid, and issue. Also some pretty colors
# readme: explanations of different actions we could take to remedy the data issues found

# write to each FO's cleaning log folder



cleaning_log %>% 
  purrr::map(~ create_xlsx_cleaning_log(.[], 
                                                       cleaning_log_name = "cleaning_log",
                                                       change_type_col = "change_type",
                                                       column_for_color = "check_binding",
                                                       header_front_size = 10,
                                                       header_front_color = "#FFFFFF",
                                                       header_fill_color = "#ee5859",
                                                       header_front = "Calibri",
                                                       body_front = "Calibri",
                                                       body_front_size = 10,
                                                       use_dropdown = T,
                                                       sm_dropdown_type = "numerical",
                                                       kobo_survey = kobo_survey,
                                                       kobo_choices = kobo_choice,
                                                       output_path = paste0("01_cleaning_logs/",
                                                                            unique(.[]$checked_dataset$fo_in_charge),
                                                                            "/",
                                                                            "cleaning_log_",
                                                                            unique(.[]$checked_dataset$fo_in_charge),
                                                                            "_",
                                                                            date_time_now,
                                                                            ".xlsx")))














