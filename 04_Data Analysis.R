rm(list = ls())

library(tidyverse)
library(cleaningtools)
library(analysistools)
library(presentresults)
library(readxl)

##############################################################################
########################## Load the Data and Survey ##########################
##############################################################################


## tool
kobo_tool_name <- "02_input/DSRA_II_Tool.xlsx"

# read in the survey questions / choices
kobo_survey <- read_excel(kobo_tool_name, sheet = "survey") %>%
  mutate(type = str_squish(type))  %>% 
  mutate(`label::English (en)` = ifelse(`label::English (en)` == "A.4 In which region is the assessment being conducted?" & type == "calculate", "", `label::English (en)`))

kobo_choice <- read_excel(kobo_tool_name, sheet = "choices") %>% 
  filter(list_name != "land_ownership" | list_name == "land_ownership" & is.na(code))

# load datasets for processing
file_path <- "03_output/final_cleaned_data/SOM_DSRA_II_Output.xlsx"
main_data <- read_excel(file_path, 'clean_HH_data', guess_max =  10000) %>%
  mutate(hh_size = as.numeric(hh_size))
clean_roster <- read_excel(file_path, 'clean_roster_data', guess_max =  10000)
raw_data <- read_excel(file_path, 'raw_HH_data', guess_max =  10000)
raw_roster_data <- read_excel(file_path, 'raw_roster_data', guess_max =  10000)


## load cleaning_logs
cleaning_logs <- readxl::read_excel("03_output/combined_cleaning_log/combined_cleaning_log.xlsx")

## load deletion log
deletion_log <- read_excel("03_output/combined_cleaning_log/combined_deletion_log.xlsx") %>% 
  distinct(uuid, .keep_all = T)


## join relevant info 

clean_roster <- clean_roster %>%
  left_join(main_data %>% 
              select(instance_name, idp_hc_code, site_name, localisation_district_label, localisation_region_label),
            by = join_by("parent_instance_name" == "instance_name"))

## load in the LOA

loa <- readxl::read_excel("02_input/analysis_loa_all_data.xlsx")
loa_roster <- readxl::read_excel("02_input/analysis_loa_roster.xlsx")

###################################apply weights##################################################################

hc_hh_size <- main_data %>% 
  filter(idp_host_community == "host_community") %>% 
  group_by(idp_hc_code) %>% 
  summarise(hh_size = mean(hh_roster_count, na.rm = T))

  
sampling_df_idp <- readxl::read_excel('02_input/DSRA_II_Sampling_Info.xlsx', sheet = "IDP") %>%
  select(idp_hc_code = idp_code, site_name, district_name = District, district = district_code, population = HH_size, sample_size, group)

sampling_df_hc <- readxl::read_excel('02_input/DSRA_II_Sampling_Info.xlsx', sheet = "HC") %>%
  janitor::clean_names() %>%
  select(idp_hc_code = city_town_name, district_name, district,ind_population = estimated_number_of_resident_individuals ,sample_size = sample ) %>%
  mutate(group = "Host Community",
         site_name = idp_hc_code) %>% 
  left_join(hc_hh_size, by = join_by("site_name" == "idp_hc_code")) %>% 
  mutate(population = round(ind_population / hh_size)) %>% 
  select(idp_hc_code, population, sample_size)

sampling_df <- bind_rows(sampling_df_idp,sampling_df_hc) %>%
  select(idp_hc_code, population, sample_size) %>% 
  filter(idp_hc_code %in% main_data$idp_hc_code)


actual_sampled <- main_data %>% 
  count(idp_hc_code, name = "sample_size")

sampling_df<- sampling_df %>% 
  select(-sample_size) %>% 
  left_join(actual_sampled)


main_data_weighted <- main_data %>%
  add_weights(sampling_df, 
              strata_column_dataset = "idp_hc_code",
              strata_column_sample = "idp_hc_code",
              population_column = "population") 


############################### create HH survey design and analysis #################################################


DSRA_II_Survey_Design <- main_data_weighted %>% 
  srvyr::as_survey_design(., strata = "idp_hc_code", weights = weights)

my_analysis <- create_analysis(DSRA_II_Survey_Design, loa = loa, sm_separator = "/")

results_table <- my_analysis$results_table

review_kobo_labels_results <- review_kobo_labels(kobo_survey,
                                                 kobo_choice,
                                                 label_column = "label::English (en)",
                                                 results_table = results_table)

label_dictionary <- create_label_dictionary(kobo_survey, 
                                            kobo_choice, 
                                            label_column = "label::English (en)",
                                            results_table = results_table)

results_table_labeled <- add_label_columns_to_results_table(
  results_table,
  label_dictionary
)


#### create long output tables::

### percentage tables
df_main_analysis_table <- presentresults::create_table_variable_x_group(
  analysis_key = "label_analysis_key",
  results_table = results_table_labeled, 
  value_columns = "stat")

# Replace NA values in list and non-listcolumns with NULL

df_main_analysis_table <- df_main_analysis_table %>%
  mutate(across(where(is.list), ~ map(.x, ~ ifelse(is.na(.x), list(NULL), .x)))) %>% 
  mutate(across(where(~ !is.list(.x) & is.numeric(.x)), ~ replace(.x, is.na(.x), NA))) %>%
  mutate(across(where(~ !is.list(.x) & !is.numeric(.x)), ~ ifelse(is.na(.x), "NA", .x)))



# Export the main analysis percentages table -------------------------
presentresults::create_xlsx_variable_x_group(
  table_group_x_variable = df_main_analysis_table,
  file_path = paste0("05_HQ_validation/02_results_tables/results_table_long_percent.xlsx"),
  value_columns = c("stat","n"),
  overwrite = TRUE
)


# Create and process the statistics table (counts: n, N, weighted) ----

df_stats_table <- presentresults::create_table_variable_x_group(
  results_table = results_table_labeled,
  analysis_key = "label_analysis_key",
  value_columns = c("n")
)

# Handle NA values in df_stats_table
df_stats_table <- df_stats_table %>%
  mutate(across(where(is.list), ~ map(.x, ~ ifelse(is.na(.x), list(NULL), .x)))) %>%
  mutate(across(where(~ !is.list(.x)), ~ ifelse(is.na(.x), "", .x))) %>% 
  mutate(across(where(~ !is.list(.x) & !is.numeric(.x)), ~ ifelse(is.na(.x), "NA", .x)))

# Export the processed stats table to Excel
presentresults::create_xlsx_variable_x_group(
  table_group_x_variable = df_stats_table,  # Use the processed table
  file_path = paste0("05_HQ_validation/02_results_tables/results_table_long_values.xlsx"),
  value_columns = c("n"),
  overwrite = TRUE  
)

#------------------------------------------------------------------------------------------------------------------------#
############################### create ROSTER survey design and analysis #################################################
#------------------------------------------------------------------------------------------------------------------------#


clean_roster_weighted <- clean_roster %>% 
  add_weights(sampling_df, 
              strata_column_dataset = "idp_hc_code",
              strata_column_sample = "idp_hc_code",
              population_column = "population")

Roster_Survey_Design <- clean_roster_weighted %>% 
  srvyr::as_survey_design(., strata = "idp_hc_code", weights = weights)

roster_analysis <- create_analysis(Roster_Survey_Design, loa = loa_roster, sm_separator = "/")

roster_results_table <- roster_analysis$results_table


roster_results_table_labeled <- add_label_columns_to_results_table(
  roster_results_table,
  label_dictionary
)

### percentage tables
df_roster_analysis_table <- presentresults::create_table_variable_x_group(
  analysis_key = "label_analysis_key",
    results_table = roster_results_table_labeled, 
  value_columns = "stat")

# Replace NA values in list columns with NULL

df_roster_analysis_table <- df_roster_analysis_table %>%
  mutate(across(where(is.list), ~ map(.x, ~ ifelse(is.na(.x), list(NULL), .x)))) %>% 
  mutate(across(where(~ !is.list(.x) & is.numeric(.x)), ~ replace(.x, is.na(.x), NA))) %>%
  mutate(across(where(~ !is.list(.x) & !is.numeric(.x)), ~ ifelse(is.na(.x), "NA", .x)))


# Export the main analysis percentages table -------------------------
presentresults::create_xlsx_variable_x_group(
  table_group_x_variable = df_roster_analysis_table,
  file_path = paste0("05_HQ_validation/02_results_tables/roster_results_table_long_percent.xlsx"),
  value_columns = c("stat","n"),
  overwrite = TRUE
)


# Create and process the statistics table (counts: n, N, weighted) ----

df_roster_stats_table <- presentresults::create_table_variable_x_group(
  analysis_key = "label_analysis_key",
  results_table = roster_results_table_labeled,
  value_columns = c("n")
)

# Handle NA values in df_stats_table
df_roster_stats_table <- df_roster_stats_table %>%
  mutate(across(where(is.list), ~ map(.x, ~ ifelse(is.na(.x), list(NULL), .x)))) %>%
  mutate(across(where(~ !is.list(.x)), ~ ifelse(is.na(.x), "", .x))) %>% 
  mutate(across(where(~ !is.list(.x) & !is.numeric(.x)), ~ ifelse(is.na(.x), "NA", .x)))

# Export the processed stats table to Excel
presentresults::create_xlsx_variable_x_group(
  table_group_x_variable = df_roster_stats_table,  # Use the processed table
  file_path = paste0("05_HQ_validation/02_results_tables/roster_results_table_long_values.xlsx"),
  value_columns = c("n"),
  overwrite = TRUE  
)


####################################### output the final data ###############################################################


readme <- data.frame(
  Introduction = c(
    "variable_tracker", 
    "raw_data", 
    "cleaned_data", 
    "raw_roster_data",
    "cleaned_roster",
    "survey", 
    "choices", 
    "deletion_log", 
    "cleaning_log"),
  `Sheet Descriptions` = c(
    "A list of all variables removed or added.",
    "Raw data extract", 
    "Clean data extract.", 
    "Raw roster data",
    "Cleaned Roster Data",
    "Kobo survey.",
    "Kobo choices.",
    "All surveys deleted as part of data cleaning.",
    "All changes made as part of the data cleaning process."
  )
)


cols_to_remove <- c("consent_no", "instance_note", "deviceid", "audit", "enum_name", "note_tool", "note","idp_returned_issue",
                    "idp_not_displaced","hc_displaced","date_issues","note_date_diff",
                    "healthcare_issue","land_tenure_check","threshold_msg_positive","threshold_msg_negative", "observation_gps", 
                    "observation_gps_latitude", "observation_gps_longitude", "observation_gps_altitude", "observation_gps_precision",
                    "observation_gps_wkt", "pt_sample_lat", "pt_sample_lon", "distance_to_site",
                     "_submission_time", "_validation_status", "Longitude", "Latitude", "attachments", "instanceID",
                    "_notes", "_status", "_submitted_by", "__version__", "_tags", "_index", "audit_URL", "interview_duration", "District")

raw_data <- raw_data %>% 
  select(-idp_hc_code)

raw_data_no_pii <- raw_data %>% 
  select(-any_of(cols_to_remove))

main_data_weighted_no_pii <- main_data_weighted %>% 
  select(-any_of(cols_to_remove)) %>% 
  select(-village, -idp_code, -reasons_why_far)

variable_tracker <- ImpactFunctions::create_variable_tracker(raw_data, main_data_weighted_no_pii)

log_book_output <- list(README = readme, variable_tracker = variable_tracker, raw_data = raw_data_no_pii, cleaned_data = main_data_weighted_no_pii, raw_roster_data = raw_roster_data, clean_roster = clean_roster_weighted, survey = kobo_survey, choices = kobo_choice, deletion_log = deletion_log, cleaning_logs = cleaning_logs)
writexl::write_xlsx(log_book_output, paste0("05_HQ_validation/01_all_data_and_logbook/DSRA_II_all_data_logbook_", today(), ".xlsx"))




