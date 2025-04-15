rm(list = ls())
### post collection checks

## read in all the data
library(tidyverse)
library(readxl)

## FO data
fo_district_mapping <- read_excel("02_input/fo_base_assignment_DSRA_II.xlsx") %>%
  select(district_name = district, "district" = district_p_code, "fo" = fo_in_charge_for_code) 

## raw data
raw_data <- ImpactFunctions::get_kobo_data(asset_id = "aBfWuR6hn3cdMJDLRyTVKD", un = "abdirahmanaia") 
raw_kobo_data <- raw_data %>%
  pluck("main") %>%
  dplyr::rename(survey_uuid = uuid,
                uuid =`_uuid`)


## tool


kobo_tool_name <- "02_input/DSRA_II_Tool.xlsx"

# read in the survey questions / choices
kobo_survey <- read_excel(kobo_tool_name, sheet = "survey")
kobo_choice <- read_excel(kobo_tool_name, sheet = "choices")

# Define directory pattern
dir_path <- "01_cleaning_logs"


all_files <- list.files(
  path = dir_path,
  recursive = TRUE,
  full.names = TRUE
)

file_list <- all_files %>%
  keep(~ str_detect(.x, "/[^/]+_complete/") & str_detect(.x, "cleaning_log.*\\.xlsx$") & !str_detect(.x, "report"))



# Function to read and convert all columns to character
read_and_clean <- function(file, sheet) {
  read_excel(file, sheet = sheet) %>%
    mutate(across(everything(), as.character))  # Convert all columns to character
}

# Read and combine all files into a single dataframe
cleaning_logs <- map_dfr(file_list, sheet = 'cleaning_log', read_and_clean)

cleaning_logs <- cleaning_logs %>%
  filter(!is.na(change_type))

# now read in the deletion log

## all dlogs
dlogs_path <- r"(03_output\deletion_log)"

# List all .xlsx files that contain 'cleaning_log' in the name
dlog_list <- list.files(dlogs_path, pattern = "deletion_log.*\\.xlsx$", recursive = TRUE, full.names = TRUE)

all_dlogs <- dlog_list %>%
  map_dfr(., sheet = 'Sheet1', read_and_clean) %>%
  distinct() %>%
  pull(uuid)

raw_kobo_data_nas <- raw_kobo_data %>%
  mutate(interview_duration = NA)

## now apply the clog and the clog using cleaningtools code

my_clean_data <- create_clean_data(raw_dataset = raw_kobo_data_nas,
                                   raw_data_uuid_column = "uuid",
                                   cleaning_log = cleaning_logs, 
                                   cleaning_log_uuid_column = "uuid",
                                   cleaning_log_question_column = "question",
                                   cleaning_log_new_value_column = "new_value",
                                   cleaning_log_change_type_column = "change_type")




## now remove dlog

my_clean_data <- my_clean_data %>%
  filter(! uuid %in% all_dlogs)

my_clean_data %>%
  writexl::write_xlsx(., paste0('03_output/daily_cleaned_data/all_clean_data_', today(), '.xlsx'))


## only need to run it if you want

## soft duplicates

my_clean_data_added <- my_clean_data %>%
  mutate(idp_hc_code = ifelse(is.na(idp_code), village, idp_code)) %>%
  left_join(fo_district_mapping)

enum_typos <- my_clean_data_added %>%
  dplyr::count(enum_name) %>%
  filter(n >= 5) %>%
  pull(enum_name)

group_by_enum <- my_clean_data_added %>%
  filter(enum_name %in% enum_typos) %>%
  group_by(enum_name)

soft_per_enum <- group_by_enum %>%
  dplyr::group_split() %>%
  purrr::map(~ check_soft_duplicates(dataset = .,
                                     kobo_survey = kobo_survey,
                                     uuid_column = "uuid",
                                     idnk_value = "dnk",
                                     sm_separator = "/",
                                     log_name = "soft_duplicate_log",
                                     threshold = 7
  )
  )

# recombine the similar survey data
similar_surveys <- soft_per_enum %>%
  purrr::map(~ .[["soft_duplicate_log"]]) %>%
  purrr::map2(
    .y = dplyr::group_keys(group_by_enum) %>% unlist(),
    ~ dplyr::mutate(.x, enum = .y)
  ) %>%
  do.call(dplyr::bind_rows, .)


similar_surveys_with_info <- similar_surveys %>%
  left_join(my_clean_data_added, by = "uuid") %>%
  select(district, idp_hc_code, fo, start, end, uuid, issue, enum_name, num_cols_not_NA, total_columns_compared, num_cols_dnk, id_most_similar_survey, number_different_columns) 


similar_survey_raw_data <- my_clean_data %>%
  filter(uuid %in% (similar_surveys_with_info$uuid))

similar_survey_export_path <- paste0("03_output/similar_survey_checks/similar_surveys_", today(), ".xlsx")

# create a workbook with our data
wb <- createWorkbook()
addWorksheet(wb, "similar_surveys")
addWorksheet(wb, "similar_survey_raw_data")

writeData(wb, 1, similar_surveys_with_info)
writeData(wb, 2, similar_survey_raw_data)

saveWorkbook(wb, similar_survey_export_path, overwrite = TRUE)




## over sampling

## step 1 - read in data containing the sample size for each CA / settlement

sampling_df <- readxl::read_excel('02_input/DSRA_II_Sampling_Info.xlsx') %>%
  select(idp_code, sample_size, group)

## step 2 - calculate the number of interviews per settlement or HC, based on the existing data

idp_count <- my_clean_data %>%
  count(idp_code) %>%
  left_join(sampling_df) %>%
  mutate(oversampled = ifelse(n > sample_size, TRUE, FALSE))


## step 3 - calculate which sites are oversampled

## step 4 - join to any relevant data that assessment want adding - eg clog info

## output for assessment input and they will make into deletion log




## outliers - not sure if this is necessary as there's just one integer question anyway - HH size
# we should exclude all questions from outlier checks that aren't integer response types (integer is the only numerical response type)
outlier_excluded_questions <- kobo_survey %>%
  filter(type != 'integer') %>%
  pull(name) %>%
  unique()

# intersect between the dataset and the kobo tool questions to make sure we get a clean list
excluded_questions_in_data <- intersect(colnames(data_in_processing), outlier_excluded_questions)



