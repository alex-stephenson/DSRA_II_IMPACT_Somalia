### post collection checks

## read in all the data
library(tidyverse)
library(readxl)

## raw data
raw_kobo_data <- ImpactFunctions::get_kobo_data(asset_id = "amnDUBvDnga4UYnYU4g5kz", un = "abdirahmanaia")


## all clogs
dir_path <- "C:/Users/alex.stephenson/ACTED/IMPACT SOM - 01_REACH/2024_25/01_ISU/SOM1901_HSM/03_Data & Data Analysis/DEC_24/01CLeaning scripts/04_data_cleaning"

# List all .xlsx files that contain 'cleaning_log' in the name
file_list <- list.files(dir_path, pattern = "cleaning_log.*\\.xlsx$", recursive = TRUE, full.names = TRUE) %>%
  keep(~ !str_detect(.x, "report"))  # Exclude files with 'report' in the name

# Function to read and convert all columns to character
read_and_clean <- function(file, sheet) {
  read_excel(file, sheet = sheet) %>%
    mutate(across(everything(), as.character))  # Convert all columns to character
}

# Read and combine all files into a single dataframe
cleaning_logs <- map_dfr(file_list, sheet = 'cleaning_log', read_and_clean)


# now read in the deletion log

## all dlogs
dlogs_path <- r"(03_output\deletion_log)"

# List all .xlsx files that contain 'cleaning_log' in the name
dlog_list <- list.files(dlogs_path, pattern = "deletion_log.*\\.xlsx$", recursive = TRUE, full.names = TRUE)

all_dlogs <- dlog_list %>%
  map_dfr(read_excel) %>%
  pull(uuid)

## now remove dlog

raw_data <- raw_kobo_data %>%
  filter(! uuid %in% all_dlogs)


## now apply the clog and the clog using cleaningtools code


my_clean_data <- create_clean_data(raw_dataset = raw_data,
                                   raw_data_uuid_column = "uuid",
                                   cleaning_log = cleaning_logs, 
                                   cleaning_log_uuid_column = "uuid",
                                   cleaning_log_question_column = "question",
                                   cleaning_log_new_value_column = "new_value",
                                   cleaning_log_change_type_column = "change_type")



## soft duplicates

enum_typos <- my_clean_data %>%
  dplyr::count(enum_code) %>%
  filter(n < 3) %>%
  pull(enum_code)

group_by_enum <- data_in_processing %>%
  filter(!(enum_code %in% enum_typos)) %>%
  group_by(enum_code)

soft_per_enum <- group_by_enum %>%
  dplyr::group_split() %>%
  purrr::map(~ check_soft_duplicates(dataset = .,
                                     kobo_survey = kobo_survey,
                                     uuid_column = "uuid",
                                     idnk_value = "dnk",
                                     sm_separator = "/",
                                     log_name = "soft_duplicate_log",
                                     threshold = 10
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
  left_join(data_in_processing, by = "uuid") %>%
  select(fo_in_charge_for_code, district, settlement, start, end, uuid, issue, enum, num_cols_not_NA, total_columns_compared, num_cols_dnk, id_most_similar_survey, number_different_columns)


similar_survey_raw_data <- data_in_processing %>%
  filter(uuid %in% (similar_surveys_with_info$uuid))

similar_survey_export_path <- paste0("../04_data_cleaning/_similar_survey_checks/similar_surveys_", date_time_now, ".xlsx")

# create a workbook with our data
wb <- createWorkbook()
addWorksheet(wb, "similar_surveys")
addWorksheet(wb, "similar_survey_raw_data")

writeData(wb, 1, similar_surveys_with_info)
writeData(wb, 2, similar_survey_raw_data)

saveWorkbook(wb, similar_survey_export_path, overwrite = TRUE)




## over sampling

## step 1 - read in data containing the sample size for each CA / settlement

## step 2 - calculate the number of interviews per CA / settlement, based on the existing data

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



