rm(list = ls())
### post collection checks

library(tidyverse)
library(cleaningtools)
library(readxl)

date_time_now <- format(Sys.time(), "%b_%d_%Y_%H%M%S")

## FO data
fo_district_mapping <- read_excel("02_input/fo_base_assignment_DSRA_II.xlsx") %>%
  select(district_name = district, "district" = district_p_code, "fo" = fo_in_charge_for_code) 

## raw data
raw_kobo_data <- read_csv("03_output/raw_data/raw_kobo_output.csv", guess_max = 10000)

raw_kobo_roster <-  read_csv("03_output/raw_data/raw_roster_output.csv")


## tool
kobo_tool_name <- "02_input/DSRA_II_Tool.xlsx"

# read in the survey questions / choices
kobo_survey <- read_excel(kobo_tool_name, sheet = "survey") %>% 
  mutate(type = stringr::str_squish(type))

kobo_choice <- read_excel(kobo_tool_name, sheet = "choices")



# Define directory pattern
dir_path <- "01_cleaning_logs"


all_files <- list.files(
  path = dir_path,
  recursive = TRUE,
  full.names = TRUE
)

## this is to only keep the files in a folder with _complete_validated/ in the title. But you can adjust to remove any as required.
file_list <- all_files %>%
  keep(~ (str_detect(.x, "/[^/]+_complete_validated/") & str_detect(.x, "cleaning_log.*\\.xlsx$") & !str_detect(.x, "report")))



# Function to read and convert all columns to character
read_and_clean <- function(file, sheet) {
  read_excel(file, sheet = sheet) %>%
    mutate(across(everything(), as.character))  %>%  # Convert all columns to character
    mutate(file_path = file)
}

# Read and combine all files into a single dataframe
cleaning_logs <- map_dfr(file_list, sheet = 'cleaning_log', read_and_clean)

cleaning_logs <- cleaning_logs %>%
#  filter(!is.na(change_type)) %>% ### this needs taking out at the end
  mutate(question = ifelse(question == "hh_size_roster", "hh_roster_count", question))

## also check all dlogs

deletion_exclusions <- read_excel("02_input/deletion_exclusions.xlsx")

all_dlogs <- readxl::read_excel(r"(03_output/deletion_log/deletion_log.xlsx)", col_types = "text")
manual_dlog <- readxl::read_excel("03_output/deletion_log_manual/DSRA_II_Manual_Deletion_Log.xlsx", col_types = "text")
cleaning_log_deletions <- cleaning_logs %>% 
  filter(change_type == "remove_survey") %>% 
  mutate(interview_duration = "") %>% 
  select(uuid, site_name, enum_name,interview_duration, comment = issue)

all_deletions <- bind_rows(all_dlogs, manual_dlog) %>% 
  bind_rows(cleaning_log_deletions) %>% 
  filter(! uuid %in% deletion_exclusions$uuid)


##### check others

cleaning_logs_amended <- ImpactFunctions::update_others(kobo_survey, kobo_choice, cleaning_log = cleaning_logs)

raw_kobo_data_nas <- raw_kobo_data %>%
  mutate(interview_duration = NA)

## now apply the clog and the clog using cleaningtools code

my_clean_data <- create_clean_data(raw_dataset = raw_kobo_data_nas,
                                   raw_data_uuid_column = "uuid",
                                   cleaning_log = cleaning_logs_amended, 
                                   cleaning_log_uuid_column = "uuid",
                                   cleaning_log_question_column = "question",
                                   cleaning_log_new_value_column = "new_value",
                                   cleaning_log_change_type_column = "change_type")

my_clean_data_parentcol <- recreate_parent_column(dataset = my_clean_data,
                                         uuid_column = "uuid",
                                         kobo_survey = kobo_survey,
                                         kobo_choices = kobo_choice,
                                         sm_separator = "/", 
                                         cleaning_log_to_append = cleaning_logs_amended)



cleaning_log <- my_clean_data_parentcol$cleaning_log

my_clean_data <- my_clean_data_parentcol$data_with_fix_concat %>%
  mutate(idp_hc_code = ifelse(is.na(idp_code), village, idp_code)) %>%
  filter(! uuid %in% all_deletions$uuid)


my_clean_roster_data <- raw_kobo_roster %>% 
  filter(parent_instance_name %in% my_clean_data$instance_name)

cleaning_log_final <- cleaning_log %>% 
  filter(! uuid %in% all_deletions$uuid)

## output everything

all_clean_data <- list("clean_HH_data" = my_clean_data, "clean_roster_data" = my_clean_roster_data, "raw_HH_data" = raw_kobo_data, "raw_roster_data" = raw_kobo_roster)

my_clean_data %>%
  writexl::write_xlsx(., paste0('03_output/daily_cleaned_data/all_clean_data_', today(), '.xlsx'))

my_clean_data %>%
  select(!contains('/')) %>%
  writexl::write_xlsx(., paste0('03_output/daily_cleaned_data/all_clean_data_dashboard.xlsx'))

all_clean_data %>% 
  writexl::write_xlsx(., "03_output/final_cleaned_data/SOM_DSRA_II_Output.xlsx")

cleaning_log_final %>% 
  writexl::write_xlsx(., "03_output/combined_cleaning_log/combined_cleaning_log.xlsx")

all_deletions %>% 
  
  writexl::write_xlsx(., "03_output/combined_cleaning_log/combined_deletion_log.xlsx")





############################################# soft duplicates #####################################################

## read in already approved ones
exclusions <- read_excel("03_output/similar_survey_checks/exclusions.xlsx")

my_clean_data_added <- my_clean_data %>%
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
                                     threshold = 5
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
  left_join(my_clean_data_added %>% select(uuid, similiar_survey_date = today), by = join_by("id_most_similar_survey" == "uuid")) %>%
  select(district, idp_hc_code, fo, today, start, end, uuid, issue, enum_name, num_cols_not_NA, total_columns_compared, num_cols_dnk, similiar_survey_date, id_most_similar_survey, number_different_columns) %>% 
  filter(! uuid %in% exclusions$uuid & ! id_most_similar_survey %in% exclusions$id_most_similar_survey,
         today == similiar_survey_date)


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

## outliers - not sure if this is necessary as there's just one integer question anyway - HH size
# we should exclude all questions from outlier checks that aren't integer response types (integer is the only numerical response type)
outlier_excluded_questions <- kobo_survey %>%
  filter(type != 'integer') %>%
  pull(name) %>%
  unique()

# intersect between the dataset and the kobo tool questions to make sure we get a clean list
excluded_questions_in_data <- intersect(colnames(data_in_processing), outlier_excluded_questions)



