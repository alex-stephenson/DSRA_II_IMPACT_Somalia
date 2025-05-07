library(readxl)
library(magrittr)
library(cleaningtools)

final_check_path <- "05_HQ_validation/01_all_data_and_logbook/DSRA_II_all_data_logbook.xlsx"

raw_data <- read_excel(final_check_path, "raw_data")
cleaned_data <- read_excel(final_check_path, "cleaned_data")
deletion_log <- read_excel(final_check_path, "deletion_log")
cleaning_logs  <- read_excel(final_check_path, "cleaning_logs")

final_review <- review_cleaning(
  raw_dataset = raw_data,
  raw_dataset_uuid_column = "uuid",
  clean_dataset = cleaned_data,
  clean_dataset_uuid_column = "uuid",
  cleaning_log = cleaning_logs,
  cleaning_log_uuid_column = "uuid",
  cleaning_log_change_type_column = "change_type",
  cleaning_log_question_column = "question",
  cleaning_log_new_value_column = "new_value",
  cleaning_log_old_value_column = "old_value",
  cleaning_log_added_survey_value = "added_survey",
  cleaning_log_no_change_value = c("no_action", "no_change"),
  deletion_log = deletion_log,
  deletion_log_uuid_column = "uuid",
  check_for_deletion_log = T
)


### all approved 
cleaningtools::check_pii(cleaned_data)

cleaningtools::check_pii(raw_data)

cleaningtools::check_pii(deletion_log)

cleaningtools::check_pii(cleaning_logs)


### 

others <- cleaned_data %>% 
  dplyr::select(contains('other')) 
  
  
df_roster_analysis_table <- df_roster_analysis_table %>%
  mutate(across(where(is.list), ~ map(.x, ~ ifelse(is.na(.x), list(NULL), .x)))) %>% 


  # Count NA values in each column using base R and lapply
na_counts_dplyr <- others %>%
  summarise_all(~ sum(is.na(.)))

                