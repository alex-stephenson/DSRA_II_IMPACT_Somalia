rm(list=ls())
library(readxl)
library(tidyverse)
library(openxlsx)
library(cleaningtools)


raw_kobo_data <- read_excel("03_output/final_cleaned_data/SOM_DSRA_II_Output.xlsx", sheet = "raw_HH_data")
clean_kobo_data <- read_excel("03_output/final_cleaned_data/SOM_DSRA_II_Output.xlsx", sheet = "clean_HH_data")
cleaning_log_final <- read_excel("03_output/combined_cleaning_log/combined_cleaning_log.xlsx")
deletion_log_final <- read_excel("03_output/combined_cleaning_log/combined_deletion_log.xlsx")

#######################################################################################
############################## Review the cleaning logs ###############################
#######################################################################################

review_clog <- cleaningtools::review_cleaning_log(raw_kobo_data,
                                   raw_data_uuid_column = "uuid",
                                   cleaning_log = cleaning_log_final,
                                   cleaning_log_uuid_column = "uuid",
                                   cleaning_log_question_column = "question",
                                   cleaning_log_new_value_column = "new_value",
                                   cleaning_log_change_type_column = "change_type",
                                   change_response_value = "change_response"
)


#######################################################################################################
######################## Check for discrepancies between clog and clean data ##########################
#######################################################################################################

review_cleaning <- review_cleaning(raw_kobo_data,
                                   raw_dataset_uuid_column = "uuid",
                                   clean_kobo_data,
                                   clean_dataset_uuid_column = "uuid",
                                   cleaning_log = cleaning_log_final,
                                   cleaning_log_uuid_column = "uuid",
                                   cleaning_log_change_type_column = "change_type",
                                   cleaning_log_question_column = "question",
                                   cleaning_log_new_value_column = "new_value",
                                   cleaning_log_old_value_column = "old_value",
                                   cleaning_log_added_survey_value = "added_survey",
                                   cleaning_log_no_change_value = c("no_action", "no_change"),
                                   deletion_log = deletion_log_final,
                                   deletion_log_uuid_column = "uuid",
                                   check_for_deletion_log = T
)



#### action everything #####

# Define directory pattern
dir_path <- "01_cleaning_logs"

all_files <- list.files(
  path = dir_path,
  recursive = TRUE,
  full.names = TRUE
)

file_list <- all_files %>%
  keep(~ str_detect(.x, "/[^/]+_complete_validated/") & str_detect(.x, "cleaning_log.*\\.xlsx$") & !str_detect(.x, "report"))

# Function to read and convert all columns to character
read_and_clean_uuid <- function(file, sheet) {
  read_excel(file, sheet = sheet) %>%
    mutate(across(everything(), as.character)) %>%  # Convert all columns to character
    mutate(file_path = file) %>% 
    select(uuid, file_path)
}

# Read and combine all files into a single dataframe
cleaning_logs_uuid <- map_dfr(file_list, sheet = 'cleaning_log', read_and_clean_uuid)

cleaning_logs <- cleaning_logs %>%
  #  filter(!is.na(change_type)) %>% ### this needs taking out at the end
  mutate(question = ifelse(question == "hh_size_roster", "hh_roster_count", question))


review_cleaning %>% 
  left_join(cleaning_logs_uuid %>% distinct()) %>% 
  View()


write.xlsx(review_cleaning,"input/cleaned/hh_roster/review.xlsx")
write.xlsx(clean_data,"input/cleaned/cleaned_data.xlsx")
