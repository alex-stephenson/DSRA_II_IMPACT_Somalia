rm(list=ls())
library(readxl)
library(tidyverse)
library(openxlsx)
library(cleaningtools)


raw_dataset <- read_csv("03_output/raw_data/raw_kobo_output.csv")

####################################################################################
##################### read in all the clogs ########################
####################################################################################

# Define directory pattern
dir_path <- "01_cleaning_logs/omar/omar_complete_validated"


all_files <- list.files(
  path = dir_path,
  recursive = TRUE,
  full.names = TRUE
)


# Function to read and convert all columns to character
read_and_clean <- function(file, sheet) {
  read_excel(file, sheet = sheet) %>%
    mutate(across(everything(), as.character))  # Convert all columns to character
}


# Read and combine, adding file path as an `id` column
cleaning_logs <- map_dfr(all_files, function(file) {
  read_and_clean(file, sheet = "cleaning_log") %>%
    mutate(file_path = file)
})


cleaning_logs <- cleaning_logs %>%
  mutate(question = ifelse(question == "hh_size_roster", "hh_roster_count", question))


#######################################################################################
############################## Review the cleaning logs ###############################
#######################################################################################

review_clog <- cleaningtools::review_cleaning_log(raw_dataset,
                                   raw_data_uuid_column = "uuid",
                                   cleaning_log = cleaning_logs,
                                   cleaning_log_uuid_column = "uuid",
                                   cleaning_log_question_column = "question",
                                   cleaning_log_new_value_column = "new_value",
                                   cleaning_log_change_type_column = "change_type",
                                   change_response_value = "change_response"
)




# rbind all the deletions
deletion_from_clogs <- clogs %>% 
  filter(change_type == "remove_survey")


#######################################################################################
############################ Create clean data with clogs #############################
#######################################################################################

clean_data <- create_clean_data(raw_dataset,
                                raw_data_uuid_column = "uuid",
                                clogs,
                                cleaning_log_uuid_column = "uuid",
                                cleaning_log_question_column = "question",
                                cleaning_log_new_value_column = "new_value",
                                cleaning_log_change_type_column = "change_type",
                                change_response_value = "change_response",
                                NA_response_value = "blank_response",
                                no_change_value = "no_action",
                                remove_survey_value = "remove_survey"
) 



##########################################################################################################################
############################### Removing any clog entries associated with deleted surveys ################################
##########################################################################################################################

clog_input_from_surveys_not_removed <- filter(clogs, uuid %!in% deletion_from_clogs$uuid)

#######################################################################################################
######################## Check for discrepancies between clog and clean data ##########################
#######################################################################################################

review_cleaning <- review_cleaning(raw_dataset,
                                   raw_dataset_uuid_column = "uuid",
                                   clean_data,
                                   clean_dataset_uuid_column = "uuid",
                                   cleaning_log = clog_input_from_surveys_not_removed,
                                   cleaning_log_uuid_column = "uuid",
                                   cleaning_log_change_type_column = "change_type",
                                   cleaning_log_question_column = "question",
                                   cleaning_log_new_value_column = "new_value",
                                   cleaning_log_old_value_column = "old_value",
                                   cleaning_log_added_survey_value = "added_survey",
                                   cleaning_log_no_change_value = c("no_action", "no_change"),
                                   deletion_log = deletion_from_clogs,
                                   deletion_log_uuid_column = "uuid",
                                   check_for_deletion_log = T
)

write.xlsx(review_cleaning,"input/cleaned/hh_roster/review.xlsx")


write.xlsx(clean_data,"input/cleaned/cleaned_data.xlsx")
