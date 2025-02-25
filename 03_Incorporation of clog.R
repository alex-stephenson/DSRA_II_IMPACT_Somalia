rm(list=ls())
library(readxl)
library(tidyverse)
library(openxlsx)
library(cleaningtools)


raw_dataset <- read_excel("input/cleaned/hh_roster/hh_roster_raw.xlsx",sheet = "Sheet3")
clean_data <- read_excel("input/cleaned/hh_roster/hh_roster_cleaned.xlsx",sheet = "Sheet3")
clogs <- read_excel("input/cleaned/hh_roster/roster_clog.xlsx")


#######################################################################################
############################## Review the cleaning logs ###############################
#######################################################################################

review_clog <- review_cleaning_log(raw_dataset,
                                   raw_data_uuid_column = "uuid",
                                   clogs,
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

"%!in%" <- Negate("%in%")
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
