rm(list = ls())

library(readxl)
library(tidyverse)

survey <- read_excel("../../02_Data_Collection/02_Tool/REACH_2025_SOM_DSRA_II_Survey_Tool.xlsx", sheet = "survey")
choices <- read_excel("../../02_Data_Collection/02_Tool/REACH_2025_SOM_DSRA_II_Survey_Tool.xlsx", sheet = "choices")


## get clogs
{
  
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
  
}


cleaning_log <- cleaning_logs





## return all questions that are other free text
other_text_questions <- survey %>%
  filter(stringr::str_detect(name, 'other') &
         type == "text" &
         str_detect(relevant, "other"))

## now from these others identify what the name of the parent question is from the relevance criteria         
other_questions_strings <- other_text_questions %>%
  mutate(relevant_criteria = str_extract(relevant, "\\{[A-Za-z0-9_]+\\}"),
         relevant_criteria = str_remove_all(relevant_criteria, "\\{|\\}")) %>%
  pull(relevant_criteria)

## get the name of the answer options list from the tool
list_names <- survey %>%
  filter(str_detect(type, "select")) %>%
  mutate(list_name = str_split_i(type, "[ ]+", 2)) 

# now filter for the other parent questions
parent_questions <- list_names %>%
  filter(name %in% other_questions_strings)

# extract the relevant answers options
parent_questions_list_name <- parent_questions %>%
  pull(list_name)

# now we filter the choices tab based on this, so we only have the answers for other options
other_answer_options <- choices %>%
  filter(list_name %in% parent_questions_list_name) 

## calculate each other answer and its corresponding question
other_parent_question <- other_text_questions %>%
  mutate(relevant_criteria = str_extract(relevant, "\\{[A-Za-z0-9_]+\\}"),
         relevant_criteria = str_remove_all(relevant_criteria, "\\{|\\}")) %>%
  select(name, parent_question = relevant_criteria)

## get combination of questions and answers
questions_and_answers <- parent_questions %>%
  select(type, name, list_name) %>%
  left_join((choices %>% select(list_name, answer_name = name)), by = join_by(list_name == list_name)) %>%
  mutate(question_answer = paste0(name,"/", answer_name)) %>%
  pull(question_answer)

## identify whether question is select one or select multiple

select_type <- survey %>%
  filter(str_detect(type, "select")) %>%
  mutate(select_type = str_split_i(type, "[ ]+", 1)) %>%
  select(name, select_type)


## check if the answers are valid
clogs_validated <- cleaning_log %>%
  filter(change_type == "change_response",
         issue == "recode other") %>%
  left_join(other_parent_question, by = join_by(question == name)) %>%
  mutate(question_answer = paste0(parent_question,"/", new_value),
         answer_valid = ifelse(question_answer %in% questions_and_answers, TRUE, FALSE)) %>%
  left_join(select_type, by = join_by(parent_question == name))

valid_clogs <- clogs_validated %>%
  filter(answer_valid == TRUE) 

## select multiple update
update_select_multiple <- valid_clogs %>%
  filter(select_type == "select_multiple") %>%
  select(uuid, question = question_answer, change_type) %>%
  mutate(new_value = 1)

turn_off_other <- update_select_multiple %>%
  mutate(question = paste0(str_split_i(question, "/", 1), "/other"),
         new_value = 0)

nullify_select_multiple <- valid_clogs %>% 
  filter(select_type == "select_multiple") %>%
  select(uuid, question, change_type) %>% 
  mutate(new_value= NA)


select_mulitple_clog <- bind_rows(update_select_multiple, turn_off_other, nullify_select_multiple)
  


## select multiple update
update_select_one <- valid_clogs %>%
  filter(select_type == "select_one")%>%
  select(uuid, question = parent_question, change_type, new_value)

nullify_select_one <- valid_clogs %>% 
  filter(select_type == "select_one") %>%
  select(uuid, question, change_type) %>% 
  mutate(new_value= NA)

select_one_clog <- bind_rows(update_select_one, nullify_select_one)

others_clog <- rbind(select_mulitple_clog, select_one_clog) %>%
  arrange(uuid)







