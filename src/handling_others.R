


library(readxl)
library(tidyverse)

survey <- read_excel("../../02_Data_Collection/02_Tool/REACH_2025_SOM_DSRA_II_Survey_Tool.xlsx", sheet = "survey")
choices <- read_excel("../../02_Data_Collection/02_Tool/REACH_2025_SOM_DSRA_II_Survey_Tool.xlsx", sheet = "choices")

other_questions <- survey %>%
  filter(stringr::str_detect(name, 'other') &
         type == "text" &
         str_detect(relevant, "other"))

         
other_questions_strings <- other_questions %>%
  mutate(relevant_criteria = str_extract(relevant, "\\{[a-z|_]+\\}"),
         relevant_criteria = str_remove_all(relevant_criteria, "\\{|\\}")) %>%
  pull(relevant_criteria)


list_names <- survey %>%
  filter(str_detect(type, "select")) %>%
  mutate(list_name = str_split_i(type, "[ ]+", 2)) 


other_questions <- list_names %>%
  filter(name %in% other_questions_strings)

other_questions_list_name <- other_questions %>%
  pull(list_name)

other_answer_options <- choices %>%
  filter(list_name %in% other_questions_list_name) 

## calculate each other answer and its corresponding question
other_parent_question <- other_questions %>%
  mutate(relevant_criteria = str_extract(relevant, "\\{[a-z|_]+\\}"),
         relevant_criteria = str_remove_all(relevant_criteria, "\\{|\\}")) %>%
  select(name, parent_question = relevant_criteria)

## get combination of questions and answers
questions_and_answers <- other_questions %>%
  select(type, name, list_name) %>%
  left_join((choices %>% select(list_name, answer_name = name)), by = join_by(list_name == list_name)) %>%
  mutate(question_answer = paste0(name,"/", answer_name)) %>%
  pull(question_answer)

## identify whether question is select one or select multiple

select_type <- survey %>%
  filter(str_detect(type, "select")) %>%
  mutate(select_type = str_split_i(type, "[ ]+", 1)) %>%
  select(name, select_type)


## read in a clog example
cleaning_log<- read_excel("01_cleaning_logs/ahad/ahad_complete/cleaning_log_ahad_Mar_19_2025_143544.xlsx", sheet = "cleaning_log")


clogs_validated <- cleaning_log %>%
  filter(change_type == "change_response",
         issue == "recode other") %>%
  left_join(other_parent_question, by = join_by(question == name)) %>%
  mutate(question_answer = paste0(parent_question,"/", new_value),
         answer_valid = ifelse(question_answer %in% questions_and_answers, TRUE, FALSE)) %>%
  left_join(select_type, by = join_by(parent_question == name))

valid_clogs <- clogs_validated %>%
  filter(answer_valid == TRUE)


## 
update_select_one <- valid_clogs %>%
  filter(select_type == "select_multiple")%>%
  select(question = question_answer) %>%
  mutate(new_value = 1)
  
### still to do - how does other get turned off, how does the value of the original question get nullified

