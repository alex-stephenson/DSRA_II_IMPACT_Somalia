
audit_files <- robotoolbox::kobo_audit(x = "aBfWuR6hn3cdMJDLRyTVKD", progress = T)
audit_files_length <- audit_files %>% dplyr::mutate(metadata_duration = (end_int - start_int)/60000)

                                                                           

ids <- data_in_processing %>%
  filter(enum_name == 20358) %>% 
  filter(date(start) == "2025-04-13") %>% 
  pull(`_id`)


df <- audit_files_length %>% 
  filter(`_id` %in% ids)

df <- audit_files_length %>% 
  filter(event == "form start") %>% 
  left_join(data_in_processing %>%  select(`_id`, enum_name, District)) 
  

library(dplyr)

# Define the time window in seconds
time_window <- 5 * 60  # 5 minutes

# Perform a self-join on enum_name and filter based on time difference
close_surveys <- df %>%
  select(id1 = `_id`, enum_name, start1 = start, District) %>%
  inner_join(df %>% select(id2 = `_id`, enum_name, start2 = start), by = "enum_name") %>%
  filter(id1 < id2, abs(as.numeric(difftime(start1, start2, units = "secs"))) <= time_window)

# View the result
close_surveys %>% count(id1) %>%  arrange(desc(n))


start_and_end <- close_surveys %>% 
  filter(id1 %in% c('2572978', '2574610', '2567482')) %>% 
  left_join(data_in_processing %>% select(`_id`, uuid_1 = uuid), by = join_by("id1" == `_id`)) %>% 
  left_join(data_in_processing %>% select(`_id`, uuid_2 = uuid), by = join_by("id2" == `_id`)) %>% 
  select(enum_name, District, uuid_1, start1, uuid_2, start2)

write_csv(start_and_end, "close_surveys.csv")

library(ggplot2)

df %>% 
  filter(date(start) == "2025-04-13") %>% 
  filter(hour(start)< 15) %>%
  ggplot(., aes(x = start, 
                y = 1, 
                color = factor(`_id`))) +
  geom_point(size = 3) +
  #scale_x_datetime(breaks = scales::date_breaks("30 min"), date_labels = "%H:%M") +
  labs(x = "Time", title = "Start times of questions") +
  theme_minimal()

df %>%
  mutate(start = lubridate::round_date(start, unit = "minutes")) %>% 
  
  ggplot(., aes(x=x)) +
  geom_histogram(aes(`_id` = ))
  
  
  ggplot(., aes(x = start,, group = `_id`)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Time", y = "Count of Questions", title = "Distribution of Start Times") +
  theme_minimal()
