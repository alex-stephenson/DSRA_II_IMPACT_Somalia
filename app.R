rm(list = ls())

#list.files("03_output/deletion_log", recursive = TRUE)

### Dashboard Data
library(dplyr)
library(ggplot2)
library(readxl)
library(readr)
library(purrr)
library(tidyr)
library(stringr)
library(lubridate)
library(shiny)
library(shinydashboard)
library(reactable)
library(openxlsx)

### read in relevant data
site_data <- read.csv(here::here("04_tool", "239_site_lookup.csv"))


### raw data
#raw_kobo_data <- read_csv("03_output/raw_data/raw_kobo_output.csv")

### clean data
clean_data_list <- file.info(list.files("03_output/daily_cleaned_data/", full.names = T))
latest_file <- rownames(clean_data_list)[which.max(clean_data_list$mtime)]
clean_data_raw <- readxl::read_excel(latest_file)

clean_data <- clean_data_raw %>% 
  mutate(idp_hc_code = ifelse(is.na(idp_code), village, idp_code)) %>%
  left_join(site_data, by = "idp_code") %>%
  relocate(site_name, .after = idp_code) %>%
  relocate(idp_hc_code, .before = idp_code) %>% 
  mutate(site_name = ifelse(is.na(site_name), village, site_name))

rm(clean_data_raw)

### deletion log

all_dlogs <- readxl::read_excel("03_output/deletion_log/deletion_log.xlsx")

manual_dlog <- readxl::read_excel("03_output/deletion_log_manual/DSRA_II_Manual_Deletion_Log.xlsx")

all_dlogs <- rbind(all_dlogs, manual_dlog)

### Sampling data
## over sampling

sampling_df_idp <- readxl::read_excel('02_input/DSRA_II_Sampling_Info.xlsx', sheet = "IDP") %>%
  select(idp_hc_code = idp_code, site_name, district_name = District, district = district_code, sample_size, group)

sampling_df_hc <- readxl::read_excel('02_input/DSRA_II_Sampling_Info.xlsx', sheet = "HC") %>%
  janitor::clean_names() %>%
  select(idp_hc_code = city_town_name, district_name, district, sample_size = sample) %>%
  mutate(group = "Host Community",
         site_name = idp_hc_code)

sampling_df <- rbind(sampling_df_idp,sampling_df_hc) 


idp_count <- clean_data %>%
  count(idp_hc_code, district) %>%
  left_join(sampling_df, by = c("idp_hc_code", "district")) %>%
  mutate(oversampled = ifelse(n > sample_size, TRUE, FALSE))

### FO Data
fo_district_mapping <- read_excel("02_input/fo_base_assignment_DSRA_II.xlsx") %>%
  select(district_name = district, "district" = district_p_code, "fo" = fo_in_charge_for_code) 


dashboard_data <- clean_data %>%
  select(-idp_code) %>%
  rename(idp_code = idp_hc_code) %>%
  ## add in FO
    left_join(fo_district_mapping) %>%
  ##
  dplyr::rename("enum_phone" = enum_name) 

rm(clean_data)

### Site level Completion

sites_done <- dashboard_data %>% 
  group_by(fo, idp_code) %>%
  dplyr::summarise(surveys_done = n()) %>%
  ungroup()

KIIs_Done <- sampling_df %>% 
  rename(idp_code = idp_hc_code) %>%
  left_join(sites_done %>% select(idp_code, surveys_done)) %>%
  left_join((fo_district_mapping %>% select(-district_name)), by = join_by("district")) %>%
  filter(group == "Core 189" | (group == "Buffer 50" & surveys_done > 0) | (group == "Host Community")) %>%
  mutate(surveys_done = replace_na(surveys_done, 0)) %>%
  rename(total_surveys = sample_size) %>%
  select(fo, idp_code, site_name, district_name, surveys_done, total_surveys) %>%
  mutate(Complete = ifelse(surveys_done >= total_surveys, "Yes", "No"))

KIIs_Done %>%
  writexl::write_xlsx(., "03_output/completion_report/completion_report.xlsx")

## Completion by FO

completion_by_FO <- KIIs_Done %>%
  group_by(fo) %>%
  summarise(total_surveys = sum(total_surveys),
            total_done = sum(surveys_done)) %>%
  mutate(Completion_Percent = round((total_done / total_surveys) * 100, 1)) %>%
  mutate(Completion_Percent = ifelse(Completion_Percent > 100, 100, Completion_Percent))

### OPZ burndown
# Parameters for the ideal burndown chart
total_tasks <- 11710
days <- 26

# Ideal burndown data (linear decrease)
ideal_burndown <- data.frame(
  Day = 1:days,
  Remaining_Tasks = seq(from = total_tasks, to = 0, length.out = days)
)


# Prepare the actual burndown data
actual_burndown <- dashboard_data %>%
  mutate(today = as.Date(today),
         Day = as.integer(today - min(today)) + 1) %>%  # Calculate day number s
  group_by(Day) %>%  # Group by FO, Region, and District
  summarise(
    Tasks_Completed = n(),  # Count tasks completed on each day
    .groups = "drop"
  ) %>%
  mutate(
    Remaining_Tasks = total_tasks - cumsum(Tasks_Completed)  # Calculate running total
  )


# Sleeker Burndown Chart
burndown <- ggplot() +
  # Ideal burndown line
  geom_line(data = ideal_burndown, aes(x = Day, y = Remaining_Tasks), 
            color = "#2C3E50", size = 1, linetype = "dashed") +
  # Actual progress line
  geom_line(data = actual_burndown, aes(x = Day, y = Remaining_Tasks), 
            color = "#EE5859", size = 1.5, alpha = 0.5) +
  # Points for actual progress
  geom_point(data = actual_burndown, aes(x = Day, y = Remaining_Tasks), 
             color = "#EE5859", size = 2) +
  labs(
    x = "Days",
    y = "Remaining Surveys"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#2C3E50"),
    axis.title = element_text(size = 14, face = "bold", color = "#34495E"),
    axis.text = element_text(size = 12, color = "#34495E"),
    panel.grid.major = element_line(color = "#BDC3C7", size = 0.5),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  ) +
  scale_y_continuous(breaks = seq(0, total_tasks, by = 400), labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, max(ideal_burndown$Day), by = 2))

### enumerator performance


deleted_data <- all_dlogs %>%
  rename(enum_phone = enum_name) %>%
  count(enum_phone, name = "deleted") %>%
  mutate(enum_phone = as.character(enum_phone))

valid_data <- dashboard_data %>%
  count(enum_phone, name = "valid") %>%
  mutate(enum_phone = as.character(enum_phone))


enum_performance <- deleted_data %>%
  full_join(valid_data) %>%
  mutate(valid = replace_na(valid, 0),
         deleted = replace_na(deleted, 0),
         total = deleted + valid, 
         pct_valid = round((valid / (deleted + valid)) * 100)) %>%
  filter(total > 5)


mean_per_day <- dashboard_data %>%
  group_by(fo, enum_phone, today, district = localisation_district_label) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(fo, enum_phone, district) %>%
  summarise("Average per day" = round(mean(n))) %>%
  mutate(enum_phone = as.character(enum_phone))

enum_performance <- enum_performance %>%
  left_join(mean_per_day) %>%
  select(fo, enum_phone, district, valid, deleted, total, pct_valid, `Average per day`)



####### Shiny Dashboard

{
# UI
ui <- dashboardPage(
  dashboardHeader(title = "DSRA II April 2025 Dashboard"),
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))))
  ,
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Site Completion", status = "primary", solidHeader = TRUE, width = 6,
                    reactableOutput("reactable_OPZ"),
                    downloadButton("download_OPZ", "Export as Excel")
                ),
                box(title = "Enumerator Performance", status = "primary", solidHeader = TRUE, width = 6,
                    reactableOutput("reactable_enum"),
                    downloadButton("download_enum", "Export as Excel")
                )
              ),
              fluidRow(
                box(title = "Total Surveys vs Surveys Completed Over Time", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("plot_burndown")
                ),
                box(title = "Completion by FO", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("plot_completion_by_FO")
                )
              )
      )
    )
  )
)



# Server
server <- function(input, output, session) {
  # Reactive filtered data
  
  # Reactable for OPZ Completion
  output$reactable_OPZ <- renderReactable({
    reactable(KIIs_Done, searchable = TRUE, bordered = TRUE, filterable = TRUE)
  })
  
  # Download for OPZ Completion
  output$download_OPZ <- downloadHandler(
    filename = function() {
      paste("Survey_District_Done", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(KIIs_Done, file)
    }
  )
  
  # Bar Chart for Completion by FO
  output$plot_completion_by_FO <- renderPlot({
    ggplot(completion_by_FO, aes(x = reorder(fo, -Completion_Percent), y = Completion_Percent, fill = fo)) +
      geom_col(show.legend = FALSE, width = 0.6) +
      geom_text(aes(label = paste0(Completion_Percent, "%")), vjust = -0.5, size = 5) +
      labs(title = "Completion by FO", x = "Field Officer", y = "Completion Percentage") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
      ) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))  # Corrected y-axis scale
  })
  
  
  # Burndown Chart
  output$plot_burndown <- renderPlot({
    burndown
  })
  
  # Reactable for Enumerator Performance
  output$reactable_enum <- renderReactable({
    reactable(enum_performance, searchable = TRUE, bordered = TRUE)
  })
  
  # Download for Enumerator Performance
  output$download_enum <- downloadHandler(
    filename = function() {
      paste("Enumerator_Performance_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(enum_performance, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)#
} ## all the code used to make the dashboard in Shiny

