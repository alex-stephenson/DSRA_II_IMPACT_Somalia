rm(list = ls())

### Dashboard Data
library(dplyr)
library(ggplot2)
library(readxl)
library(purrr)
library(tidyr)
library(stringr)
library(fuzzyjoin)
library(lubridate)
library(shiny)
library(shinydashboard)
library(reactable)
library(openxlsx)


### read in relevant data


### raw data
raw_kobo_data <- ImpactFunctions::get_kobo_data(asset_id = "amnDUBvDnga4UYnYU4g5kz", un = "abdirahmanaia")


### clean data
clean_data_list <- file.info(list.files("03_output/daily_cleaned_data/", full.names = T))
latest_file <- rownames(clean_data_list)[which.max(clean_data_list$mtime)]
clean_data <- readxl::read_excel(latest_file)

### deletion log
dlog_list <- list.files(dlogs_path, pattern = "deletion_log.*\\.xlsx$", recursive = TRUE, full.names = TRUE)

all_dlogs <- dlog_list %>%
  map_dfr(read_excel)

### Sampling data


### FO Data
fo_district_mapping <- read_excel("inputs/fo_base_assignment_DSRA_II.xlsx") %>%
  select(district, "district_code" = district_p_code, "fo" = fo_in_charge_for_code) %>%
  mutate_all(tolower)



dashboard_data_path = r"(outputs/dashboard/)"

dashboard_data_files_names <- list.files(path = dashboard_data_path,
                                         pattern = "*unchecked_data_for_dashboard_*", 
                                         recursive = FALSE)

dashboard_data <- dashboard_data_files_names %>%
  map_dfr(~read_excel(paste0(dashboard_data_path,.x))) %>%
  dplyr::rename("FO" = fo_in_charge_for_code,
                "District" = district,
                "Settlement" = settlement,
                "Enum Phone" = enum_code,
                "Interview Type" = ki_interview
  ) %>%
  mutate_all(tolower)

dashboard_data_all <- dashboard_data

dashboard_data <- dashboard_data %>%
  filter(length_valid== "okay")


## remove data that is oversampled

dashboard_data <- dashboard_data %>%
  filter(str_detect(Settlement, "so")) %>%
  group_by(Settlement) %>%
  dplyr::slice_head(n = 1) %>%
  ungroup()

fo_district_mapping <- read_excel("inputs/fo_base_assignment_1224.xlsx") %>%
  select(district, "district_code" = district_for_code, "fo" = fo_in_charge_for_code) %>%
  mutate_all(tolower)

### OPZ Completion

KIIs_Done <- dashboard_data %>% 
  group_by(FO, operational_zone) %>%
  dplyr::summarise(surveys_done = n()) %>%
  ungroup() %>%
  select(-(FO)) %>%
  mutate(operational_zone = str_replace_all(operational_zone, "_", " "))

districts <- fo_district_mapping %>%
  select(district) %>%
  distinct(district) %>%
  pull(district) 


OPZs <- readxl::read_excel("inputs/Very Heavy Restriction Settlements 6_REF.xlsx", sheet = "Correct") %>%
  mutate_all(tolower) %>% 
  filter(District %in% districts) %>%
  select(-Region) %>%
  dplyr::rename("Region" = "admin1Name")

# Summarize the OPZs data by Region, District, and OPZ
OPZ_grpd <- OPZs %>%
  group_by(Region, District, OPZ) %>%
  dplyr::summarise(total_surveys = n(), .groups = "drop") %>%
  left_join(
    fo_district_mapping,
    by = c("District" = "district"))

OPZ_Completion <- OPZ_grpd %>%
  left_join(KIIs_Done, by = join_by("OPZ" == "operational_zone")) %>% 
  mutate(surveys_done = ifelse(is.na(surveys_done), 0, surveys_done),
         "% Complete" = round((surveys_done / total_surveys) * 100, 1)) %>%
  select(-Region, -district_code) %>%
  select(fo, District, OPZ, total_surveys, surveys_done, `% Complete`)


completion_by_FO <- OPZ_Completion %>%
  group_by(fo) %>%
  summarise(total_surveys = sum(total_surveys),
            total_done = sum(surveys_done)) %>%
  mutate(Completion_Percent = round((total_done / total_surveys) * 100, 1))

### OPZ burndown
# Parameters for the ideal burndown chart
total_tasks <- 1805
days <- 13

# Ideal burndown data (linear decrease)
ideal_burndown <- data.frame(
  Day = 1:days,
  Remaining_Tasks = seq(from = total_tasks, to = 0, length.out = days)
)


# Prepare the actual burndown data
actual_burndown <- dashboard_data %>%
  filter(date(`_submission_time`) >= "2024-12-18") %>%  # Filter relevant dates
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


enum_performance <- dashboard_data_all %>%
  count(FO, `Enum Phone`, length_valid) %>%
  mutate(length_valid = 
           case_when(
             length_valid == 'too short' ~ 'deleted',
             length_valid == 'too long' ~ 'deleted',
             TRUE ~ length_valid
           )) %>%
  group_by(FO, `Enum Phone`, length_valid) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from = length_valid, values_from = n, values_fill = 0) %>%
  mutate(
    Total = deleted + okay,
    "Percent Accepted" = (okay / (deleted + okay)) * 100
  ) %>%
  select(-deleted, -okay)


mean_per_day <- dashboard_data_all %>%
  group_by(FO, `Enum Phone`, today) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(FO, `Enum Phone`) %>%
  summarise("Average per day" = mean(n))

enum_performance <- enum_performance %>%
  left_join(mean_per_day)



### check number of surveys per settlement:

sites_and_surveys <- OPZs %>%
  left_join(
    fo_district_mapping,
    by = c("District" = "district")) %>%
  select(District, name, Settlement = Pcode, FO = fo, operational_zone = OPZ) %>%
  mutate(Settlement = str_replace_all(Settlement, " ", "_"),
         operational_zone = str_replace_all(operational_zone, " ", "_")) %>%
  left_join((dashboard_data %>%
               group_by(FO, operational_zone, Settlement) %>%
               summarise(number_surveys = n()))) 


sites_and_surveys %>%
  writexl::write_xlsx(., "outputs/sites_and_surveys_names_district.xlsx")
####### Shiny Dashboard





{
# UI
ui <- dashboardPage(
  dashboardHeader(title = "HSM Dec 24 Dashboard"),
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))))
  ,
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "OPZ Completion", status = "primary", solidHeader = TRUE, width = 6,
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
    reactable(OPZ_Completion, searchable = TRUE, bordered = TRUE)
  })
  
  # Download for OPZ Completion
  output$download_OPZ <- downloadHandler(
    filename = function() {
      paste("OPZ_Completion_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(OPZ_Completion, file)
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
