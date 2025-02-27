library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)

## This script takes the DSA data and scoring and calculates which sites should be included in the DSRA. 

rm(list = ls())
set.seed(123)


## function
calculate_sample_size <- function(confidence, margin_error, p = 0.5, population) {
  # Compute Z-score for the given confidence level
  z <- qnorm(1 - (1 - confidence) / 2)
  
  # Calculate initial sample size assuming infinite population
  n <- (z^2 * p * (1 - p)) / (margin_error^2)
  
  # Apply finite population correction for each household size
  n_adj <- n / (1 + (n - 1) / population)
  
  return(ceiling(n_adj))  # Round up to nearest whole number
}


data <- read_csv(r"(input/DSA_aggregated_data.csv)")
scoring <- read_excel(r"(input/DS critical indicator scoring_UPDATED.xlsx)", sheet = "Sheet2") 

idp_site_list <- readxl::read_excel(r"(input/idp-site-master-list-sep-2024.xlsx)", sheet = "CCCM IDP Site List (Verified)")


target_districts <- c("Doolow", "Afmadow", "Baardheere", "Kismaayo", "Baidoa", "Xudur", "Diinsoor", "Mogadishu Khada", "Mogadishu Dayniile", "Jowhar", "Belet Weyne", "Gaalkacyo", 
                      "Bossaso", "Garoowe", "Cadaado", "Cabudwaaq")


data <- data %>%
  filter(district_name %in% target_districts)

## need to hard code the school questions - if an answer has ANY of these in the select multiple then it should not score.


  edu_barriers <- c("closed",
                    "security_concerns",
                    "costs",
                    "help_at_home",
                    "work_outside_home",
                    "no_aware_education_opportunities",
                    "parents_no_value_edu",
                    "parents_no_approve_curric",
                    "cultural_beliefs",
                    "child_pycho_distress",
                    "displacement_conflict",
                    "child_lack_documentation",
                    "flood",
                    "child_recruited_ag",
                    "marriage_pregnant",
                    "language",
                    "poor_infrastructure",
                    "no_wash_at_school",
                    "other")
## if any of these are detected then nullify the whole answer so it'll score 0
  
data <- data %>%
  mutate(education_barriers_boys = if_else(
    str_detect(education_barriers_boys, str_c(edu_barriers, collapse = "|")),
    NA_character_,
    education_barriers_boys
  ),
  education_barriers_girls = if_else(
    str_detect(education_barriers_girls, str_c(edu_barriers, collapse = "|")),
    NA_character_,
    education_barriers_girls))
  


# Get column names that need scoring
colnames <- scoring %>%
  select(Variable) %>% 
  filter(!Variable %in% c("sanitation_toilets_male", "shelter_types")) %>% ##remove these 2 as we handle them differently. 
  distinct() %>%
  pull()

##combine the health barrier 
filtered_data <- data %>% 
  mutate(health_barriers = paste(health_barriers_men, health_barriers_women, health_barriers_children)) %>%
  select(idp_code, all_of(colnames)) 


## calculate numerical columns
numeric_data <- data %>% 
  select(idp_code, cccm_populationestimates_individuals, sanitation_toilets_total, solid_apartment) %>%
    mutate(people_per_toilet = (cccm_populationestimates_individuals / sanitation_toilets_total),
         sanitation_toilets_score = 
           case_when(people_per_toilet < 20 ~ 1,
                     FALSE ~ 0),
         shelter_types =
           case_when(solid_apartment > 50 ~ 1,
                     solid_apartment < 50 ~ 0)) %>%
  select(idp_code, sanitation_toilets_score, shelter_types)


# Convert scoring reference table
scoring <- scoring %>%
  mutate(Score = ifelse(str_detect(Type, "1$"), 1, 0)) %>%
  select(Variable, Value, Score) %>%
  filter(Score == 1)


# Reshape data into long format
long_data <- filtered_data %>%
  pivot_longer(cols = all_of(colnames), names_to = "Variable", values_to = "Response")

# Assign scores, handling select multiple
scored_data <- long_data %>%
  rowwise() %>%
  mutate(Score = ifelse(
    any(str_detect(Response, fixed(scoring$Value[scoring$Variable == Variable]))),
    1, 0
  )) %>%
  ungroup()

# Reshape back to wide format
final_scored_data <- scored_data %>%
  select(idp_code, Variable, Score) %>%
  pivot_wider(names_from = Variable, values_from = Score, values_fill = 0) %>%
  left_join(numeric_data, by = "idp_code")

# Calculate total score
score_and_total_data <- final_scored_data %>%
  rowwise() %>%
  mutate(total = sum(c_across(-idp_code), na.rm = T)) %>%
  ungroup()


#Sites meeting scoring threshold - this is all the sites from DSA 2 which meet 22 or above score
score_and_total_data_district <- score_and_total_data %>% 
  left_join(data %>% select(idp_code, district_name)) %>%
  filter(total >= 22) %>%
  select(idp_code, district_name, everything())


### CALCULATE HOW MANY SITES IN DSA VIII FROM DSRA 1
DSRA_1 <- read_excel(r"(input/DS critical indicator scoring_UPDATED.xlsx)", sheet = "DSRA 1 Sites")
 
DSRA_1_Sites <- DSRA_1 %>%  
   pull(Sites)
  
#Filter DSA VIII data to only keep sites from DSRA 1
dsra_1_match <- data %>%
  filter(idp_code %in% DSRA_1_Sites) %>%
  filter(district_name %in% target_districts) %>%
  pull(idp_code)

# take the scores for these DSRA 1 sites
sites_dsra_1 <- score_and_total_data %>%
  filter(idp_code %in% dsra_1_match) %>%
  left_join(data %>% select(idp_code, district_name)) %>%
  select(idp_code, district_name, everything())


## bind all together

sites_scoring_and_dsra1 <- rbind(sites_dsra_1, score_and_total_data_district) %>%
  distinct() ## this will be our DSSI output

## now onto the sampling

sites_and_catchment <- sites_scoring_and_dsra1 %>%
  left_join(idp_site_list %>% select(idp_code = `CCCM IDP Site Code`, Neighbourhood, HH_size =`HH (Q1-2024)`)) %>%
  select(idp_code, HH_size, district_name, Neighbourhood, everything())

## sampling based on no catchment area
idp_score_hh <- sites_and_catchment %>%
  mutate(sample_size = calculate_sample_size(0.9, 0.1, population = HH_size)) %>%
  select(idp_code, HH_size, sample_size)

df_output <- list("Sites DSRA 2" = sites_and_catchment, "HH Sampling DSRA 2" = idp_score_hh, "DSRA 1 Sites" = DSRA_1)


df_output %>%
  writexl::write_xlsx(., 'DSRA_2_DSSI_27_02_2025.xlsx')