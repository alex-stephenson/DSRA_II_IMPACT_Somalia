
###########################################################
########################## Setup ##########################
###########################################################

rm(list = ls())

options(scipen = 999)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, hypegrammaR, rio, readxl, openxlsx)

source(r"(C:\Users\alex.stephenson\ACTED\IMPACT SOM - 02_Research\01_REACH\2023_24\03_DDSU\SOM2401_DSRA\03_Data\03_Data\DSRA\functions/05_Results Table - Support Functions.R)")

# write the aggregation file with a timestamp to more easily keep track of different versions
date_time_now <- format(Sys.time(), "%a_%b_%d_%Y_%H%M%S")

##############################################################################
########################## Load the Data and Survey ##########################
##############################################################################

# read in the kobo tool survey and choices sheets
kobo_tool_name <- r"(C:\Users\alex.stephenson\ACTED\IMPACT SOM - 01_REACH\2023_24\03_DDSU\SOM2401_DSRA\03_Data/03_Data/DSRA/tool/dsra_tool.xlsx)"
questions <- read_excel(kobo_tool_name, "survey")
# choices <- read_excel(kobo_tool_name, "choices")

# load datasets for processing
file_path <- r"(C:\Users\alex.stephenson\ACTED\IMPACT SOM - 01_REACH\2023_24\03_DDSU\SOM2401_DSRA\03_Data/03_Data/DSRA/REACH2401_DSRA_DATA&ANALYSIS_HQ REVIEW.xlsx)"
main_data <- read_excel(file_path, 'Clean_Data')
hh_roster <- read_excel(file_path, 'Clean_Data_hh_roster')

################################################################################
############################# Narrow our data down #############################
################################################################################

#########################Making sure numerical variables are formatted correctly#########################################
num_q <- questions %>% filter(name %in% c("hh_size")) %>% pull(name)

num_q <- num_q[num_q %in% names(main_data)]

main_data <- mutate_at(main_data,num_q,as.numeric)

# Concatenating 'region' and 'idp' columns into a new column 'region_idp_combined'
main_data$localisation_region_idp_code_combined <- paste0(main_data$localisation_region, main_data$idp_code)

# Print the first few rows to verify the changes
head(main_data)
# Concatenating 'region' and 'host_community' columns into a new column 'region_host_community_combined'
main_data$localisation_region_idp_host_community_combined <- paste0(main_data$localisation_region, main_data$idp_host_community)

# Print the first few rows to verify the changes
head(main_data)

#####################Data collection was in idp sites as well as the respective district capitals, and this sections is basically 
##to create an extra column for classifing the settlements and idps in one column
main_data$idp_host_community_grouped<-ifelse(main_data$idp_host_community=="host_community" ,paste0(main_data$localisation_district_label,"_city"),
                                             ifelse(main_data$idp_host_community=="idp_site",paste0(main_data$idp_code),NA))



names(main_data)[names(main_data) == "_uuid"] <- "uuid" 

###############Rename the mogadishu districts for the sake of adding weights

main_data <- main_data %>%
  mutate(idp_host_community_grouped = case_when(idp_host_community_grouped == "Dayniile_city" ~ "Mogadishu_city" ,
                                                idp_host_community_grouped == "Khada_city" ~ "Mogadishu_city" ,
                                                    TRUE ~ as.character(idp_host_community_grouped)))



###################################apply weights##################################################################
sampling.frame <- read.csv(r"(C:\Users\alex.stephenson\ACTED\IMPACT SOM - 02_Research\01_REACH\2023_24\03_DDSU\SOM2401_DSRA\03_Data/03_Data/DSRA/sample_frame/sampling_frame.csv)", stringsAsFactors = FALSE)

weighting_function <-map_to_weighting(sampling.frame = sampling.frame,
                                      data.stratum.column = "idp_host_community_grouped",
                                      sampling.frame.population.column = "population",
                                      sampling.frame.stratum.column = "idp_code",
                                      data = main_data)
main_data[["weights"]] <-  weighting_function(main_data)



####################Adding weights and other relevant information to the hh_roster loop to for the purpose of analysis####################
hh_roster$localisation_district_label <- main_data$localisation_district_label[match(hh_roster$parent_instance_name, main_data$instance_name)]#district
hh_roster$weights <- main_data$weights[match(hh_roster$parent_instance_name, main_data$instance_name)]#weights
hh_roster$localisation_region_label <- main_data$localisation_region_label[match(hh_roster$parent_instance_name, main_data$instance_name)]#district_pop
hh_roster$idp_host_community_grouped <- main_data$idp_host_community_grouped[match(hh_roster$parent_instance_name, main_data$instance_name)]#idp_host_community_grouped





##############################Export csv files for analysis############################
write.csv(main_data,"fo_analysis/main_data.csv")

write.csv(hh_roster,"fo_analysis/hh_roster.csv")
                                                     
###############################################################################################
################################ Prepare and Write the Output #################################
###############################################################################################

questions <- read_xlsx(kobo_tool_name,
                       guess_max = 50000,
                       na = c("NA","#N/A",""," "),
                       sheet = 1) %>% 
                       filter(!is.na(name)) %>% 
                       mutate(q.type = as.character(lapply(type, function(x) str_split(x, " ")[[1]][1])),
                              list_name = as.character(lapply(type, function(x) str_split(x, " ")[[1]][2])),
                              list_name = ifelse(str_starts(type, "select_"), list_name, NA))

choices <- read_xlsx(kobo_tool_name,
                     guess_max = 50000,
                     na = c("NA","#N/A",""," "),
                     sheet = 2)

############################Load data for analysis##################
main_data <- read.csv("fo_analysis/main_data.csv")
hh_roster <- read.csv("fo_analysis/hh_roster.csv")

res <- generate_results_table(data = hh_roster,
                              questions = questions,
                              choices = choices,
                              weights.column = "weights",
                              use_labels = T,
                              labels_column = "label::English (en)",
                              "idp_host_community_grouped","localisation_district_label","localisation_region_label")

# add this to the path if you're running >= 2 surveys: more_than_1_settlement
export_table(res, "fo_analysis/")

