
## create output for validation of security

sites_189 <- readxl::read_excel(r"(0_DSSI_scoring/DSRA_Agreed_189_Sites.xlsx)")
sites_20 <- readxl::read_excel(r"(0_DSSI_scoring/DSRA_Agreed_189_Sites.xlsx)", sheet = 2)

buffer_site_security <- DSRA_II_scores_minus_DSRA_1 %>%
  filter(!idp_code %in% (sites_189 %>% pull(idp_code)),
         !idp_code %in% (sites_20 %>% pull(idp_code)),
         District %in% c('Baidoa', 'Gaalkacyo', 'Kismaayo', 'Mogadishu Dayniile', 'Mogadishu Khada'),
         DSRA_2_only == "Only in DSRA 2",
         total == 21) %>%
  { set.seed(123); slice_sample(., n = 30) }

security_output <- rbind(sites_189 %>% mutate(group = "Core 189"), sites_20 %>% mutate(group = "Buffer 50"), buffer_site_security %>% mutate(group = "Buffer 50"))


#security_output %>%
#  writexl::write_xlsx(., "0_DSSI_scoring/sites_for_security_review.xlsx")


## Update the coordinates based on the DSA site locations. The aim here is to take the enumerator location from the DSA if its tighly clustered.
## if not I'll take the DSA location.

raw_kobo_data <- ImpactFunctions::get_kobo_data(asset_id = "asSj6Aq8kDg5FULShCKbjN", un = "abdirahmanaia")
clean_data <- readxl::read_excel(r"(C:\Users\alex.stephenson\Downloads/SOM2204_DSA_Vlll_2025_Clean_Data.xlsx)")

raw_clean <- raw_kobo_data %>%
  filter(`_uuid` %in% (clean_data %>% pull(`_uuid`)))

raw_clean_geo <- raw_clean %>%
  select(idp_code, localisation_site, district_name, geopoint_latitude, geopoint_longitude) %>%
  filter(idp_code %in% (security_output %>% pull(idp_code)))

duplicate_pcode <- raw_clean_geo %>% distinct(idp_code, localisation_site) %>% count(idp_code) %>% filter(n > 1) %>% pull(idp_code)

# Calculate centroid and distance from centroid
raw_clean_geo <- raw_clean_geo %>%
  group_by(idp_code) %>%
  mutate(
    centroid_lat = mean(geopoint_latitude, na.rm = TRUE),
    centroid_lon = mean(geopoint_longitude, na.rm = TRUE),
    distance_meters = distHaversine(
      matrix(c(geopoint_longitude, geopoint_latitude), ncol = 2),
      matrix(c(centroid_lon, centroid_lat), ncol = 2)
    )
  ) %>%
  ungroup()

## sites where I'm happy to use the centroid location:

centroid_loc <- raw_clean_geo %>%
  group_by(idp_code, centroid_lat, centroid_lon) %>%
  summarise(avg_distance_sites_to_centroid = mean(distance_meters)) %>%
  ungroup() %>%
  filter(avg_distance_sites_to_centroid < 100) %>%
  arrange(desc(avg_distance_sites_to_centroid)) %>%
  select(idp_code, Latitude = centroid_lat, Longitude = centroid_lon)


## sites where I'll take the CCCM data

high_centroid_loc <- raw_clean_geo %>%
  group_by(idp_code, centroid_lat, centroid_lon) %>%
  summarise(avg_distance_sites_to_centroid = mean(distance_meters)) %>%
  ungroup() %>%
  filter(avg_distance_sites_to_centroid > 100) %>%
  arrange(desc(avg_distance_sites_to_centroid)) %>%
  left_join(idp_site_list %>% select(idp_code = `CCCM IDP Site Code`, Longitude, Latitude)) %>%
  select(idp_code, Longitude, Latitude)


site_locations <- rbind(centroid_loc, high_centroid_loc)

site_locations %>% 
  writexl::write_xlsx(., "0_DSSI_scoring/239_site_locations.xlsx")

