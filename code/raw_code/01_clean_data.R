
  # Import Bus Stop Census Survey Responses
  raw_df <- read.csv(here("data", "raw_data", "survey", "2021-01-01_raw_bus_stop_census_responses.csv"))

  # Clean data of identical records
  raw_df_deduped <- raw_df %>% distinct_at(vars(-Record_ID), .keep_all = TRUE)
  
  # Merge 'Another.HUGE.inconvenience' into 'Additional_Comments' field and remove
  raw_df_deduped_clean <- raw_df_deduped %>% mutate(Additional_Comments = 
                                                        ifelse(is.na(Another.HUGE.inconvenience), Additional_Comments,
                                                               paste(Additional_Comments, Another.HUGE.inconvenience, sep = " "))) %>%
                                             select(-Another.HUGE.inconvenience)
  
  # Remove trailing columns
  raw_df_deduped_clean <- raw_df_deduped_clean %>% select(-To, -Date, -Subject, -Cc, -X.From)
  
  # Convert Timestamp To R Readable Format
  raw_df_deduped_clean$Timestamp_Sub <- substring(raw_df_deduped_clean$Timestamp, 0, 24) # Take substring of timestamp to reformat
  raw_df_deduped_clean$Timestamp_Sub <- as.POSIXct(raw_df_deduped_clean$Timestamp_Sub, format = "%a %b %d %Y %H:%M:%S") # Convert to POSIXct
  raw_df_deduped_clean <- raw_df_deduped_clean %>% mutate(Timestamp = Timestamp_Sub) %>%
                                                   select(-Timestamp_Sub)
  
  # Collect GTFS Stop Data to Compare to Survey Responses
  stop_comparison_data <- stops %>% full_join(post_pandemic_stops, by = c("stop_id")) %>%                                # Join pre-pandemic stop data with end of 2020 stop data
                                             # mutate(name_diff = ifelse(stop_name.x != stop_name.y, "FLAG", ""),
                                             #         lat_diff = ifelse(stop_lat.x != stop_lat.y, "FLAG", ""),
                                             #         lon_diff = ifelse(stop_lon.x != stop_lon.y, "FLAG", "")) %>%
                                              mutate(stop_code = ifelse(is.na(stop_code.x), stop_code.y, stop_code.x),
                                                     stop_name = ifelse(is.na(stop_name.x), stop_name.y, stop_name.x),
                                                     stop_lat = ifelse(is.na(stop_lat.x), stop_lat.y, stop_lat.x),
                                                     stop_lon = ifelse(is.na(stop_lon.x), stop_lon.y, stop_lon.x)) %>%
                                              select(stop_id, stop_code, stop_name, stop_lat, stop_lon) %>%
                                    separate(stop_name, c("main_street_or_station", "cross_street"), sep = "@")
  
  raw_df_deduped_clean_joined <- raw_df_deduped_clean %>% left_join(stop_comparison_data, # Join Survey Responses and GTFS Data
                                                                    by = c("Stop_ID" = "stop_id")) 
  
  no_stop_id_df <- raw_df_deduped_clean_joined %>% dplyr::filter(is.na(main_street_or_station)) # Get list of Stop_IDs not in GTFS Data
  
  write.csv(no_stop_id_df, here("data", "raw_data", "validation", "No Stop ID Survey Records.csv"), row.names = FALSE) # Export records for examination
  
  # Collect Duplicate Records by Stop_ID
  
  #Data Output
  write.csv(survey_df, file = here("data", "tidy_data", "pre_processed_survey_df.csv"))
  
            