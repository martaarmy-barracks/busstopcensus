
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
                                              mutate(stop_name = ifelse(is.na(stop_name.x), stop_name.y, stop_name.x),
                                                     stop_lat = ifelse(is.na(stop_lat.x), stop_lat.y, stop_lat.x),
                                                     stop_lon = ifelse(is.na(stop_lon.x), stop_lon.y, stop_lon.x)) %>%
                                              select(stop_id, stop_name, stop_lat, stop_lon) %>%
                                    separate(stop_name, c("main_street_or_station", "cross_street"), sep = "@")
  
  no_stop_id_full_join <- raw_df_deduped_clean %>% left_join(stop_comparison_data, # Join Survey Responses and GTFS Data
                                                                    by = c("Stop_ID" = "stop_id")) 
  no_stop_id_df <- no_stop_id_full_join %>% dplyr::filter(is.na(main_street_or_station)) # Get list of Stop_IDs not in GTFS Data
  write.csv(no_stop_id_df, here("data", "raw_data", "validation", "No Stop ID Survey Records.csv"), row.names = FALSE) # Export records for examination
  
  # Add Stop IDs to records with missing Stop IDs
  'These Stop_IDs were manually identified using the data exported to the "No Stop ID Survey Records.csv" file and the Bus Stop Census map. Any records where the 
  Stop ID could not be identified are filtered out of the dataset. The results of the manul identification are contained in the "Bus Stop Census Corrections Log" 
  spreadhseet. In total, there were 17 responses with no identified Stop ID. Stop IDs were identified for five of these records, leaving 12 records without
  Stop IDs remaining. These records are removed from the response data because they could not be linked to a bus stop.'
  raw_df_deduped_clean <- no_stop_id_full_join %>% mutate(Stop_ID = case_when(Record_ID == 42 ~ "212715",
                                                                              Record_ID == 1000 ~ "900948",
                                                                              Record_ID == 3032 ~ "903549",
                                                                              Record_ID == 3720 ~ "901906",
                                                                              Record_ID == 7345 ~ "120024",
                                                                              TRUE              ~ Stop_ID)) %>%
                                                   dplyr::filter(!is.na(main_street_or_station)) #Remove remaining records from response data without a Stop ID.
  
  
  # Collect Duplicate Records by Stop_ID
  stop_id_duplicates <- raw_df_deduped_clean %>% select(Stop_ID) %>%
                                                   group_by(Stop_ID) %>%
                                                   summarize(count = n()) %>%
                                                   dplyr::filter(count > 1) %>%
                                                   ungroup() %>%
                                                   select(Stop_ID)
  
  # Create data.frame of only Stop_ID duplicates
  duplicates_by_stop_id_df <- raw_df_deduped_clean %>% inner_join(stop_id_duplicates, by = c("Stop_ID")) %>%
                                                       
  
  
  #Data Output
  write.csv(survey_df, file = here("data", "tidy_data", "pre_processed_survey_df.csv"))
  
            