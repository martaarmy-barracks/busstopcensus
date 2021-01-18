
  # Import Bus Stop Census Survey Responses
  raw_df <- read.csv(here("data", "raw_data", "survey", "2021-01-01_raw_bus_stop_census_responses.csv"))

  # Merge 'Another.HUGE.inconvenience' into 'Additional_Comments' field and remove
  raw_df_clean <- raw_df %>% mutate(Additional_Comments = ifelse(is.na(Another.HUGE.inconvenience), Additional_Comments,
                                             paste(Additional_Comments, Another.HUGE.inconvenience, sep = " "))) %>% # Merge into Additional_Comments field
                             select(-Another.HUGE.inconvenience) # Remove field
  
  # Clean Obstacle Column
  raw_df_clean <- raw_df_clean %>% mutate(Obstacle_Desc_Clean = ifelse(is.na(Obstacles), Obstacle_Desc,   # Create a new Obstacle_Desc field that populates with non Yes/No answers from Obstacle field            
                                                                       ifelse(Obstacles %in% c("Yes", "No"), Obstacle_Desc, Obstacles)),
                                          Obstacle_Desc = Obstacle_Desc_Clean,
                                          Obstacles = ifelse(is.na(Obstacles) | Obstacles == "Yes", Obstacles, # Set Obstacle field to Yes if Obstacle_Desc is not NULL, No if it is NULL, and the same if field is already NULL or Yes
                                                             ifelse(!is.na(Obstacle_Desc), "Yes", "No"))) %>%
                                   select(-Obstacle_Desc_Clean)

  # Remove trailing columns
  raw_df_clean <- raw_df_clean %>% select(-Resent.From, -To, -Date, -Subject, -Cc, -X.From) # Removes unneeded columns
  
  # Replace NAs in columns to help with removing duplicates
  raw_df_clean <- raw_df_clean %>% mutate(Additional_Comments = replace_na(Additional_Comments, ""),
                                          Crosswalk_Features = ifelse(!is.na(Timestamp), replace_na(Crosswalk_Features, ""), Crosswalk_Features))
  
  # Clean data of identical records
  raw_df_clean_deduped <- raw_df_clean %>% distinct_at(vars(-Record_ID, -Timestamp), .keep_all = TRUE)
  
  # Convert Timestamp To R Readable Format
  raw_df_clean_deduped$Timestamp_Sub <- substring(raw_df_clean_deduped$Timestamp, 0, 24) # Take substring of timestamp to reformat
  raw_df_clean_deduped$Timestamp_Sub <- as.POSIXct(raw_df_clean_deduped$Timestamp_Sub, format = "%a %b %d %Y %H:%M:%S") # Convert to POSIXct
  raw_df_clean_deduped <- raw_df_clean_deduped %>% mutate(Timestamp = Timestamp_Sub) %>%
                                                   select(-Timestamp_Sub)
  
  # Collect GTFS Stop Data to Compare to Survey Responses
  stop_comparison_data <- stops %>% full_join(post_pandemic_stops, by = c("stop_id")) %>%                                # Join pre-pandemic stop data with end of 2020 stop data
                                             # mutate(name_diff = ifelse(stop_name.x != stop_name.y, "FLAG", ""),
                                             #         lat_diff = ifelse(stop_lat.x != stop_lat.y, "FLAG", ""),
                                             #         lon_diff = ifelse(stop_lon.x != stop_lon.y, "FLAG", "")) %>%
                                              mutate(stop_name = ifelse(is.na(stop_name.x), stop_name.y, stop_name.x),
                                                     stop_lat = ifelse(is.na(stop_lat.x), stop_lat.y, stop_lat.x),
                                                     stop_lon = ifelse(is.na(stop_lon.x), stop_lon.y, stop_lon.x)) %>%
                                              select(stop_id, stop_name) %>%
                                    separate(stop_name, c("main_street_or_station", "cross_street"), sep = "@") %>%
                                    mutate(main_street_or_station = trimws(main_street_or_station),
                                           cross_street = trimws(cross_street))
  
  no_stop_id_full_join <- raw_df_clean_deduped %>% left_join(stop_comparison_data, # Join Survey Responses and GTFS Data
                                                                    by = c("Stop_ID" = "stop_id")) 
  no_stop_id_df <- no_stop_id_full_join %>% dplyr::filter(is.na(main_street_or_station)) # Get list of Stop_IDs not in GTFS Data
  write.csv(no_stop_id_df, here("data", "raw_data", "validation", "No Stop ID Survey Records.csv"), row.names = FALSE) # Export records for examination
  
  # Add Stop IDs to records with missing Stop IDs
  'These Stop_IDs were manually identified using the data exported to the "No Stop ID Survey Records.csv" file and the Bus Stop Census map. Any records where the 
  Stop ID could not be identified are filtered out of the dataset. The results of the manul identification are contained in the "Bus Stop Census Corrections Log" 
  spreadhseet. In total, there were 17 responses with no identified Stop ID. Stop IDs were identified for five of these records, leaving 12 records without
  Stop IDs remaining. These records are removed from the response data because they could not be linked to a bus stop.'
  raw_df_clean_deduped <- no_stop_id_full_join %>% mutate(Stop_ID = case_when(Record_ID == 42 ~ "212715",
                                                                              Record_ID == 1000 ~ "900948",
                                                                              Record_ID == 3032 ~ "903549",
                                                                              Record_ID == 3720 ~ "901906",
                                                                              Record_ID == 7345 ~ "120024",
                                                                              TRUE              ~ Stop_ID)) %>%
                                                   dplyr::filter(!is.na(main_street_or_station)) %>% #Remove remaining records from response data without a Stop ID.
                                                   mutate(Main_Street = main_street_or_station, # Normalize Main_Street field using GTFS stop name
                                                          Nearest_Landmark = cross_street) %>% # Normalize Nearest_Landmark field using GTFS stop name
                                                   select(-main_street_or_station, -cross_street) # Remove helper columns
  
  # Collect Duplicate Records by Stop_ID
  stop_id_duplicates <- raw_df_clean_deduped %>% select(Stop_ID) %>%
                                                   group_by(Stop_ID) %>%
                                                   summarize(count = n()) %>%
                                                   dplyr::filter(count > 1) %>%
                                                   ungroup() %>%
                                                   select(Stop_ID)
  
  # Create data.frame of only Stop_ID duplicates
  duplicates_by_stop_id_df <- raw_df_clean_deduped %>% inner_join(stop_id_duplicates, by = c("Stop_ID"))
  
  non_duplicates <- raw_df_clean_deduped %>% anti_join(stop_id_duplicates, by = c("Stop_ID"))                                                     
  
  # Create data.frame of records with missing data for Sidewalk, Obstacle, Obstacle_Desc, Boarding_Area, and Crosswalk_Features questions
  'Apporximately XX days after the start of data collection, four new questions were added:
  '
  missing_questions_df <- raw_df_clean_deduped %>% dplyr::filter(is.na(Obstacles)) # Filter for all records where Obstacle question is NULL (this means this response was before the question was created and required)
  write.csv(missing_questions_df, here("data", "raw_data", "validation", "Missing Question Data Records.csv"), row.names = FALSE) # Export missing data records for MARTA Army team to populate
  
  
  #Data Output
  # write.csv(, file = here("data", "tidy_data", "pre_processed_survey_df.csv"))
  
            