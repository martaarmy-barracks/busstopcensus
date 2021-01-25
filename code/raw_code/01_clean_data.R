
  # Import Bus Stop Census Survey Responses
  raw_df <- read.csv(here("data", "raw_data", "survey", "2021-01-01_raw_bus_stop_census_responses.csv"))
  
  # Merge 'Another.HUGE.inconvenience' into 'Additional_Comments' field and remove
  raw_df_clean <- raw_df %>% mutate(Additional_Comments = ifelse(is.na(Another.HUGE.inconvenience), Additional_Comments,
                                             paste(Additional_Comments, Another.HUGE.inconvenience, sep = " "))) %>% # Merge into Additional_Comments field if populated with text
                             select(-Another.HUGE.inconvenience) # Remove field
  
  # Clean Obstacle Column
  raw_df_clean <- raw_df_clean %>% mutate(Obstacle_Desc_Clean = ifelse(is.na(Obstacles), Obstacle_Desc,   # Create a new Obstacle_Desc field that populates with non Yes/No answers from Obstacle field            
                                                                       ifelse(Obstacles %in% c("Yes", "No"), Obstacle_Desc, Obstacles)),
                                          Obstacle_Desc = Obstacle_Desc_Clean,
                                          Obstacles = ifelse(is.na(Obstacles) | Obstacles == "Yes", Obstacles, # Set Obstacle field to Yes if Obstacle_Desc is not NULL, No if it is NULL, and the same if field is already NULL or Yes
                                                             ifelse(!is.na(Obstacle_Desc), "Yes", "No"))) %>%
                                   select(-Obstacle_Desc_Clean)

  # Remove trailing columns
  raw_df_clean <- raw_df_clean %>% select(-Resent.From, -Curbside.Boarding, -To, -Date, -Subject, -Cc, -X.From) # Removes unneeded columns
  
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
  
  raw_df_clean_deduped_gtfs_stops_join <- raw_df_clean_deduped %>% left_join(stop_comparison_data, # Join Survey Responses and GTFS Data
                                                                    by = c("Stop_ID" = "stop_id")) 
  no_stop_id_df <- raw_df_clean_deduped_gtfs_stops_join %>% dplyr::filter(is.na(main_street_or_station)) # Get list of Stop_IDs not in GTFS Data
  # write.csv(no_stop_id_df, here("data", "raw_data", "validation", "No Stop ID Survey Records.csv"), row.names = FALSE) # Export records for examination
  
  # Clean Stop ID Field
  'The first mutate function adds Stop IDs to records that were the Stop ID was missing but was manually identified by the MARTA Army review team.
  These Stop_IDs were manually identified using the data exported to the "No Stop ID Survey Records.csv" file and the Bus Stop Census map. Any records where the 
  Stop ID could not be identified are filtered out of the dataset. The results of the manul identification are contained in the "Bus Stop Census Corrections Log" 
  spreadhseet. In total, there were 17 responses with no identified Stop ID. Stop IDs were identified for XX of these records, leaving XX records without
  Stop IDs remaining. These records are removed from the response data because they could not be linked to a bus stop.
  
  The second mutate normalizes the Main_Street and Nearest_Landmark names by setting the fields equal to the stop name listed in the GTFS data.'
  raw_df_clean_deduped <- raw_df_clean_deduped_gtfs_stops_join %>% mutate(Stop_ID = case_when(
                                                                              Record_ID == 42 ~ "212715",
                                                                              Record_ID == 885 ~ "903344",
                                                                              Record_ID == 1000 ~ "900948",
                                                                              Record_ID == 1833 ~ "212540",
                                                                              Record_ID == 3032 ~ "903549",
                                                                              Record_ID == 3720 ~ "901906",
                                                                              Record_ID == 3807 ~ "903757",
                                                                              Record_ID == 5836 ~ "122084",
                                                                              Record_ID == 6004 ~ "903313",
                                                                              Record_ID == 7345 ~ "120024",
                                                                              TRUE              ~ Stop_ID)) %>%
                                                   dplyr::filter(!is.na(main_street_or_station)) %>% #Remove remaining records from response data without a Stop ID.
                                                   mutate(Main_Street = main_street_or_station, # Normalize Main_Street field using GTFS stop name
                                                          Nearest_Landmark = cross_street) %>% # Normalize Nearest_Landmark field using GTFS stop name
                                                   select(-main_street_or_station, -cross_street) # Remove helper columns
  
  
  # Create data.frame of records with missing data for Sidewalk, Obstacle, Obstacle_Desc, Boarding_Area, and Crosswalk_Features questions
  'Apporximately XX days after the start of data collection, four new questions were added:
  '
  missing_questions_df <- raw_df_clean_deduped %>% dplyr::filter(is.na(Obstacles)) # Filter for all records where Obstacle question is NULL (this means this response was before the question was created and required)
  #write.csv(missing_questions_df, here("data", "raw_data", "validation", "Missing Question Data Records.csv"), row.names = FALSE) # Export missing data records for MARTA Army team to populate
  
  # Collect Duplicate Records by Stop_ID
  stop_id_duplicates <- raw_df_clean_deduped %>% select(Stop_ID) %>%
                                                 group_by(Stop_ID) %>%
                                                 summarize(count = n()) %>%
                                                 dplyr::filter(count > 1) %>%
                                                 ungroup() %>%
                                                 select(Stop_ID)
  
  # Create data.frame of only Stop_ID duplicates
  duplicates_by_stop_id_df <- raw_df_clean_deduped %>% inner_join(stop_id_duplicates, by = c("Stop_ID")) %>%
                              arrange(Stop_ID, Timestamp, Email_Masked)
  
  duplicates_by_fields <- duplicates_by_stop_id_df %>% distinct_at(vars(-Record_ID, -Timestamp, -Email_Masked,
                                                                        -Main_Street, -Nearest_Landmark, -Routes,
                                                                        -Direction, -Cleanliness, -Line_of_Sight, 
                                                                        -Lighting, -Obstacles, -Obstacle_Desc,
                                                                        -Behavior, -On_Site_Survey, -Additional_Comments), .keep_all = TRUE) %>%
                                                       arrange(Stop_ID, Timestamp, Email_Masked) %>%
                                                       mutate(Duplicate = ifelse((Stop_ID == dplyr::lag(Stop_ID) | Stop_ID == dplyr::lead(Stop_ID)),
                                                                                   "Duplicate","")) %>%
                                                       dplyr::filter(Duplicate != "") %>%
                                                       select(-Duplicate)
  
  write.csv(duplicates_by_fields, here("data", "raw_data", "validation", "Duplicate Records by Stop ID.csv"), row.names = FALSE)
  
  non_duplicates <- raw_df_clean_deduped %>% anti_join(stop_id_duplicates, by = c("Stop_ID"))  
  

  
  
  # Separate Question Fields With Multiple Answers Into One Field Per Answer
  raw_df_clean_deduped <- raw_df_clean_deduped %>% mutate(Litter = ifelse(grepl("Litter at the stop", Cleanliness, fixed = TRUE), "Yes", "No"),
                                                          Grafitti = ifelse(grepl("Graffiti (unauthorized) or tagging on bus stop amenities",Cleanliness, fixed = TRUE), "Yes", "No"),
                                                          Overflow = ifelse(grepl("Overflowing or poorly maintained trash can", Cleanliness, fixed = TRUE), "Yes","No"),
                                                          Dirty_Seating = ifelse(grepl("Dirty seating area", Cleanliness, fixed = TRUE), "Yes", "No"),
                                                          Other = ifelse(grepl("Other", Cleanliness, fixed = TRUE), "Yes", "No")) %>%
                                                   mutate(Route_Number = ifelse(grepl("Route Numbers", Wayfinding, fixed = TRUE), 1, 0),
                                                          Route_Schedule = ifelse(grepl("Route Schedule", Wayfinding, fixed = TRUE), 1, 0),
                                                          Route_Map = ifelse(grepl("Route Map", Wayfinding, fixed = TRUE), 1,0),
                                                          Customer_Service = ifelse(grepl("Customer Service Information", Wayfinding, fixed = TRUE), 1, 0),
                                                          None_Of_The_Above = ifelse(grepl("None of the Above", Wayfinding, fixed = TRUE), 1, 0)) %>%
                                                   mutate(Main_Street = ifelse(grepl("Yes, on the main street", Crosswalks, fixed = TRUE), 1, 0),
                                                          Cross_Street = ifelse(grepl("Yes, on the cross street",Crosswalks, fixed = TRUE), 1, 0),
                                                          Worn_Faded = ifelse(grepl("Yes, and crosswalk paint is faded or worn away", Crosswalks, fixed = TRUE), 1,0),
                                                          No_Crosswalk = ifelse(grepl("No, no painted crosswalk within 100 feet", Crosswalks, fixed = TRUE), 1, 0)) %>%
                                                   mutate(Traffic_Light = ifelse(grepl("Traffic_Light", Crosswalk_Features, fixed = TRUE), 1, 0),
                                                          Curb_Cuts = ifelse(grepl("Curb cuts for wheelchairs",Crosswalk_Features, fixed = TRUE), 1, 0),
                                                          Crosswalk_Signals = ifelse(grepl("Crosswalk signals with push buttons", Crosswalk_Features, fixed = TRUE), 1,0),
                                                          Crossing_Audio = ifelse(grepl("Crossing audio overlays for the visually impaired", Crosswalk_Features, fixed = TRUE), 1, 0),
                                                          Tactile_Guide = ifelse(grepl("Tactile guide strips for the visually impaired", Crosswalk_Features, fixed = TRUE), 1, 0)) %>%
                                                   mutate(Informal_Pathways = ifelse(grepl("Pedestrians using informal pathways where sidewalks do not exist", Behavior, fixed = TRUE), 1, 0),
                                                          Compete_For_Seat = ifelse(grepl("Pedestrians competing for seating at the bus stop",Behavior, fixed = TRUE), 1, 0),
                                                          Cross_Midblock = ifelse(grepl("Pedestrians crossing the roadway at midblock locations", Behavior, fixed = TRUE), 1,0),
                                                          Catch_The_Bus = ifelse(grepl("Pedestrians running across roadways to catch the bus", Behavior, fixed = TRUE), 1, 0),
                                                          Dangerous_Motorists = ifelse(grepl("Dangerous motorist behavior around bus stop (i.e. speeding or not yielding to pedestrians)", Behavior, fixed = TRUE), 1, 0),
                                                          First_Visit = ifelse(grepl("None of the above (first visit to this stop)", Behavior, fixed = TRUE), 1, 0),
                                                          Regular_User_None = ifelse(grepl("None of the above (occasional or frequent user of this stop", Behavior, fixed = TRUE), 1, 0))
  
  
  # Clean Wayfinding Field
  'Because nearly all MARTA bus stop markers have the Customer Service Number print on them, we have made the decision to replace all responses
  with "None of the above" to "Customer Service Information"'
  raw_df_clean_deduped <- raw_df_clean_deduped %>% mutate(Wayfinding = ifelse(Wayfinding == "None of the Above", "Customer Service Information",
                                                                              Wayfinding))
  
  # Clean Wayfinding Accessibility Field
  raw_df_clean_deduped <- raw_df_clean_deduped %>% mutate(Wayfinding_Accessibility = ifelse(Wayfinding == "Customer Service Information", "No wayfinding information present",
                                                                                            Wayfinding_Accessibility))
  
  # Clean Crosswalk Field
  crosswalk_verification <- raw_df_clean_deduped %>% mutate(Verify = ifelse((Main_Street + Cross_Street + Worn_Faded) > 0 & No_Crosswalk == 1, 1, 0),
                                                            Verify_2 = ifelse((Main_Street + Cross_Street + Worn_Faded) == 1 & Worn_Faded == 1, 1, 0))
  
  
  
  #Data Output
  # write.csv(, file = here("data", "tidy_data", "pre_processed_survey_df.csv"))
  
            