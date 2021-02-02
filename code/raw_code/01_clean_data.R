
  ## IMPORT DATA ----------------------------------------------------------------------------------------------------------------
  # Import Bus Stop Census Survey Responses
  raw_df <- read.csv(here("data", "raw_data", "survey", "2021-01-01_raw_bus_stop_census_responses.csv"))
  
  # Import Corrections Sheet
  corrections_df <- read_sheet("https://docs.google.com/spreadsheets/d/1n8RErNhOXrNptVyul5rTuE-ZTdSzkljB0cCAV8hBJkw/edit?usp=sharing",
                               sheet = "Corrections by Record ID",
                               col_names = TRUE,
                               col_types = "icccccc",
                               na = "") 
  
  # Import Missing Question Data
  missing_questions_populated_df <- read_sheet("https://docs.google.com/spreadsheets/d/1n8RErNhOXrNptVyul5rTuE-ZTdSzkljB0cCAV8hBJkw/edit?usp=sharing",
                                    sheet = "Missing Question Records",
                                    col_names = TRUE,
                                    na = "") %>%
                                    select(Record_ID, Sidewalk_New, Obstacles_New, Obstacle_Desc_New, Boarding_Area_New, Crosswalk_Features_New)
  
  # Import Duplicate Records Data
  duplicate_records_reconciled_df <- read_sheet("https://docs.google.com/spreadsheets/d/1n8RErNhOXrNptVyul5rTuE-ZTdSzkljB0cCAV8hBJkw/edit?usp=sharing",
                                                sheet = "Duplicate Record by Stop ID",
                                                col_names = TRUE,
                                                na = "") %>%
                                     select(Record_ID, Seating_Reconciled, Shelter_Reconciled, Trash_Can_Reconciled,
                                            Wayfinding_Reconciled, Wayfinding_Accessibility_Reconciled, Sidewalk_Reconciled,
                                            Boarding_Area_Reconciled, Crosswalks_Reconciled, Crosswalk_Features_Reconciled)
  
  # Merge 'Another.HUGE.inconvenience' into 'Additional_Comments' field and remove
  raw_df_clean <- raw_df %>% mutate(Additional_Comments = 
                                      ifelse(is.na(Another.HUGE.inconvenience), Additional_Comments,
                                             paste(Additional_Comments, Another.HUGE.inconvenience, sep = " "))) %>% # Merge into Additional_Comments field if populated with text
                             select(-Another.HUGE.inconvenience) # Remove field
  
  # Clean Obstacle Column
  raw_df_clean <- raw_df_clean %>% mutate(Obstacle_Desc_Clean = ifelse(is.na(Obstacles), Obstacle_Desc,   # Create a new Obstacle_Desc field that populates with non Yes/No answers from Obstacle field            
                                                                       ifelse(Obstacles %in% c("Yes", "No"), Obstacle_Desc, Obstacles)),
                                          Obstacle_Desc = Obstacle_Desc_Clean,
                                          Obstacles = ifelse(is.na(Obstacles) | Obstacles == "Yes", Obstacles, # Set Obstacle field to Yes if Obstacle_Desc is not NULL, No if it is NULL, and the same if field is already NULL or Yes
                                                             ifelse(!is.na(Obstacle_Desc), "Yes", "No"))) %>%
                                   select(-Obstacle_Desc_Clean) %>%
                                   mutate(Obstacle_Desc = ifelse(is.na(Obstacle_Desc), "", Obstacle_Desc))

  # Remove trailing columns
  raw_df_clean <- raw_df_clean %>% select(-Resent.From, -Curbside.Boarding, -To, -Date, -Subject, -Cc, -X.From) # Removes unneeded columns
  
  # Replace NAs in columns to help with removing duplicates
  raw_df_clean <- raw_df_clean %>% mutate(Additional_Comments = replace_na(Additional_Comments, ""),
                                          Crosswalk_Features = replace_na(Crosswalk_Features, ""))  #ifelse(!is.na(Timestamp), replace_na(Crosswalk_Features, ""), Crosswalk_Features))
  
  # Clean data of identical records
  raw_df_clean_deduped <- raw_df_clean %>% distinct_at(vars(-Record_ID, -Timestamp), .keep_all = TRUE)
  
  # Convert Timestamp To R Readable Format
  raw_df_clean_deduped$Timestamp_Sub <- substring(raw_df_clean_deduped$Timestamp, 0, 24) # Take substring of timestamp to reformat
  raw_df_clean_deduped$Timestamp_Sub <- as.POSIXct(raw_df_clean_deduped$Timestamp_Sub, format = "%a %b %d %Y %H:%M:%S") # Convert to POSIXct
  raw_df_clean_deduped <- raw_df_clean_deduped %>% mutate(Timestamp = Timestamp_Sub) %>%
                                                   select(-Timestamp_Sub)
  

  ## INTEGRATE SURVEYOR SUBMITTED CORRECTIONS -----------------------------------------------------------------------------------
   raw_df_clean_deduped <- correctRecords(raw_df_clean_deduped, corrections_df, "Surveyor Submitted Correction")
  
  ## CLEAN STOP ID FIELD --------------------------------------------------------------------------------------------------------
  raw_df_clean_deduped_gtfs_stops_join <- raw_df_clean_deduped %>% left_join(final_stop_list, # Join Survey Responses and GTFS Data
                                                                             by = c("Stop_ID" = "stop_id")) 
  no_stop_id_df <- raw_df_clean_deduped_gtfs_stops_join %>% dplyr::filter(is.na(Main_Street_or_Station),
                                                                          !grepl("streetcar", Routes.x, ignore.case = TRUE)) # Get list of Stop_IDs not in GTFS Data
  # write.csv(no_stop_id_df, here("data", "raw_data", "validation", "No Stop ID Survey Records.csv"), row.names = FALSE) # Export records for examination
  
  raw_df_clean_deduped <- correctRecords(raw_df_clean_deduped, corrections_df, "No Stop ID Record")
  
  ## FILL IN MISSING QUESTION DATA ----------------------------------------------------------------------------------------------
  # Create data.frame of records with missing data for Sidewalk, Obstacle, Obstacle_Desc, Boarding_Area, and Crosswalk_Features questions
  'Apporximately XX days after the start of data collection, four new questions were added:
  '
  missing_questions_df <- raw_df_clean_deduped %>% dplyr::filter(is.na(Obstacles)) # Filter for all records where Obstacle question is NULL (this means this response was before the question was created and required)
  #write.csv(missing_questions_df, here("data", "raw_data", "validation", "Missing Question Data Records.csv"), row.names = FALSE) # Export missing data records for MARTA Army team to populate
  
  raw_df_clean_deduped <- raw_df_clean_deduped %>% left_join(missing_questions_populated_df, by = c("Record_ID")) %>%
                                                   mutate(Sidewalk = ifelse(!is.na(Sidewalk_New), Sidewalk_New, Sidewalk),
                                                          Obstacles = ifelse(!is.na(Obstacles_New), Obstacles_New, Obstacles),
                                                          Obstacle_Desc = ifelse(!is.na(Obstacle_Desc_New), Obstacle_Desc_New, Obstacle_Desc),
                                                          Boarding_Area = ifelse(!is.na(Boarding_Area_New), Boarding_Area_New, Boarding_Area),
                                                          Crosswalk_Features = ifelse(!is.na(Crosswalk_Features_New), Crosswalk_Features_New, Crosswalk_Features)) %>%
                                                   select(-Sidewalk_New, -Obstacles_New, -Obstacle_Desc_New, -Boarding_Area_New, -Crosswalk_Features_New)
  
  raw_df_clean_deduped <- correctRecords(raw_df_clean_deduped, corrections_df, "Missing Question Records")
  
  # Collect Duplicate Records by Stop_ID
  stop_id_duplicates <- raw_df_clean_deduped %>% select(Stop_ID) %>%
                                                 group_by(Stop_ID) %>%
                                                 summarize(count = n()) %>%
                                                 dplyr::filter(count > 1) %>%
                                                 ungroup() %>%
                                                 select(Stop_ID)
  
  
  # Remove Duplicates of same record with different Cleanliness and Timestamp field entries
  raw_df_clean_deduped <- raw_df_clean_deduped %>% arrange(Stop_ID, Timestamp) %>%
                                                   mutate(Duplicate = ifelse(Stop_ID == dplyr::lag(Stop_ID) &
                                                                             Email_Masked == dplyr::lag(Email_Masked) &
                                                                             Main_Street == dplyr::lag(Main_Street) &
                                                                             Nearest_Landmark == dplyr::lag(Nearest_Landmark) &
                                                                             Routes == dplyr::lag(Routes) &
                                                                             Direction == dplyr::lag(Direction) &
                                                                             Seating == dplyr::lag(Seating) &
                                                                             Shelter == dplyr::lag(Shelter) &
                                                                             Trash_Can == dplyr::lag(Trash_Can) &
                                                                             Line_of_Sight == dplyr::lag(Line_of_Sight) &
                                                                             Wayfinding == dplyr::lag(Wayfinding) &
                                                                             Wayfinding_Accessibility == dplyr::lag(Wayfinding_Accessibility) &
                                                                             Lighting == dplyr::lag(Lighting) &
                                                                             Sidewalk == dplyr::lag(Sidewalk) &
                                                                             Obstacles == dplyr::lag(Obstacles) &
                                                                             Obstacle_Desc == dplyr::lag(Obstacle_Desc) &
                                                                             Boarding_Area == dplyr::lag(Boarding_Area) &
                                                                             Crosswalks == dplyr::lag(Crosswalks) &
                                                                             Crosswalk_Features == dplyr::lag(Crosswalk_Features) &
                                                                             Behavior == dplyr::lag(Behavior) &
                                                                             On_Site_Survey == dplyr::lag(On_Site_Survey) &
                                                                             Additional_Comments == dplyr::lag(Additional_Comments) &
                                                                             is.na(Timestamp) & is.na(Cleanliness), "Duplicate", ""
                                                                             ))
  
  # Create data.frame of only Stop_ID duplicates
  duplicates_by_stop_id_df <- raw_df_clean_deduped %>% inner_join(stop_id_duplicates, by = c("Stop_ID")) %>%
                              arrange(Stop_ID, Timestamp, Email_Masked)
  
  duplicates_by_fields <- duplicates_by_stop_id_df %>% 
                                                       distinct_at(vars(-Record_ID, -Timestamp, -Email_Masked,
                                                                        -Main_Street, -Nearest_Landmark, -Routes,
                                                                        -Direction, -Cleanliness, -Line_of_Sight, 
                                                                        -Lighting, -Obstacles, -Obstacle_Desc,
                                                                        -Behavior, -On_Site_Survey, -Additional_Comments), .keep_all = TRUE) %>%
                                                       arrange(Stop_ID, Timestamp, Email_Masked) %>%
                                                       mutate(Duplicate = ifelse((Stop_ID == dplyr::lag(Stop_ID) | Stop_ID == dplyr::lead(Stop_ID)),
                                                                                   "Duplicate","")) %>%
                                                       dplyr::filter(Duplicate != "") %>%
                                                       select(-Duplicate)
  
  
  
  
  #write.csv(duplicates_by_fields, here("data", "raw_data", "validation", "Duplicate Records by Stop ID.csv"), row.names = FALSE)
  
  raw_df_clean_deduped <- raw_df_clean_deduped %>% 
                            left_join(duplicate_records_reconciled_df, by = c("Record_ID")) %>%
                            mutate(Seating = ifelse(!is.na(Seating_Reconciled), Seating_Reconciled, Seating),
                                   Shelter = ifelse(!is.na(Shelter_Reconciled), Shelter_Reconciled, Shelter),
                                   Trash_Can = ifelse(!is.na(Trash_Can_Reconciled), Trash_Can_Reconciled, Trash_Can),
                                   Wayfinding = ifelse(!is.na(Wayfinding_Reconciled), Wayfinding_Reconciled, Wayfinding),
                                   Wayfinding_Accessibility = ifelse(!is.na(Wayfinding_Accessibility_Reconciled), Wayfinding_Accessibility_Reconciled, Wayfinding_Accessibility),
                                   Sidewalk = ifelse(!is.na(Sidewalk_Reconciled), Sidewalk_Reconciled, Sidewalk),
                                   Boarding_Area = ifelse(!is.na(Boarding_Area_Reconciled), Boarding_Area_Reconciled, Boarding_Area),
                                   Crosswalks = ifelse(!is.na(Crosswalks_Reconciled), Crosswalks_Reconciled, Crosswalks),
                                   Crosswalk_Features = ifelse(!is.na(Crosswalk_Features_Reconciled), Crosswalk_Features_Reconciled, Crosswalk_Features)) %>%
                            select(-Seating_Reconciled, -Shelter_Reconciled, -Trash_Can_Reconciled,
                                   -Wayfinding_Reconciled, -Wayfinding_Accessibility_Reconciled, -Sidewalk_Reconciled,
                                   -Boarding_Area_Reconciled, -Crosswalks_Reconciled, -Crosswalk_Features_Reconciled)
  
  raw_df_clean_deduped <- correctRecords(raw_df_clean_deduped, corrections_df, "Duplicate Records") %>%
                          mutate(Crosswalk_Features = ifelse(Crosswalk_Features == "NA" | is.na(Crosswalk_Features), "", Crosswalk_Features))
  
  
  write.csv(duplicates_by_fields,"Remaining Duplicates.csv")
  

  
  
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
  
  # Contradictory Behavior Answers
  behavior_verification <- raw_df_clean_deduped %>% mutate(Verify = ifelse((Firsit_Visit == "Yes" | Regular_User_None == "Yes") &  Cross_Street + Worn_Faded) > 0 & No_Crosswalk == 1, 1, 0))
  
  
  'The second mutate normalizes the Main_Street and Nearest_Landmark names by setting the fields equal to the stop name listed in the GTFS data.'
  dplyr::filter(!is.na(main_street_or_station)) %>% #Remove remaining records from response data without a Stop ID.
    mutate(Main_Street = main_street_or_station, # Normalize Main_Street field using GTFS stop name
           Nearest_Landmark = cross_street) %>% # Normalize Nearest_Landmark field using GTFS stop name
    select(-main_street_or_station, -cross_street) # Remove helper columns
  
  #Data Output
  # write.csv(, file = here("data", "tidy_data", "pre_processed_survey_df.csv"))
  
            