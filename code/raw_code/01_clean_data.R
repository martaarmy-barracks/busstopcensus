  
  ###################### INTRODUCTION TO THE CLEAN DATA SCRIPT ######################### 
  # A table of all cleaning tasks performed, their purpose, and their location are contained within the Bus Stop Corrections Log at the following linK:
  # https://docs.google.com/spreadsheets/d/1n8RErNhOXrNptVyul5rTuE-ZTdSzkljB0cCAV8hBJkw/edit?usp=sharing
  #
  # The Bus Stop Corrections Log is where the MARTA Army Team performed a number of manual cleaning tasks.
  #
  # The log is also where all corrections identified during manual reviews are recorded for transparency.
  #
  # The code below is labeled with comments that provide clarification and correlate the code to a cleaning task
  # listed in the "Data Cleaning Tasks" sheet in the Bus Stop Census Corrections Log.  


  ###################### IMPORT DATA ######################### 
  raw_df <- read.csv(here("data", "raw_data", "survey", "2021-01-01_raw_bus_stop_census_responses.csv")) # Import Bus Stop Census Survey Responses
  
  # The following imports use the googlesheets4 package to import data from the Bus Stop Corrections Log.
  # You must have a Google Account to use the package as it will ask you to authorize the package to access your Google Sheets.
  # There is a limit on how many reads can be made through the Google Sheets API. 
  # Import Corrections Sheet
  corrections_df <- read_sheet(
                         ss        = "https://docs.google.com/spreadsheets/d/1n8RErNhOXrNptVyul5rTuE-ZTdSzkljB0cCAV8hBJkw/edit?usp=sharing",
                         sheet     = "Corrections by Record ID",
                         col_names = TRUE,
                         col_types = "icccccc",
                         na        = ""
                    ) 
  
  # Import Missing Question Data
  missing_questions_populated_df <- read_sheet(
                                          ss = "https://docs.google.com/spreadsheets/d/1n8RErNhOXrNptVyul5rTuE-ZTdSzkljB0cCAV8hBJkw/edit?usp=sharing",
                                          sheet = "Missing Question Records",
                                          col_names = TRUE,
                                          na = "") %>%
                                    select(Record_ID, Sidewalk_New, Obstacles_New, 
                                           Obstacle_Desc_New, Boarding_Area_New, Crosswalk_Features_New) # Selects only the fields that were filled in
  
  # Import Duplicate Records By Stop ID and Fields Data
  duplicate_records_reconciled_df <- read_sheet("https://docs.google.com/spreadsheets/d/1n8RErNhOXrNptVyul5rTuE-ZTdSzkljB0cCAV8hBJkw/edit?usp=sharing",
                                                sheet = "Duplicate Record by Stop ID and Fields",
                                                col_names = TRUE,
                                                na = "") %>%
                                     select(Record_ID, Seating_Reconciled, Shelter_Reconciled, Trash_Can_Reconciled, # Selects only the fields that were reconciled
                                            Wayfinding_Reconciled, Wayfinding_Accessibility_Reconciled, Sidewalk_Reconciled,
                                            Boarding_Area_Reconciled, Crosswalks_Reconciled, Crosswalk_Features_Reconciled)
  
  ###################### PERFORM INITIAL CLEANING TASKS TO PREPARE DATA FOR FURTHER PROCESSING ######################### 
  
  # Merge 'Another.HUGE.inconvenience' into 'Additional_Comments' field and remove | Data Cleaning Task #1
  raw_df_clean <- raw_df %>% mutate(
                                Additional_Comments = ifelse(is.na(Another.HUGE.inconvenience), Additional_Comments,
                                             paste(Additional_Comments, Another.HUGE.inconvenience, sep = " "))) %>% # Merge into Additional_Comments field if populated with text
                             select(-Another.HUGE.inconvenience)
  
  # Clean Obstacle Column | Data Cleaning Tasks #2 & #3
  raw_df_clean <- raw_df_clean %>% mutate(Obstacle_Desc_Clean = ifelse(is.na(Obstacles), Obstacle_Desc,   # Create a new Obstacle_Desc field that populates with non Yes/No answers from Obstacle field            
                                                                       ifelse(Obstacles %in% c("Yes", "No"), Obstacle_Desc, Obstacles)),
                                          Obstacle_Desc = Obstacle_Desc_Clean,
                                          Obstacles = ifelse(is.na(Obstacles) | Obstacles == "Yes", Obstacles, # Set Obstacle field to Yes if Obstacle_Desc is not NULL, No if it is NULL, and the same if field is already NULL or Yes
                                                             ifelse(!is.na(Obstacle_Desc), "Yes", "No"))) %>%
                                   select(-Obstacle_Desc_Clean) %>% 
                                   mutate(Obstacle_Desc = ifelse(is.na(Obstacle_Desc), "", Obstacle_Desc))

  # Remove trailing fields # Data Cleaning Task #4
  raw_df_clean <- raw_df_clean %>% 
                    select(-Resent.From, -Curbside.Boarding, -To, -Date, -Subject, -Cc, -X.From) # Removes unneeded columns
  
  # Replace NAs in fields to help with removing duplicates | Data Cleaning Task #5
  raw_df_clean <- raw_df_clean %>% 
                      mutate(Email_Masked        = replace_na(Email_Masked, ""),
                             Crosswalk_Features  = replace_na(Crosswalk_Features, ""),
                             Additional_Comments = replace_na(Additional_Comments, "")) 
  
  # Clean data of identical records | Data Cleaning Task #6
  raw_df_clean_deduped <- raw_df_clean %>% 
                              distinct_at(vars(-Record_ID, -Timestamp), .keep_all = TRUE)
  
  # Convert Timestamp To R Readable Format | Data Cleaning Task #7
  raw_df_clean_deduped$Timestamp_Sub <- substring(raw_df_clean_deduped$Timestamp, 0, 24) # Take substring of timestamp to reformat
  raw_df_clean_deduped$Timestamp_Sub <- as.POSIXct(raw_df_clean_deduped$Timestamp_Sub, format = "%a %b %d %Y %H:%M:%S") # Convert to POSIXct
  raw_df_clean_deduped <- raw_df_clean_deduped %>% 
                              mutate(Timestamp = Timestamp_Sub) %>% # Set original timestamp value to converted timestamp
                              select(-Timestamp_Sub) # Remove timestamp helper field created for the conversion
  
  rm(raw_df)
  ######################### INTEGRATE SURVEYOR SUBMITTED CORRECTIONS #########################
  # Use correctRecords function to apply corrections to response data | Data Cleaning Task #8
  raw_df_clean_deduped <- correctRecords(raw_df_clean_deduped, corrections_df, "Surveyor Submitted Correction")
  
  ######################### CHECK FOR VALID STOP ID ######################### 
  # Not all surveys contained a Stop ID that was listed in the GTFS data. This code identifies those stops and 
  # exports them to a csv for further review | Data Cleaning Task #9
  
  raw_df_clean_deduped_gtfs_stops_join <- raw_df_clean_deduped %>% 
                                              left_join(final_stop_list, by = c("Stop_ID" = "stop_id")) # Join Survey Responses and GTFS Data
  
  no_stop_id_df <- raw_df_clean_deduped_gtfs_stops_join %>% 
                     dplyr::filter(is.na(Main_Street_or_Station), !grepl("streetcar", Routes_Final, ignore.case = TRUE)) # Get list of Stop_IDs not in GTFS Data
  # write.csv(no_stop_id_df, here("data", "raw_data", "validation", "No Stop ID Survey Records.csv"), row.names = FALSE) # Export records for examination
  rm(raw_df_clean_deduped_gtfs_stops_join)
  
  raw_df_clean_deduped <- correctRecords(
                              survey_df      = raw_df_clean_deduped, 
                              corrections_df = corrections_df, 
                              source_sheet   = "No Stop ID Record"
                          )
  
  ######################### FILL IN MISSING QUESTION DATA #########################
  # Questions corresponding to Sidewalk, Obstacle, Obstacles_Desc, Boarding_Area, and Crosswalk_Features were added shortly after
  # the Bus Stop Census survey was launched. This resulted in over 100 surveys being submitted without answers for these fields.
  # The following code identifies these records and exports them to a csv for the answers to be filled in. | Data Cleaning Task #10
  
  # Create data.frame of records with missing data for Sidewalk, Obstacle, Obstacle_Desc, Boarding_Area, and Crosswalk_Features questions
  'Apporximately XX days after the start of data collection, four new questions were added:
  '
  missing_questions_df <- raw_df_clean_deduped %>% 
                            dplyr::filter(is.na(Obstacles)) # Filter for all records where Obstacle question is NULL (this means this response was before the question was created and required)
  #write.csv(missing_questions_df, here("data", "raw_data", "validation", "Missing Question Data Records.csv"), row.names = FALSE) # Export missing data records for MARTA Army team to populate
  
  raw_df_clean_deduped <- raw_df_clean_deduped %>% 
                            left_join(missing_questions_populated_df, by = c("Record_ID")) %>%
                            mutate(Sidewalk = ifelse(!is.na(Sidewalk_New), Sidewalk_New, Sidewalk),
                                   Obstacles = ifelse(!is.na(Obstacles_New), Obstacles_New, Obstacles),
                                   Obstacle_Desc = ifelse(!is.na(Obstacle_Desc_New), Obstacle_Desc_New, Obstacle_Desc),
                                   Boarding_Area = ifelse(!is.na(Boarding_Area_New), Boarding_Area_New, Boarding_Area),
                                   Crosswalk_Features = ifelse(!is.na(Crosswalk_Features_New), Crosswalk_Features_New, Crosswalk_Features)) %>%
                            select(-Sidewalk_New, -Obstacles_New, -Obstacle_Desc_New, -Boarding_Area_New, -Crosswalk_Features_New)
  
  # Apply corrections from the Missing Questions Data re-populating
  raw_df_clean_deduped <- correctRecords(
                              survey_df      = raw_df_clean_deduped, 
                              corrections_df = corrections_df, 
                              source_sheet   = "Missing Question Records"
                          )
  
  ######################### REMOVE SPECIAL DUPLICATES #########################
  
  # Remove Duplicates of same record with different Cleanliness and Timestamp field entries | Data Cleaning Task #11
  raw_df_clean_deduped <- raw_df_clean_deduped %>% 
                            arrange(Stop_ID, Email_Masked, Timestamp) %>%
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
                                   is.na(Timestamp) & is.na(Cleanliness), "Duplicate", "")) %>%
                            dplyr::filter(Duplicate == "") %>%
                            select(-Duplicate)
  
  # Data Cleaning Task #12
  duplicates_by_fields <- raw_df_clean_deduped %>% 
                            distinct_at(vars(-Record_ID, -Timestamp, -Email_Masked,
                                             -Main_Street, -Nearest_Landmark, -Routes,
                                             -Direction, -Cleanliness, -Line_of_Sight, 
                                             -Lighting, -Obstacles, -Obstacle_Desc,
                                             -Behavior, -On_Site_Survey, -Additional_Comments), .keep_all = TRUE) %>%
                            arrange(Stop_ID, Timestamp, Email_Masked) %>%
                            mutate(Duplicate = ifelse((Stop_ID == dplyr::lag(Stop_ID) | Stop_ID == dplyr::lead(Stop_ID)), "Duplicate","")) %>%
                            dplyr::filter(Duplicate != "") %>%
                            select(-Duplicate)

  
  #write.csv(duplicates_by_fields, here("data", "raw_data", "validation", "Duplicate Records by Stop ID Round 2.csv"), row.names = FALSE)
  
  # The following code block takes the manually inputted fields from the "Duplicate Records by Stop ID and Fields" sheet and assigns them to the
  # corresponding field in the original data set.
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
  
  # Apply corrections from the Duplicate Records by Stop ID and Fields reconciliation
  raw_df_clean_deduped <- correctRecords(raw_df_clean_deduped, corrections_df, "Duplicate Records") %>%
                          mutate(Crosswalk_Features = ifelse(Crosswalk_Features == "NA" | is.na(Crosswalk_Features), "", Crosswalk_Features)) # convert string NA to be empty string
  
  # After running this code, the duplicates_by_fields data frame was rerun again to identify any more duplicates that are remaining after the reconciled
  # data and corrections were inputted. These duplicates were exported to their own file then copied into the "Duplicates by Stop ID" sheet
  # in the Bus Stop Census Corrections Workbook
  
  # Remove Duplicates By Stop ID and Surveyor | Data Cleaning Task #13 
  # Identify Duplicate Records by Stop_ID and Surveyor
  surveyor_duplicates <- raw_df_clean_deduped %>% select(Stop_ID, Email_Masked) %>% 
    group_by(Stop_ID, Email_Masked) %>%
    summarize(count = n()) %>%  # Counts the amount of records for each Stop_ID and Email_Masked group
    dplyr::filter(count > 1) %>% # Filters out groups with only had one record 
    ungroup() %>%
    select(Stop_ID, Email_Masked) # Outputs list of Stop_ID and Email_Masked pair with more than one record in the data
  
  # Create data.frame of only Stop_ID and Surveyor duplicates
  duplicates_by_surveyor_df <- raw_df_clean_deduped %>% inner_join(surveyor_duplicates, by = c("Stop_ID","Email_Masked")) %>% # Returns all records that represent a surveyor duplicate
    arrange(Stop_ID, Timestamp, Email_Masked) %>%
    dplyr::filter(Email_Masked != "")
  
  # Export Duplicate Records By Stop ID and Surveyor to validation folder
  #write.csv(duplicates_by_surveyor_df, here("data", "raw_data", "validation", "Duplicate Records by Surveyor.csv"), row.names = FALSE)
  
  # Apply corrections from the Duplicate Records by Stop ID and Surveyor reconciliation
  raw_df_clean_deduped <- correctRecords(raw_df_clean_deduped, corrections_df, "Surveyor Duplicate Records")
  
  rm(surveyor_duplicates)
  rm(duplicates_by_surveyor_df)
  ###################### SPREAD DATA FOR SELECT ALL THAT APPLY RESPONSES ######################### 
  
  # Separate Select All That Apply Question Fields With Multiple Answers Into One Field Per Answer | Data Cleaning Task #14
  raw_df_clean_deduped <- raw_df_clean_deduped %>% 
                              mutate(Litter = ifelse(grepl("Litter at the stop", Cleanliness, fixed = TRUE), "Yes", "No"),
                                     Grafitti = ifelse(grepl("Graffiti (unauthorized) or tagging on bus stop amenities",Cleanliness, fixed = TRUE), "Yes", "No"),
                                     Overflow = ifelse(grepl("Overflowing or poorly maintained trash can", Cleanliness, fixed = TRUE), "Yes","No"),
                                     Dirty_Seating = ifelse(grepl("Dirty seating area", Cleanliness, fixed = TRUE), "Yes", "No"),
                                     Other = ifelse(grepl("Other", Cleanliness, fixed = TRUE), "Yes", "No")) %>%
                              mutate(Route_Number = ifelse(grepl("Route Numbers", Wayfinding, fixed = TRUE), "Yes", "No"),
                                     Route_Schedule = ifelse(grepl("Route Schedule", Wayfinding, fixed = TRUE), "Yes", "No"),
                                     Route_Map = ifelse(grepl("Route Map", Wayfinding, fixed = TRUE), "Yes","No"),
                                     Customer_Service = ifelse(grepl("Customer Service Information", Wayfinding, fixed = TRUE), "Yes", "No"),
                                     None_Of_The_Above = ifelse(grepl("None of the Above", Wayfinding, fixed = TRUE), "Yes", "No")) %>%
                              mutate(Main_Street_Crosswalk = ifelse(grepl("Yes, on the main street", Crosswalks, fixed = TRUE), "Yes", "No"),
                                     Cross_Street_Crosswalk = ifelse(grepl("Yes, on the cross street",Crosswalks, fixed = TRUE), "Yes", "No"),
                                     Worn_Faded = ifelse(grepl("Yes, and crosswalk paint is faded or worn away", Crosswalks, fixed = TRUE), "Yes","No"),
                                     No_Crosswalk = ifelse(grepl("No, no painted crosswalk within 100 feet", Crosswalks, fixed = TRUE), "Yes", "No")) %>%
                              mutate(Traffic_Light = ifelse(grepl("Traffic light", Crosswalk_Features, fixed = TRUE), "Yes", "No"),
                                     Curb_Cuts = ifelse(grepl("Curb cuts for wheelchairs",Crosswalk_Features, fixed = TRUE), "Yes", "No"),
                                     Crosswalk_Signals = ifelse(grepl("Crosswalk signals with push buttons", Crosswalk_Features, fixed = TRUE), "Yes","No"),
                                     Crossing_Audio = ifelse(grepl("Crossing audio overlays for the visually impaired", Crosswalk_Features, fixed = TRUE), "Yes", "No"),
                                     Tactile_Guide = ifelse(grepl("Tactile guide strips for the visually impaired", Crosswalk_Features, fixed = TRUE), "Yes", "No")) %>%
                              mutate(Informal_Pathways = ifelse(grepl("Pedestrians using informal pathways where sidewalks do not exist", Behavior, fixed = TRUE), "Yes", "No"),
                                     Compete_For_Seat = ifelse(grepl("Pedestrians competing for seating at the bus stop",Behavior, fixed = TRUE), "Yes", "No"),
                                     Cross_Midblock = ifelse(grepl("Pedestrians crossing the roadway at midblock locations", Behavior, fixed = TRUE), "Yes","No"),
                                     Catch_The_Bus = ifelse(grepl("Pedestrians running across roadways to catch the bus", Behavior, fixed = TRUE), "Yes", "No"),
                                     Dangerous_Motorists = ifelse(grepl("Dangerous motorist behavior around bus stop (i.e. speeding or not yielding to pedestrians)", Behavior, fixed = TRUE), "Yes", "No"),
                                     First_Visit = ifelse(grepl("None of the above (first visit to this stop)", Behavior, fixed = TRUE), "Yes", "No"),
                                     Regular_User_None = ifelse(grepl("None of the above (occasional or frequent user of this stop", Behavior, fixed = TRUE), "Yes", "No")) %>%
                              select(Record_ID, Stop_ID, Timestamp, Email_Masked, Main_Street, Nearest_Landmark, Routes, Direction,
                                     Seating, Shelter, Trash_Can, Litter, Grafitti, Overflow, Dirty_Seating, Other, Line_of_Sight,
                                     Route_Number, Route_Schedule, Route_Map, Customer_Service, None_Of_The_Above, Wayfinding_Accessibility,
                                     Lighting, Sidewalk, Obstacles, Obstacle_Desc, Boarding_Area, Main_Street_Crosswalk, Cross_Street_Crosswalk, 
                                     Worn_Faded, No_Crosswalk, Traffic_Light, Curb_Cuts, Crosswalk_Signals, Crossing_Audio, Tactile_Guide, Informal_Pathways,
                                     Compete_For_Seat, Cross_Midblock, Catch_The_Bus, Dangerous_Motorists, First_Visit, Regular_User_None,
                                     On_Site_Survey, Additional_Comments)
  
  ###################### SPREAD DATA FOR SELECT ALL THAT APPLY RESPONSES ######################### 
  
  # Clean Trash_Can and Overflow Contradictory Answers | Data Cleaning Task #15
  trash_can_verification <- raw_df_clean_deduped %>% 
                                mutate(Flag = ifelse(Trash_Can == "No" & Overflow == "Yes", "FLAG", "")) %>%
                                dplyr::filter(Flag == "FLAG")
  #write.csv(trash_can_verification, here("data", "raw_data", "validation", "Trash_Can and Overflow Responses.csv"), row.names = FALSE)
  
  # Identify and Export Records where the surveyor selected one or more wayfinding information present 
  # and in Wayfinding Accessibility selected "No, no wayfinding information present" | Data Cleaning Task #16
  wayfinding_verification <- raw_df_clean_deduped %>%
                                mutate(Flag = ifelse(str_count(paste(Route_Number, Route_Schedule, Route_Map, sep = ""), "Yes") > 0 &
                                                     Wayfinding_Accessibility == "No wayfinding information present",
                                                   "FLAG", "")) %>%
                                dplyr::filter(Flag == "FLAG")
  #write.csv(wayfinding_verification, here("data", "raw_data", "validation", "Wayfinding and No Wayfinding Responses.csv"), row.names = FALSE)
  
  # Identify and Export Records where the options for Main Street, Cross Street, or Worn or Faded Crosswalk are selected along 
  # with the "No, there is no crosswalk within 100 feet" selection | Data Cleaning Task #17
  crosswalk_verification_pt_one <- raw_df_clean_deduped %>% 
                                    mutate(Flag = ifelse(str_count(paste(Main_Street_Crosswalk, Cross_Street_Crosswalk,  Worn_Faded, sep = ""), "Yes") > 0 & No_Crosswalk == "Yes",
                                                         "FLAG", "")) %>%
                                    dplyr::filter(Flag == "FLAG")
  #write.csv(crosswalk_verification_pt_one, here("data", "raw_data", "validation", "Crosswalk and No Crosswalk Responses.csv"), row.names = FALSE)
  
  # Identify and Export Records where the surveyor only checked "Yes, and the crosswalk paint is worn or faded" | Data Cleaning Task #18
  crosswalk_verification_pt_two <- raw_df_clean_deduped %>% 
                                    mutate(Flag = ifelse(str_count(paste(Main_Street_Crosswalk, Cross_Street_Crosswalk, Worn_Faded, sep = ""), "Yes") == 1 & Worn_Faded == "Yes", "FLAG", "")) %>%
                                    dplyr::filter(Flag == "FLAG")
  #write.csv(crosswalk_verification_pt_two, here("data", "raw_data", "validation", "Only Worn or Faded Crosswalk Response.csv"), row.names = FALSE)
  
  # Identify and Export Records where the surveyor checked "No, there is no seating" and checked "Dirty Seating Area" as a cleanliness issue
  # Data Cleaning Task #19
  seating_verification <- raw_df_clean_deduped %>%
                              mutate(Flag = ifelse(Seating == "No, there is no seating" & Dirty_Seating == "Yes", "FLAG", "")) %>%
                              dplyr::filter(Flag == "FLAG")
  #write.csv(seating_verification, here("data", "raw_data", "validation", "No Seating and Dirty Seating Response.csv"), row.names = FALSE)
  
  
  # Apply Contradictory Answer Corrections to Records
  raw_df_clean_deduped <- correctRecords(raw_df_clean_deduped, corrections_df, "Contradictory Responses")
  
  rm(trash_can_verification)
  rm(wayfinding_verification)
  rm(crosswalk_verification_pt_one)
  rm(crosswalk_verification_pt_two)
  rm(seating_verification)
  
  ###################### DATA MODIFICATIONS BASED ON MARTA ARMY ASSUMPTIONS #########################
  
  # Clean Contradictory Behavior Answers | Data Cleaning Task #20
  raw_df_clean_deduped <- raw_df_clean_deduped %>% 
                              mutate(First_Visit = ifelse(First_Visit         == "Yes"  &  
                                                         (Informal_Pathways   == "Yes" | 
                                                          Compete_For_Seat    == "Yes" |
                                                          Cross_Midblock      == "Yes" | 
                                                          Catch_The_Bus       == "Yes" |
                                                          Dangerous_Motorists == "Yes"), "No", First_Visit),
                                     Regular_User_None = ifelse(Regular_User_None   == "Yes"  &  
                                                               (Informal_Pathways   == "Yes" | 
                                                                Compete_For_Seat    == "Yes" |
                                                                Cross_Midblock      == "Yes" | 
                                                                Catch_The_Bus       == "Yes" |
                                                                Dangerous_Motorists == "Yes"), "No", Regular_User_None))
  
  # Remove "None of the Above" response to the Wayfinding Question | Data Cleaning Task #21
  'Because all MARTA bus stop markers have the Customer Service Number print on them, we have made the decision to remove the "None of the Above" 
  option from all records and to make "Customer Service Information" the default for each record'
  raw_df_clean_deduped <- raw_df_clean_deduped %>% 
                              mutate(Customer_Service = "Yes",
                                     None_Of_The_Above = "No")
  
  # Modify records to show "No wayfinding information present" when only Customer Service Information is selected for Wayfinding information present
  # Data Cleaning Task #22
  raw_df_clean_deduped <- raw_df_clean_deduped %>% 
                              mutate(Wayfinding_Accessibility = 
                                       ifelse(Route_Number == "No" & Route_Schedule == "No" & Route_Map == "No" & Customer_Service == "Yes",
                                              "No wayfinding information present", Wayfinding_Accessibility))
  
  # Change responses for Wayfinding Accessibility to "No" if the response shows a shelter present | Data Cleaning Task #23
  raw_df_clean_deduped <- raw_df_clean_deduped %>%
                            mutate(Wayfinding_Accessibility = ifelse(Shelter == "Yes" & 
                                                                     Seating == "Yes, there is seating provided by MARTA or another transit agency" &
                                                                     (Route_Number == "Yes" | Route_Schedule == "Yes" | Route_Map == "Yes"),
                                                                     "No", Wayfinding_Accessibility))
 
  # Normalize Main Street and Cross Street Names, Add Routes, Add Direction, and Add Lat & Lon | Data Cleaning Task #24, #25, #26, and #27
  raw_df_clean_deduped <- raw_df_clean_deduped %>% 
                                  inner_join(final_stop_list, by = c("Stop_ID" = "stop_id")) %>%
                                  mutate(Routes = Routes_Final,
                                         Direction = Direction_Final,
                                         Nearest_Landmark = Cross_Street) 
                                                               
  
  ###################### ADD CITY AND COUNTY DATA TO STOPS ######################
  raw_df_clean_deduped_copy <- raw_df_clean_deduped
  
  # The following code is adapted from this online example: https://towardsdatascience.com/reverse-geocoding-in-r-f7fe4b908355
  # Step 1: Create a blank dataframe to store results.
  data_final = data.frame()
  
  # Step 2: Create a while loop to have the function running until the # dataframe with 100,000 rows is empty.
  while (nrow(raw_df_clean_deduped_copy)>0) {
    # Step 3: Subset the data even further so that you are sending only # a small portion of requests to the Photon server.
    if (nrow(raw_df_clean_deduped_copy) >= 200) {
      subset <- raw_df_clean_deduped_copy[1:200,]
    } else {
      rows <- as.integer(nrow(raw_df_clean_deduped_copy))
      subset <- raw_df_clean_deduped_copy[1:rows,]
    }
    
    # Step 4: Extracting the lat/longs from the subsetted data from
    # the previous step (Step 3).
    latlong <- subset %>% 
      select(Record_ID, Stop_Lat, Stop_Lon) %>% 
      mutate(index=row_number())
    latlong <- data.frame(latlong)
    
    # Step 5: Incorporate the revgeo package here. I left_joined the 
    # output with the latlong dataframe from the previous step to add 
    # the latitude/longitude information with the reverse geocoded data.
    cities <- revgeo(latlong$Stop_Lon, latlong$Stop_Lat, provider =  'photon', output = 'frame') %>% 
      mutate(index = row_number()) %>%
      select(index, city) %>% 
      rename(City = city) %>%
      left_join(latlong, by="index") %>% 
      select(-index)
  
    # Removing the latlong dataframe because I no longer need it. This 
    # helps with reducing memory in my global environment.
    rm(latlong)
  
    # Step 6: Adding the information from the cities dataframe to 
    # subset dataframe (from Step 3).
  
    data_new <- subset %>% 
      left_join(cities, by=c("Record_ID", "Stop_Lat", "Stop_Lon"))
  
  
    # Step 7: Adding data_new into the empty data_all dataframe where 
    # all subsetted reverse geocoded data will be combined.
  
    data_final <- rbind(data_final,data_new)
  
    # Step 8: Remove the rows that were used in the first loop from the # main_sub frame so the next 200 rows can be read into the while # loop.
  
    raw_df_clean_deduped_copy <- anti_join(raw_df_clean_deduped_copy, subset, by=c("Record_ID"))
    print(nrow(raw_df_clean_deduped_copy))
  
    # Remove dataframes that are not needed before the while loop closes # to free up space.
    rm(data_new)
    rm(cities)
  
    print('Sleeping for 10 seconds')
    Sys.sleep(10)
  
  }

  # Reverse geocode coordinates to get county the bus stops are located in
  county <- data.frame(County = map.where(database="county", 
                    data_final$Stop_Lon, data_final$Stop_Lat)) %>%
            mutate(County = case_when(
              County == "georgia,fulton" ~ "Fulton",
              County == "georgia,de kalb" ~ "Dekalb",
              County == "georgia,clayton" ~ "Clayton",
              County == "georgia,douglas" ~ "Douglas",
              County == "georgia,gwinnett" ~ "Gwinnett",
              County == "georgia,cobb" ~ "Cobb",
              TRUE ~ ""))
  
  # Add county vector to the full survey dataframe and select columns for final output dataframe
  raw_df_clean_deduped_geo <- cbind(data_final, county) %>%
    select(Record_ID, Stop_ID, Stop_Lat, Stop_Lon, Timestamp, Email_Masked, Main_Street_or_Station, Nearest_Landmark, 
           City, County, Routes, Direction, Seating, Shelter, Trash_Can, Litter, Grafitti, Overflow, Dirty_Seating, 
           Other, Line_of_Sight, Route_Number, Route_Schedule, Route_Map, Customer_Service, None_Of_The_Above, Wayfinding_Accessibility,
           Lighting, Sidewalk, Obstacles, Obstacle_Desc, Boarding_Area, Main_Street_Crosswalk, Cross_Street_Crosswalk, 
           Worn_Faded, No_Crosswalk, Traffic_Light, Curb_Cuts, Crosswalk_Signals, Crossing_Audio, Tactile_Guide, Informal_Pathways,
           Compete_For_Seat, Cross_Midblock, Catch_The_Bus, Dangerous_Motorists, First_Visit, Regular_User_None,
           On_Site_Survey, Additional_Comments) %>%
    mutate( City = replace(City, City == "Arlanta", "Atlanta"),
            City = replace(City, City == "chamblee", "Chamblee"))
  
  
  ###################### EXPORT PREPROCESSED DATA ###################### 
  write.csv(raw_df_clean_deduped_geo, file = here("data", "tidy_data", "pre_processed_survey_df.csv"), row.names = FALSE)
  
            