
  ## Import Bus Stop Census Survey Responses
  raw_df <- fromJSON(here("data", "raw_data", "survey", "2020-12-28 stopcensus_responses.json"))

  ## Clean field names for ease of use in analysis
  survey_df <- raw_df %>% rename(Stop_ID = stopid,
                                 Crosswalk_Features = `Crosswalk Features`,
                                 Line_of_Sight = `Line of Sight Blockage`,
                                 Direction = `Stop Direction`,
                                 On_Site_Survey = `On-site`,
                                 Trash_Can = `Trash Can`,
                                 Main_Street = `On Street`,
                                 Behavior = `Dangerous Behavior`,
                                 Nearest_Landmark = `Nearest Landmark`,
                                 Routes = `Serving Routes`,
                                 Email = From,
                                 Lighting = `Stop Lighting`,
                                 Boarding_Area = `Boarding Area`,
                                 Wayfinding = Information,
                                 Wayfinding_Accessibility = `Poster Height`,
                                 Obstacle_Desc = `Obstacle Description`,
                                 Additional_Comments = `Additional Comments`) %>%
                          select(Stop_ID, Timestamp,Email, Main_Street, Nearest_Landmark,
                                 Routes, Direction, Seating, Shelter, Trash_Can,
                                 Cleanliness, Line_of_Sight, Wayfinding, Wayfinding_Accessibility,
                                 Lighting, Sidewalk, Obstacles, Obstacle_Desc,
                                 Boarding_Area, Crosswalks, Crosswalk_Features,
                                 Behavior, On_Site_Survey, Additional_Comments) %>%
                              distinct()
  
  ## Clean Timestamp
  survey_df$Timestamp_Sub <- substring(survey_df$Timestamp, 0, 24) 
  survey_df$Timestamp_Sub <- as.POSIXct(survey_df$Timestamp_Sub, format = "%a %b %d %Y %H:%M:%S")
  
  ## Move all emails to lower case for summarization
  survey_df$Email <- tolower(survey_df$Email)
  
  
  #Specify subset of bus stops to analyze out of all bus stops
  col_names <- c("Stop_ID", "Stop_Name", "Routes", "Surveyed")
  subset <- read.csv("NPU-Z Bus IDs.csv", col.names = col_names)
  subset$Stop_ID <- as.character(subset$Stop_ID)

  subset_survey_df <- subset %>% inner_join(survey_df, by = c("Stop_ID"))
                          #mutate(Surveyed_Stop = ifelse(is.na(Stop_ID_Join_Key), "N", "Y")) %>%
                          #select(Stop_ID, Stop_Name, Routes, Surveyed_Stop)
  
  #Data Output
  write.csv(survey_df, file = here("data", "tidy_data", "pre_processed_survey_df.csv"))
  
            