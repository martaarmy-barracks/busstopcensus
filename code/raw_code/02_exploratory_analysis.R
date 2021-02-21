  
  library(sp)
  library(maps)
  library(maptools)

  ######################### IMPORT DATA ######################### 
  raw_df <- read.csv(here("data", "tidy_data", "pre_processed_survey_df.csv"))

  ## IMPORT Subset
  survey_df <- raw_df %>% rename(Stop_ID = stopid,
                                         Crosswalk_Features = `crosswalk_features`,
                                         Line_of_Sight = blocked,
                                         Direction = `headings`,
                                         On_Site_Survey = `location`,
                                         Trash_Can = `trash`,
                                         Seating = seating,
                                         Main_Street = `street`,
                                         Shelter = shelter,
                                         Behavior = `danger`,
                                         Nearest_Landmark = `landmark`,
                                         Routes = `routes`,
                                         Email = email,
                                         Sidewalk = boarding,
                                         Crosswalk = crosswalk,
                                         Lighting = `light`,
                                         Boarding_Area = `paving`,
                                         Wayfinding = wayfinding,
                                         Wayfinding_Accessibility = `height`,
                                         Obstacles = obstacles,
                                         Obstacle_Desc = `obstacle_info`,
                                         Additional_Comments = `comments`,
                                         Timestamp = timestamp,
                                         Cleanliness = dirty,
                                         Image = image) %>%
    select(Stop_ID, Timestamp, Email, Main_Street, Nearest_Landmark,
           Routes, Direction, Seating, Shelter, Trash_Can,
           Cleanliness, Line_of_Sight, Wayfinding, Wayfinding_Accessibility,
           Lighting, Sidewalk, Obstacles, Obstacle_Desc,
           Boarding_Area, Crosswalk, Crosswalk_Features,
           Behavior, On_Site_Survey, Additional_Comments) %>%
    distinct() %>%
    dplyr::filter(Stop_ID != 210010)
  
  survey_df$Stop_ID <- as.character(survey_df$Stop_ID)
  
  survey_df <- raw_df_clean_deduped
  
  ## ANALYSIS ###################################################################################################################
  # Seating
  seating <- survey_df %>% distinct(Stop_ID, Seating) %>%
    group_by(Seating) %>%
    summarize(Count = n()) %>%
    mutate(Percent_Total = format(Count / sum(Count), digits = 2))
  
  seating_coord <- survey_df %>% distinct(Stop_ID, Seating) %>%
    dplyr::filter(!is.na(Seating)) %>%
    mutate(Seating_Combined = ifelse(Seating == "No, there is no seating", "No, there is no seating", "Yes, there is seating")) %>%
    inner_join(stops, by = c("Stop_ID" = "stop_id")) %>%
    select(Stop_ID, Seating_Combined, stop_lat, stop_lon)
  
  seating_map <- leaflet(data = seating_coord) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% #HERE.normalDayGrey) %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(~stop_lon, ~stop_lat,
                     radius = 2,
                     color = ~ifelse(Seating_Combined == "Yes, there is seating","green", "red"),
                     stroke = FALSE, fillOpacity = 1) %>%
    addLegend("bottomright", colors = c("green", "red"), labels = c("Yes, there is seating", "No, there is no seating"),
              title = "Does the stop have a bench or other seating?",
              opacity = 1
    )
  seating_map 
  
  # Shelter
  shelter <- survey_df %>% 
    distinct(Stop_ID, Shelter) %>%
    group_by(Shelter) %>%
    summarize(Count = n()) %>%
    mutate(Percent_Total = format(Count / sum(Count), digits = 2))
  
  # Trash Can
  trash_can <- survey_df %>% 
    distinct(Stop_ID, Trash_Can) %>%
    group_by(Trash_Can) %>%
    summarize(Count = n()) %>%
    mutate(Percent_Total = format(Count / sum(Count), digits = 2))
  
  ## Cleanliness
  cleanliness_summary <- survey_df %>% 
                          select(Litter, Grafitti, Overflow, Dirty_Seating, Other) %>%
                          summarize(Litter = sum(Litter == "Yes"),
                                    Grafitti = sum(Grafitti == "Yes"),
                                    Overflow = sum(Overflow == "Yes"),
                                    Dirty_Seating = sum(Dirty_Seating == "Yes"),
                                    Other = sum(Other == "Yes"),
                                    Total_Stops = n()) %>%
                          gather("Cleanliness_Issue", "Count", -Total_Stops) %>%
                          mutate(Percent_Total = format(Count / Total_Stops, digits = 2)) %>%
                          select(Cleanliness_Issue, Count, Percent_Total)
  
  ## Line of Sight
  line_of_sight <- survey_df %>% 
    distinct(Stop_ID, Line_of_Sight) %>%
    group_by(Line_of_Sight) %>%
    summarize(Count = n()) %>%
    mutate(Percent_Total = format(Count / sum(Count), digits = 2))
  
  ## Wayfinding
  wayfinding <- survey_df %>% distinct(Stop_ID, Wayfinding) %>%
    mutate(Route_Number = ifelse(grepl("Route Numbers", Wayfinding, fixed = TRUE), 1, 0),
           Route_Schedule = ifelse(grepl("Route Schedule", Wayfinding, fixed = TRUE), 1, 0),
           Route_Map = ifelse(grepl("Route Map", Wayfinding, fixed = TRUE), 1,0),
           Customer_Service = ifelse(grepl("Customer Service Information", Wayfinding, fixed = TRUE), 1, 0),
           None_Of_The_Above = ifelse(grepl("None of the Above", Wayfinding, fixed = TRUE), 1, 0),
           Stop_Counter = 1,
           Some_Route_Info_Only = ifelse(Route_Number + Route_Schedule + Route_Map <= 2 & Customer_Service == 0, 1, 0),
           Some_Route_Info_And_Cust_Serv = ifelse(Route_Number + Route_Schedule + Route_Map > 0 & Route_Number + Route_Schedule + Route_Map < 3 &
                                                    Customer_Service == 1, 1,0),
           All_Route_Info_Only = ifelse(Route_Number + Route_Schedule + Route_Map == 3 & Customer_Service == 0, 1, 0),
           All_Route_Info_And_Cust_Serv = ifelse(Route_Number + Route_Schedule + Route_Map == 3 & Customer_Service == 1, 1, 0),
           Customer_Service_Only = ifelse(Route_Number + Route_Schedule + Route_Map == 0 & Customer_Service == 1, 1, 0),
           No_Wayfinding = ifelse(Route_Number + Route_Schedule + Route_Map == 0 & Customer_Service == 0, 1, 0))
  
  wayfinding_summary <- survey_df %>% 
                            select(Route_Number, Route_Schedule, Route_Map,
                                   Customer_Service, None_Of_The_Above) %>%
                            summarize(Route_Number = sum(Route_Number == "Yes"),
                                      Route_Schedule = sum(Route_Schedule == "Yes"),
                                      Route_Map = sum(Route_Map == "Yes"),
                                      Customer_Service = sum(Customer_Service == "Yes"),
                                      None_Of_The_Above = sum(None_Of_The_Above == "Yes"),
                                      Total_Stops = n()) %>%
                            gather("Wayfinding_Option", "Count", -Total_Stops) %>%
                            mutate(Percent_Total = format(Count / Total_Stops, digits = 2)) %>%
                            select(Wayfinding_Option, Count, Percent_Total)
  
  wayfinding_summary_comb <- wayfinding %>% select(Some_Route_Info_Only, Some_Route_Info_And_Cust_Serv, All_Route_Info_Only,
                                                   All_Route_Info_And_Cust_Serv, Customer_Service_Only, No_Wayfinding) %>%
    summarize(Some_Route_Info_Only = sum(Some_Route_Info_Only), 
              Some_Route_Info_And_Cust_Serv = sum(Some_Route_Info_And_Cust_Serv), 
              All_Route_Info_Only = sum(All_Route_Info_Only),
              All_Route_Info_And_Cust_Serv = sum(All_Route_Info_And_Cust_Serv),
              Customer_Service_Only = sum(Customer_Service_Only), 
              No_Wayfinding = sum(No_Wayfinding)) %>%
    gather("Wayfinding Type", "Count")
  
  ## Wayfinding Accessibility
  wayfinding_accessibility <- survey_df %>% 
                                  distinct(Stop_ID, Wayfinding_Accessibility) %>%
                                  group_by(Wayfinding_Accessibility) %>%
                                  summarize(Count = n()) %>%
                                  mutate(Percent_Total = format(Count / sum(Count), digits = 2))
  
  ## Lighting
  lighting <- survey_df %>% 
                  distinct(Stop_ID, Lighting) %>%
                  group_by(Lighting) %>%
                  summarize(Count = n()) %>%
                  mutate(Percent_Total = format(Count / sum(Count), digits = 2))
  
  ## Sidewalks
  sidewalk <- survey_df %>% 
                  distinct(Stop_ID, Sidewalk) %>%
                  group_by(Sidewalk) %>%
                  summarize(Count = n()) %>%
                  mutate(Percent_Total = format(Count / sum(Count), digits = 2))
  
  sidewalk_coord <- survey_df %>% distinct(Stop_ID, Sidewalk) %>%
    dplyr::filter(!is.na(Sidewalk)) %>%
    inner_join(stops, by = c("Stop_ID" = "stop_id")) %>%
    select(Stop_ID, Sidewalk, stop_lat, stop_lon)
  
  sidewalk_map <- leaflet(data = sidewalk_coord) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% #HERE.normalDayGrey) %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(~stop_lon, ~stop_lat,
                     radius = 2,
                     color = ~ifelse(Sidewalk == "Yes, in both directions","green", ifelse(Sidewalk == "Yes, in only one direction", "yellow", "red")),
                     stroke = FALSE, fillOpacity = 1) %>%
    addLegend("bottomright", colors = c("green", "yellow", "red"), labels = c("Yes, in both directions", "Yes, in only one direction", "No"),
              title = "Is there a paved sidewalk to the boarding area of the bus?",
              opacity = 1
    )
  sidewalk_map
  
  ## Obstacles
  obstacles <- survey_df %>% distinct(Stop_ID, Obstacles) %>%
    dplyr::filter(!is.na(Obstacles)) %>%
    group_by(Obstacles) %>%
    summarize(Count = n()) %>%
    mutate(Percent_Total = format(Count / sum(Count), digits = 2))
  
  obstacles_description <- survey_df %>% distinct(Stop_ID, Main_Street, Nearest_Landmark, Routes, Direction, Obstacles, Obstacle_Desc) %>%
    dplyr::filter(Obstacles == "Yes", !is.na(Obstacle_Desc))
  
  write.csv(obstacles_description, file = "Bus Stop Census Obstacle Descriptions.csv") 
  
  ## Boarding Areas
  boarding <- survey_df %>% 
                  distinct(Stop_ID, Boarding_Area) %>%
                  group_by(Boarding_Area) %>%
                  summarize(Count = n()) %>%
                  mutate(Percent_Total = format(Count / sum(Count), digits = 2))
  
  boarding_coord <- survey_df %>% distinct(Stop_ID, Boarding_Area) %>%
    dplyr::filter(!is.na(Boarding_Area) & (Boarding_Area == "Concrete sidewalk" | Boarding_Area == "Grass or dirt")) %>%
    inner_join(stops, by = c("Stop_ID" = "stop_id")) %>%
    select(Stop_ID, Boarding_Area, stop_lat, stop_lon)
  
  boarding_map <- leaflet(data = boarding_coord) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% #HERE.normalDayGrey) %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(~stop_lon, ~stop_lat,
                     radius = 2,
                     color = ~ifelse(Boarding_Area == "Concrete sidewalk","green", "red"),
                     stroke = FALSE, fillOpacity = 1) %>%
    addLegend("bottomright", colors = c("green", "red"), labels = c("Concrete Sidewalk", "Grass or Dirt"),
              title = "What is the surface of the boarding area made of?",
              opacity = 1
    )
  boarding_map
  
  ## Crosswalks
  
  crosswalks <- survey_df %>% distinct(Stop_ID, Crosswalks) %>%
    dplyr::filter(!is.na(Crosswalks)) %>%
    mutate(Main_Street = ifelse(grepl("Yes, on the main street", Crosswalks, fixed = TRUE), 1, 0),
           Cross_Street = ifelse(grepl("Yes, on the cross street",Crosswalks, fixed = TRUE), 1, 0),
           Worn_Faded = ifelse(grepl("Yes, and crosswalk paint is faded or worn away", Crosswalks, fixed = TRUE), 1,0),
           No_Crosswalk = ifelse(grepl("No, no painted crosswalk within 100 feet", Crosswalks, fixed = TRUE), 1, 0),
           Verify = ifelse((Main_Street + Cross_Street + Worn_Faded) > 0 & No_Crosswalk == 1, 1, 0),
           Main_Street_Only = ifelse(Main_Street == 1 & Cross_Street == 0, 1, 0),
           Cross_Street_Only = ifelse(Main_Street == 0 & Cross_Street == 1, 1,0),
           Main_and_Cross = ifelse(Main_Street == 1 & Cross_Street == 1, 1, 0)) %>%
    mutate(Verify_2 = Main_Street_Only + Cross_Street_Only + Main_and_Cross + No_Crosswalk)
  
  
  crosswalks_summary <- crosswalks %>% select(Stop_ID, Main_Street_Only, Cross_Street_Only, Main_and_Cross, No_Crosswalk) %>%
    mutate(Verify = Main_Street_Only + Cross_Street_Only + Main_and_Cross + No_Crosswalk) %>%
    dplyr::filter(Verify != 0 | Verify != 2) %>%
    mutate(Crosswalk_Type = ifelse(Main_Street_Only == 1, "Main Street Only", 
                                   ifelse(Cross_Street_Only == 1, "Cross Street Only",
                                          ifelse(Main_and_Cross == 1, "Main and Cross Street", "No Crosswalk")))) %>%
    select(Stop_ID, Crosswalk_Type) %>%
    group_by(Crosswalk_Type) %>%
    summarize(Count = n()) %>%
    mutate(Percent_Total = format(Count / sum(Count), digits = 2))
  
  crosswalks_coords <- crosswalks %>% select(Stop_ID, Main_Street_Only, Cross_Street_Only, Main_and_Cross, No_Crosswalk) %>%
    mutate(Verify = Main_Street_Only + Cross_Street_Only + Main_and_Cross + No_Crosswalk) %>%
    dplyr::filter(Verify != 0 | Verify != 2) %>%
    mutate(Crosswalk_Type = ifelse(Main_Street_Only == 1, "Main Street Only", 
                                   ifelse(Cross_Street_Only == 1, "Cross Street Only",
                                          ifelse(Main_and_Cross == 1, "Main and Cross Street", "No Crosswalk")))) %>%
    select(Stop_ID, Crosswalk_Type) %>%
    inner_join(stops, by = c("Stop_ID" = "stop_id")) %>%
    select(Stop_ID, Crosswalk_Type, stop_lat, stop_lon)      
  
  
  crosswalks_map <- leaflet(data = crosswalks_coords) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% #HERE.normalDayGrey) %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(~stop_lon, ~stop_lat,
                     radius = 2,
                     color = ~ifelse(Crosswalk_Type == "Main and Cross Street","green", ifelse(Crosswalk_Type == "Main Street Only", "yellow",
                                                                                               ifelse(Crosswalk_Type == "Cross Street Only", "orange", "red"))),
                     stroke = FALSE, fillOpacity = 1) %>%
    addLegend("bottomright", colors = c("green", "yellow", "orange", "red"), labels = c("Yes, there is a crosswalk on the main and cross streets", 
                                                                                        "Yes, there is a crosswalk only on the main street",
                                                                                        "Yes, there is a crosswalk only on the cross street",
                                                                                        "No, there is no crosswalk on either main or cross street"),
              title = "Is there a clearly marked crosswalk within 100 feet of the stop?",
              opacity = 1
    )
  crosswalks_map
  
  ## Crosswalk Features
  crosswalk_features <- srg_survey_df %>% distinct(Stop_ID, Crosswalk_Features) %>%
    dplyr::filter(!is.na(Crosswalk_Features)) %>%
    mutate(Traffic_Light = ifelse(grepl("Traffic_Light", Crosswalk_Features, fixed = TRUE), 1, 0),
           Curb_Cuts = ifelse(grepl("Curb cuts for wheelchairs",Crosswalk_Features, fixed = TRUE), 1, 0),
           Crosswalk_Signals = ifelse(grepl("Crosswalk signals with push buttons", Crosswalk_Features, fixed = TRUE), 1,0),
           Crossing_Audio = ifelse(grepl("Crossing audio overlays for the visually impaired", Crosswalk_Features, fixed = TRUE), 1, 0),
           Tactile_Guide = ifelse(grepl("Tactile guide strips for the visually impaired", Crosswalk_Features, fixed = TRUE), 1, 0),
           Stop_Counter = 1)
  
  
  crosswalk_feature_summary <- crosswalk_features %>% select(Traffic_Light, Curb_Cuts, Crosswalk_Signals, 
                                                             Crossing_Audio, Tactile_Guide, Stop_Counter) %>%
    summarize(Traffic_Light = sum(Traffic_Light),
              Curb_Cuts = sum(Curb_Cuts),
              Crosswalk_Signals = sum(Crosswalk_Signals),
              Crossing_Audio = sum(Crossing_Audio),
              Tactile_Guide = sum(Tactile_Guide),
              Total_Stops = sum(Stop_Counter)) %>%
    gather("Crosswalk_Feature", "Count", -Total_Stops) %>%
    mutate(Percent_Total = format(Count / Total_Stops, digits = 2)) %>%
    select(Crosswalk_Feature, Count, Percent_Total)
  
  
  # Observations
  observations <- srg_survey_df %>% distinct(Stop_ID, Behavior) %>%
    dplyr::filter(!is.na(Behavior)) %>%
    mutate(Informal_Pathways = ifelse(grepl("Pedestrians using informal pathways where sidewalks do not exist", Behavior, fixed = TRUE), 1, 0),
           Compete_For_Seat = ifelse(grepl("Pedestrians competing for seating at the bus stop",Behavior, fixed = TRUE), 1, 0),
           Cross_Midblock = ifelse(grepl("Pedestrians crossing the roadway at midblock locations", Behavior, fixed = TRUE), 1,0),
           Catch_The_Bus = ifelse(grepl("Pedestrians running across roadways to catch the bus", Behavior, fixed = TRUE), 1, 0),
           Dangerous_Motorists = ifelse(grepl("Dangerous motorist behavior around bus stop (i.e. speeding or not yielding to pedestrians)", Behavior, fixed = TRUE), 1, 0),
           First_Visit = ifelse(grepl("None of the above (first visit to this stop)", Behavior, fixed = TRUE), 1, 0),
           Regular_User_None = ifelse(grepl("None of the above (occasional or frequent user of this stop", Behavior, fixed = TRUE), 1, 0),
           Stop_Counter = 1)
  
  
  observations_summary <- observations %>% select(Informal_Pathways, Compete_For_Seat, Cross_Midblock, 
                                                  Catch_The_Bus, Dangerous_Motorists, First_Visit,
                                                  Regular_User_None, Stop_Counter) %>%
    summarize(Informal_Pathways = sum(Informal_Pathways),
              Compete_For_Seat = sum(Compete_For_Seat),
              Cross_Midblock = sum(Cross_Midblock),
              Catch_The_Bus = sum(Catch_The_Bus),
              Dangerous_Motorists = sum(Dangerous_Motorists),
              First_Visit = sum(First_Visit),
              Regular_User_None = sum(Regular_User_None),
              Total_Stops = sum(Stop_Counter)) %>%
    gather("Observed_Behavior", "Count", -Total_Stops) %>%
    mutate(Percent_Total = format(Count / Total_Stops, digits = 2)) %>%
    select(Observed_Behavior, Count, Percent_Total)
  
  #Anecdotes
  
  anecdotes <- srg_survey_df %>% distinct(Stop_ID, Main_Street, Nearest_Landmark, Routes, Direction,
                                          Obstacles, Obstacle_Desc, Additional_Comments) 
  write.csv(anecdotes, file = "SRG Bus Stop Census Anecdotes.csv") 
  
  ## Bad Bus Stops
  
  bad_stops <- survey_df %>% dplyr::filter(Seating == "No, there is no seating",
                                           Shelter == "No",
                                           (Wayfinding == "None of the Above" | Wayfinding == "Customer Service Information"),
                                           (Wayfinding_Accessibility == "No wayfinding information present" | Wayfinding_Accessibility == "No"),
                                           (Lighting == "No" | Lighting == "I don't know"),
                                           Sidewalk == "No",
                                           Obstacles == "Yes",
                                           Boarding_Area == "Grass or dirt",
                                           Crosswalks == "No, no painted crosswalk within 100 feet")
  
  ## 212553, 211309