
  ######################### IMPORT DATA ######################### 
  # Survey Responses
  survey_df <- read.csv(here("data", "tidy_data", "pre_processed_survey_df.csv"))
  survey_df$Stop_ID <- as.character(survey_df$Stop_ID)
  
  # Ridership Data
  pre_covid_weekday <- read.xlsx(
    here("data", "raw_data", "supplemental", "Average Weekday Saturday Sunday Stop Activity_November 1-15_2019-2020_MARTA Army.xlsx"),
    sheet = "Nov 1-15, 2019_Wkdy",
    startRow = 5,
    colNames = TRUE,
    rowNames = FALSE
  ) %>%
    rename(Pre_COVID_Weekday_Boardings = Ons,
           Pre_COVID_Weekday_Alightings = Offs) 
  
  pre_covid_saturday <- read.xlsx(
    here("data", "raw_data", "supplemental", "Average Weekday Saturday Sunday Stop Activity_November 1-15_2019-2020_MARTA Army.xlsx"),
    sheet = "Nov 1-15, 2019_Sat",
    startRow = 5,
    colNames = TRUE,
    rowNames = FALSE
  ) %>%
    rename(Pre_COVID_Saturday_Boardings = Ons,
           Pre_COVID_Saturday_Alightings = Offs) 
  
  pre_covid_sunday <- read.xlsx(
    here("data", "raw_data", "supplemental", "Average Weekday Saturday Sunday Stop Activity_November 1-15_2019-2020_MARTA Army.xlsx"),
    sheet = "Nov 1-15, 2019_Sun",
    startRow = 5,
    colNames = TRUE,
    rowNames = FALSE
  ) %>%
    rename(Pre_COVID_Sunday_Boardings = Ons,
           Pre_COVID_Sunday_Alightings = Offs) 
  
  post_covid_weekday <- read.xlsx(
    here("data", "raw_data", "supplemental", "Average Weekday Saturday Sunday Stop Activity_November 1-15_2019-2020_MARTA Army.xlsx"),
    sheet = "Nov 1-15, 2020_Wkdy",
    startRow = 5,
    colNames = TRUE,
    rowNames = FALSE
  ) %>%
    rename(Post_COVID_Weekday_Boardings = Ons,
           Post_COVID_Weekday_Alightings = Offs) 
  
  post_covid_saturday <- read.xlsx(
    here("data", "raw_data", "supplemental", "Average Weekday Saturday Sunday Stop Activity_November 1-15_2019-2020_MARTA Army.xlsx"),
    sheet = "Nov 1-15, 2020_Sat",
    startRow = 5,
    colNames = TRUE,
    rowNames = FALSE
  ) %>%
    rename(Post_COVID_Saturday_Boardings = Ons,
           Post_COVID_Saturday_Alightings = Offs) 
  
  post_covid_sunday <- read.xlsx(
    here("data", "raw_data", "supplemental", "Average Weekday Saturday Sunday Stop Activity_November 1-15_2019-2020_MARTA Army.xlsx"),
    sheet = "Nov 1-15, 2020_Sun",
    startRow = 5,
    colNames = TRUE,
    rowNames = FALSE
  ) %>%
    rename(Post_COVID_Sunday_Boardings = Ons,
           Post_COVID_Sunday_Alightings = Offs)
  
  boarding_alighting_df <- pre_covid_weekday %>% 
    full_join(pre_covid_saturday, by = c("Route", "Direction", "Stop.ID", "Stop.Name")) %>%
    full_join(pre_covid_sunday, by = c("Route", "Direction", "Stop.ID", "Stop.Name")) %>%
    full_join(post_covid_weekday, by = c("Route", "Direction", "Stop.ID", "Stop.Name")) %>%
    full_join(post_covid_saturday, by = c("Route", "Direction", "Stop.ID", "Stop.Name")) %>%
    full_join(post_covid_sunday, by = c("Route", "Direction", "Stop.ID", "Stop.Name"))
  
  rm(pre_covid_weekday)
  rm(pre_covid_saturday)
  rm(pre_covid_sunday)
  rm(post_covid_weekday)
  rm(post_covid_saturday)
  rm(post_covid_sunday)
  
  boarding_alighting_summary <- boarding_alighting_df %>%
      mutate(Total_Weekly_Avg_Pre_COVID_Boardings = Pre_COVID_Weekday_Boardings + Pre_COVID_Saturday_Boardings + Pre_COVID_Sunday_Boardings,
             Total_Weekly_Avg_Pre_COVID_Alightings = Pre_COVID_Weekday_Alightings + Pre_COVID_Saturday_Alightings + Pre_COVID_Sunday_Alightings,
             Total_Weekly_Avg_Pre_COVID_Ons_Offs = Pre_COVID_Weekday_Boardings + Pre_COVID_Saturday_Boardings + Pre_COVID_Sunday_Boardings +
                                                       Pre_COVID_Weekday_Alightings + Pre_COVID_Saturday_Alightings + Pre_COVID_Sunday_Alightings,
             Total_Weekly_Avg_Post_COVID_Boardings = Post_COVID_Weekday_Boardings + Post_COVID_Saturday_Boardings + Post_COVID_Sunday_Boardings,
             Total_Weekly_Avg_Post_COVID_Alightings = Post_COVID_Weekday_Alightings + Post_COVID_Saturday_Alightings + Post_COVID_Sunday_Alightings,
             Total_Weekly_Avg_Post_COVID_Ons_Offs = Post_COVID_Weekday_Boardings + Post_COVID_Saturday_Boardings + Post_COVID_Sunday_Boardings +
                                                       Post_COVID_Weekday_Alightings + Post_COVID_Saturday_Alightings + Post_COVID_Sunday_Alightings) %>%
    select(Stop.ID, Stop.Name, Total_Weekly_Avg_Pre_COVID_Boardings, Total_Weekly_Avg_Pre_COVID_Alightings,
           Total_Weekly_Avg_Pre_COVID_Ons_Offs, Total_Weekly_Avg_Post_COVID_Boardings, Total_Weekly_Avg_Post_COVID_Alightings,
           Total_Weekly_Avg_Post_COVID_Ons_Offs) %>%
    group_by(Stop.ID, Stop.Name) %>%
    summarize(Total_Weekly_Avg_Pre_COVID_Boardings = sum(Total_Weekly_Avg_Pre_COVID_Boardings, na.rm = TRUE),
              Total_Weekly_Avg_Pre_COVID_Alightings = sum(Total_Weekly_Avg_Pre_COVID_Alightings, na.rm = TRUE),
              Total_Weekly_Avg_Pre_COVID_Ons_Offs = sum(Total_Weekly_Avg_Pre_COVID_Ons_Offs, na.rm = TRUE),
              Total_Weekly_Avg_Post_COVID_Boardings = sum(Total_Weekly_Avg_Post_COVID_Boardings, na.rm = TRUE),
              Total_Weekly_Avg_Post_COVID_Alightings = sum(Total_Weekly_Avg_Post_COVID_Alightings, na.rm = TRUE),
              Total_Weekly_Avg_Post_COVID_Ons_Offs = sum(Total_Weekly_Avg_Post_COVID_Ons_Offs, na.rm = TRUE))
  
  boarding_alighting_summary$Stop.ID <- as.character(boarding_alighting_summary$Stop.ID)
  
  survey_df <- survey_df %>%
    inner_join(boarding_alighting_summary, by = c("Stop_ID" = "Stop.ID")) %>%
    select(-Stop.Name)
  
  ######################### CREATE EXCEL WORKBOOK FOR FINDINGS #########################
  wb <- createWorkbook()
  
  addWorksheet(wb, "Survey Responses")
  addWorksheet(wb, "Boarding and Alighting Data")
  addWorksheet(wb, "Boarding and Alighting Summary")
  addWorksheet(wb, "Seating")
  addWorksheet(wb, "Shelter")
  addWorksheet(wb, "Trash Can")
  addWorksheet(wb, "Cleanliness")
  addWorksheet(wb, "Line of Sight")
  addWorksheet(wb, "Wayfinding")
  addWorksheet(wb, "Sidewalks")
  addWorksheet(wb, "Obtacles")
  addWorksheet(wb, "Obstacle Descriptions")
  addWorksheet(wb, "Boarding Area")
  addWorksheet(wb, "Crosswalks")
  addWorksheet(wb, "Crosswalk Features")
  addWorksheet(wb, "Observed Behavior")
  
  writeData(wb, "Survey Responses", survey_df, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, "Boarding and Alighting Data", boarding_alighting_df, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, "Boarding and Alighting Summary", boarding_alighting_summary, startCol = 1, startRow = 1, colNames = TRUE)
  
  ######################### ORDER RESPONSES #########################
  survey_df$Seating <- factor(survey_df$Seating, 
                              levels = c("Yes, there is seating provided by MARTA or another transit agency",
                                         "Yes, there is seating provided by someone else",
                                         "No, there is no seating"))
  
  survey_df$Shelter <- factor(survey_df$Shelter, 
                              levels = c("Yes",
                                         "No"))
  
  survey_df$Trash_Can <- factor(survey_df$Trash_Can, 
                              levels = c("Yes",
                                         "No"))
  
  survey_df$Route_Number <- factor(survey_df$Route_Number, 
                                levels = c("Yes",
                                           "No"))
  
  survey_df$Route_Schedule <- factor(survey_df$Route_Schedule, 
                                   levels = c("Yes",
                                              "No"))
  
  survey_df$Route_Map <- factor(survey_df$Route_Map, 
                                   levels = c("Yes",
                                              "No"))
  
  survey_df$Wayfinding_Accessibility <- factor(survey_df$Wayfinding_Accessibility,
                                               levels = c("Yes",
                                                          "No",
                                                          "No wayfinding information present"))
  
  survey_df$Lighting <- factor(survey_df$Lighting,
                               levels = c("Yes",
                                          "No",
                                          "I don't know"))
  
  survey_df$Sidewalk <- factor(survey_df$Sidewalk, 
                                levels = c("Yes, in both directions",
                                           "Yes, in only one direction",
                                           "No"))
  
  survey_df$Obstacles <- factor(survey_df$Obstacles,
                                levels = c("Yes",
                                           "No"))
  
  

  ## ANALYSIS ###################################################################################################################
  # Seating
  seating <- summaryTableCreator(survey_df, "Seating", NA)
  seating_by_county <- summaryTableCreator(survey_df, "Seating", "County")
  seating_by_city <- summaryTableCreator(survey_df, "Seating", "City")
  
  writeData(wb, "Seating", seating, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, "Seating", seating_by_county, startCol = 1, startRow = nrow(seating) + 3, colNames = TRUE)
  writeData(wb, "Seating", seating_by_city, startCol = 1, startRow = nrow(seating) + nrow(seating_by_county) + 6, colNames = TRUE)
  
  seating_coord <- survey_df %>% distinct(Stop_ID, Stop_Lat, Stop_Lon, Seating) %>%
    mutate(Seating_Combined = ifelse(Seating == "No, there is no seating", "No, there is no seating", "Yes, there is seating")) %>%
    select(Stop_ID, Seating_Combined, Stop_Lat, Stop_Lon)
  
  seating_map <- leaflet(data = seating_coord) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% #HERE.normalDayGrey) %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(~Stop_Lon, ~Stop_Lat,
                     radius = 2,
                     color = ~ifelse(Seating_Combined == "Yes, there is seating","green", "red"),
                     stroke = FALSE, fillOpacity = 1) %>%
    addLegend("bottomright", colors = c("green", "red"), labels = c("Yes, there is seating", "No, there is no seating"),
              title = "Does the stop have a bench or other seating?",
              opacity = 1
    )
  seating_map 
  
  # Shelter
  shelter <- summaryTableCreator(survey_df, "Shelter", NA)
  shelter_by_county <- summaryTableCreator(survey_df, "Shelter", "County")
  shelter_by_city <- summaryTableCreator(survey_df, "Shelter", "City")
  
  writeData(wb, "Shelter", shelter, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, "Shelter", shelter_by_county, startCol = 1, startRow = nrow(shelter) + 3, colNames = TRUE)
  writeData(wb, "Shelter", shelter_by_city, startCol = 1, startRow = nrow(shelter) + nrow(shelter_by_county) + 6, colNames = TRUE)
  
  # Trash Can
  trash_can <- summaryTableCreator(survey_df, "Trash_Can", NA)
  trash_can_by_county <- summaryTableCreator(survey_df, "Trash_Can", "County")
  trash_can_by_city <- summaryTableCreator(survey_df, "Trash_Can", "City")
  
  writeData(wb, "Trash Can", trash_can, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, "Trash Can", trash_can_by_county, startCol = 1, startRow = nrow(shelter) + 3, colNames = TRUE)
  writeData(wb, "Trash Can", trash_can_by_city, startCol = 1, startRow = nrow(trash_can) + nrow(trash_can_by_county) + 6, colNames = TRUE)
  
  ## Cleanliness
  cleanliness <- survey_df %>% 
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
  
  cleanliness_by_county <- survey_df %>% 
    select(County, Litter, Grafitti, Overflow, Dirty_Seating, Other) %>%
    group_by(County) %>%
    summarize(Litter = sum(Litter == "Yes"),
              Grafitti = sum(Grafitti == "Yes"),
              Overflow = sum(Overflow == "Yes"),
              Dirty_Seating = sum(Dirty_Seating == "Yes"),
              Other = sum(Other == "Yes"),
              Total_Stops = n()) 
   
  cleanliness_by_city <- survey_df %>% 
    select(City, Litter, Grafitti, Overflow, Dirty_Seating, Other) %>%
    group_by(City) %>%
    summarize(Litter = sum(Litter == "Yes"),
              Grafitti = sum(Grafitti == "Yes"),
              Overflow = sum(Overflow == "Yes"),
              Dirty_Seating = sum(Dirty_Seating == "Yes"),
              Other = sum(Other == "Yes"),
              Total_Stops = n())
  
  writeData(wb, "Cleanliness", cleanliness, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, "Cleanliness", cleanliness_by_county, startCol = 1, startRow = nrow(cleanliness) + 3, colNames = TRUE)
  writeData(wb, "Cleanliness", cleanliness_by_city, startCol = 1, startRow = nrow(cleanliness) + nrow(cleanliness_by_county) + 6, colNames = TRUE)
  
  ## Line of Sight
  line_of_sight <- summaryTableCreator(survey_df, "Line_of_Sight", NA)
  line_of_sight_by_county <- summaryTableCreator(survey_df, "Line_of_Sight", NA)
  line_of_sight_by_city <- summaryTableCreator(survey_df, "Line_of_Sight", NA)

  writeData(wb, "Line of Sight", line_of_sight, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, "Line of Sight", line_of_sight_by_county, startCol = 1, startRow = nrow(line_of_sight) + 3, colNames = TRUE)
  writeData(wb, "Line of Sight", line_of_sight_by_city, startCol = 1, startRow = nrow(line_of_sight) + nrow(line_of_sight_by_county) + 6, colNames = TRUE)
  
  ## Wayfinding
  # Route Number
  route_number <- summaryTableCreator(survey_df, "Route_Number", NA)
  route_number_by_county <- summaryTableCreator(survey_df, "Route_Number", NA)
  route_number_by_city <- summaryTableCreator(survey_df, "Route_Number", NA)
  
  # Route Schedule
  route_schedule <- summaryTableCreator(survey_df, "Route_Schedule", NA)
  route_schedule_by_county <- summaryTableCreator(survey_df, "Route_Schedule", NA)
  route_schedule_by_city <- summaryTableCreator(survey_df, "Route_Schedule", NA)
  
  # Route Map
  route_map <- summaryTableCreator(survey_df, "Route_Map", NA)
  route_map_by_county <- summaryTableCreator(survey_df, "Route_Map", NA)
  route_map_by_city <- summaryTableCreator(survey_df, "Route_Map", NA)
  
  # Wayfinding Summary
  wayfinding_summary <- survey_df %>% 
                            select(Route_Number, Route_Schedule, Route_Map,
                                   Customer_Service, None_Of_The_Above, Total_Weekly_Avg_Pre_COVID_Ons_Offs) %>%
                            summarize(Route_Number = sum(Route_Number == "Yes"),
                                      Route_Schedule = sum(Route_Schedule == "Yes"),
                                      Route_Map = sum(Route_Map == "Yes"),
                                      Customer_Service = sum(Customer_Service == "Yes"),
                                      None_Of_The_Above = sum(None_Of_The_Above == "Yes"),
                                      Total_Stops = n()) %>%
                            gather("Wayfinding_Option", "Count", -Total_Stops) %>%
                            mutate(Percent_Total = format(Count / Total_Stops, digits = 2)) %>%
                            select(Wayfinding_Option, Count, Percent_Total)
  
  wayfinding_detail <- survey_df %>% 
    distinct(Stop_ID, City, County, Route_Number, Route_Schedule, Route_Map, Customer_Service,
             Total_Weekly_Avg_Pre_COVID_Boardings, Total_Weekly_Avg_Pre_COVID_Alightings,
             Total_Weekly_Avg_Pre_COVID_Ons_Offs, Total_Weekly_Avg_Post_COVID_Boardings, Total_Weekly_Avg_Post_COVID_Alightings,
             Total_Weekly_Avg_Post_COVID_Ons_Offs) %>%
    mutate(Wayfinding_Category = ifelse(str_count(paste(Route_Number, Route_Schedule, Route_Map, sep = ""), "Yes") > 0 & 
                                                    str_count(paste(Route_Number, Route_Schedule, Route_Map, sep = ""), "Yes") < 3 &
                                                    Customer_Service == "Yes", "Some Route Info and Customer Service Information",
                                                  ifelse(str_count(paste(Route_Number, Route_Schedule, Route_Map, sep = ""), "Yes") == 3 & 
                                                           Customer_Service == "Yes", "All Route Info and Customer Service Information", 
                                                         ifelse(str_count(paste(Route_Number,  Route_Schedule,  Route_Map, sep = ""), "Yes") == 0 & 
                                                                  Customer_Service == "Yes", "Customer Service Only", ""))))
  
  
  wayfinding_category <- summaryTableCreator(wayfinding_detail, "Wayfinding_Category", NA)
  wayfinding_category_by_county <- summaryTableCreator(wayfinding_detail, "Wayfinding_Category", "county")
  wayfinding_category_by_city <- summaryTableCreator(wayfinding_detail, "Wayfinding_Category", "city")
  

  wayfinding_by_county <- survey_df %>% 
    select(County, Route_Number, Route_Schedule, Route_Map,
           Customer_Service, None_Of_The_Above) %>%
    group_by(County) %>%
    summarize(Route_Number = sum(Route_Number == "Yes"),
              Route_Schedule = sum(Route_Schedule == "Yes"),
              Route_Map = sum(Route_Map == "Yes"),
              Customer_Service = sum(Customer_Service == "Yes"),
              None_Of_The_Above = sum(None_Of_The_Above == "Yes"),
              Total_Stops = n())
  
  wayfinding_by_city <- survey_df %>% 
    select(City, Route_Number, Route_Schedule, Route_Map,
           Customer_Service, None_Of_The_Above) %>%
    group_by(City) %>%
    summarize(Route_Number = sum(Route_Number == "Yes"),
              Route_Schedule = sum(Route_Schedule == "Yes"),
              Route_Map = sum(Route_Map == "Yes"),
              Customer_Service = sum(Customer_Service == "Yes"),
              None_Of_The_Above = sum(None_Of_The_Above == "Yes"),
              Total_Stops = n())
  
  writeData(wb, "Wayfinding", line_of_sight, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, "Wayfinding", line_of_sight_by_county, startCol = 1, startRow = nrow(line_of_sight) + 3, colNames = TRUE)
  writeData(wb, "Wayfinding", line_of_sight_by_city, startCol = 1, startRow = nrow(line_of_sight) + nrow(line_of_sight_by_county) + 6, colNames = TRUE)
  
  
  ## Wayfinding Accessibility
  wayfinding_accessibility <- summaryTableCreator(survey_df, "Wayfinding_Accessibility", NA)
  wayfinding_accessibility_by_county <- summaryTableCreator(survey_df, "Wayfinding_Accessibility", "County")
  wayfinding_accessibility_by_city <- summaryTableCreator(survey_df, "Wayfinding_Accessibility", "City")
  
  writeData(wb, "Wayfinding Accessibility", wayfinding_accessibility, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, "Wayfinding Accessibility", wayfinding_accessibility_by_county, startCol = 1, startRow = nrow(wayfinding_accessibility) + 3, colNames = TRUE)
  writeData(wb, "Wayfinding Accessibility", wayfinding_accessibility_by_city, startCol = 1, startRow = nrow(wayfinding_accessibility) + nrow(wayfinding_accessibility_by_county) + 6, colNames = TRUE)
  
  ## Lighting
  lighting <- summaryTableCreator(survey_df, "Lighting", NA)
  lighting_by_county <- summaryTableCreator(survey_df, "Lighting", "County")
  lighting_by_city <- summaryTableCreator(survey_df, "Lighting", "City")
  
  writeData(wb, "Lighting", lighting, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, "Lighting", lighting_by_county, startCol = 1, startRow = nrow(lighting) + 3, colNames = TRUE)
  writeData(wb, "Lighting", lighting_by_city, startCol = 1, startRow = nrow(lighting) + nrow(lighting_by_county) + 6, colNames = TRUE)
  
  ## Sidewalks
  sidewalk <- summaryTableCreator(survey_df, "Sidewalk", NA)
  sidewalk_by_county <- summaryTableCreator(survey_df, "Sidewalk", "County")
  sidewalk_by_county <- summaryTableCreator(survey_df, "Sidewalk", "City")  
  
  writeData(wb, "Sidewalk", sidewalk, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, "Sidewalk", sidewalk_by_county, startCol = 1, startRow = nrow(sidewalk) + 3, colNames = TRUE)
  writeData(wb, "Sidewalk", sidewalk_by_city, startCol = 1, startRow = nrow(sidewalk) + nrow(sidewalk_by_county) + 6, colNames = TRUE)
  
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
  obstacles <- summaryTableCreator(survey_df, "Obstacles", NA)
  obstacles_by_county <- summaryTableCreator(survey_df, "Obstacles", "County")
  obstacles_by_city <- summaryTableCreator(survey_df, "Obstacles", "City")
  
  obstacles_description <- survey_df %>% 
    distinct(Stop_ID, Main_Street, Nearest_Landmark, County,
             City, Routes, Direction, Obstacles, Obstacle_Desc) %>%
    dplyr::filter(Obstacles == "Yes", !is.na(Obstacle_Desc))
  
  writeData(wb, "Obstacles", obstacles, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, "Obstacles", obstacles_by_county, startCol = 1, startRow = nrow(obstacles) + 3, colNames = TRUE)
  writeData(wb, "Obstacles", obstacles_by_city, startCol = 1, startRow = nrow(obstacles) + nrow(obstacles_by_county) + 6, colNames = TRUE)
  writeData(wb, "Obstacle Description", obstacles_description, startCol = 1, startRow = 1, colNames = TRUE)
  
  ## Boarding Areas
  boarding_area <- summaryTableCreator(survey_df, "Boarding_Area", NA)
  boarding_area_by_county <- summaryTableCreator(survey_df, "Boarding_Area", "County")
  boarding_area_by_city <- summaryTableCreator(survey_df, "Boarding_Area", "City")
  
  writeData(wb, "Boarding_area", boarding_area, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, "Boarding_area", boarding_area_by_county, startCol = 1, startRow = nrow(boarding_area) + 3, colNames = TRUE)
  writeData(wb, "Boarding_area", boarding_area_by_city, startCol = 1, startRow = nrow(boarding_area) + nrow(boarding_area_by_county) + 6, colNames = TRUE)
  
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
  # Main Street Crosswalk
  main_street_crosswalk <- summaryTableCreator(survey_df, "Main_Street_Crosswalk", NA)
  main_street_crosswalk_by_county <- summaryTableCreator(survey_df, "Main_Street_Crosswalk", "County")
  main_street_crosswalk_by_city <- summaryTableCreator(survey_df, "Main_Street_Crosswalk", "City")
  
  # Cross Street Crosswalk
  cross_street_crosswalk <- summaryTableCreator(survey_df, "Cross_Street_Crosswalk", NA)
  cross_street_crosswalk_by_county <- summaryTableCreator(survey_df, "Cross_Street_Crosswalk", "County")
  cross_street_crosswalk_by_city <- summaryTableCreator(survey_df, "Cross_Street_Crosswalk", "City")
  
  # Worn and Faded Crosswalk
  worn_faded <- summaryTableCreator(survey_df, "Worn_Faded", NA)
  worn_faded_by_county <- summaryTableCreator(survey_df, "Worn_Faded", "County")
  worn_faded_by_city <- summaryTableCreator(survey_df, "Worn_Faded", "City")
  
  # No Crosswalk
  no_crosswalk <- summaryTableCreator(survey_df, "No_Crosswalk", NA)
  no_crosswalk_by_county <- summaryTableCreator(survey_df, "No_Crosswalk", "County")
  no_crosswalk_by_city <- summaryTableCreator(survey_df, "No_Crosswalk", "City")
  
  crosswalk_detail <- survey_df %>% 
    distinct(Stop_ID, City, County, Main_Street_Crosswalk, Cross_Street_Crosswalk, Worn_Faded, No_Crosswalk,
             Total_Weekly_Avg_Pre_COVID_Boardings, Total_Weekly_Avg_Pre_COVID_Alightings,
             Total_Weekly_Avg_Pre_COVID_Ons_Offs, Total_Weekly_Avg_Post_COVID_Boardings, Total_Weekly_Avg_Post_COVID_Alightings,
             Total_Weekly_Avg_Post_COVID_Ons_Offs) %>%
    mutate(Crosswalk_Category = ifelse(Main_Street_Crosswalk == "Yes" & Cross_Street_Crosswalk == "No", "Main Street Crosswalk Only",
                                       ifelse(Main_Street_Crosswalk == "No" & Cross_Street_Crosswalk == "Yes", "Cross Street Crosswalk Only",
                                              ifelse(Main_Street_Crosswalk == "Yes" & Cross_Street_Crosswalk == "Yes", "Main and Cross Street Crosswalk",
                                                     ifelse(Main_Street_Crosswalk == "Yes" & Cross_Street_Crosswalk == "Yes", "No Crosswalk", "")))))
  
  crosswalks_category <- summaryTableCreator(crosswalk_detail, "Crosswalk_Category", NA)
  crosswalk_category_by_county <- summaryTableCreator(crosswalk_detail, "Crosswalk_Category", "county")
  crosswalk_category_by_city <- summaryTableCreator(crosswalk_detail, "Crosswalk_Category", "city")
  
  crosswalks_summary <- crosswalks %>% 
    select(Stop_ID, Main_Street_Only, Cross_Street_Only, Main_and_Cross, No_Crosswalk) %>%
    mutate(Verify = str_count(paste(Main_Street_Only, Cross_Street_Only, Main_and_Cross, No_Crosswalk), "Yes") )
    gather(Crosswalk_Type, Answer, Main_Street_Only:No_Crosswalk) %>%
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
  # Traffic Light
  traffic_light <- summaryTableCreator(survey_df, "Traffic_Light", NA)
  traffic_light_by_county <- summaryTableCreator(survey_df, "Traffic_Light", "County")
  traffic_light_by_city <- summaryTableCreator(survey_df, "Traffic_Light", "City")
  
  # Curb Cuts
  curb_cuts <- summaryTableCreator(survey_df, "Curb_Cuts", NA)
  curb_cuts_by_county <- summaryTableCreator(survey_df, "Curb_Cuts", "County")
  curb_cuts_by_city <- summaryTableCreator(survey_df, "Curb_Cuts", "City")
  
  # Crosswalk Signals
  crosswalk_signals <- summaryTableCreator(survey_df, "Crosswalk_Signals", NA)
  crosswalk_signals_by_county <- summaryTableCreator(survey_df, "Crosswalk_Signals", "County")
  crosswalk_signals_by_city <- summaryTableCreator(survey_df, "Crosswalk_Signals", "City")
  
  # Crossing Audio
  crossing_audio <- summaryTableCreator(survey_df, "Crossing_Audio", NA)
  crossing_audio_by_county <- summaryTableCreator(survey_df, "Crossing_Audio", "County")
  crossing_audio_by_city <- summaryTableCreator(survey_df, "Crossing_Audio", "City")
  
  #Tactile Guide Strips
  tactile_guide <- summaryTableCreator(survey_df, "Tactile_Guide", NA)
  tactile_guide_by_county <- summaryTableCreator(survey_df, "Tactile_Guide", "County")
  tactile_guide_by_city <- summaryTableCreator(survey_df, "Tactile_Guide", "City")
  
  crosswalk_features <- srg_survey_df %>% distinct(Stop_ID, Crosswalk_Features) %>%
    dplyr::filter(!is.na(Crosswalk_Features)) %>%
    mutate(Traffic_Light = ifelse(grepl("Traffic_Light", Crosswalk_Features, fixed = TRUE), 1, 0),
           Curb_Cuts = ifelse(grepl("Curb cuts for wheelchairs",Crosswalk_Features, fixed = TRUE), 1, 0),
           Crosswalk_Signals = ifelse(grepl("Crosswalk signals with push buttons", Crosswalk_Features, fixed = TRUE), 1,0),
           Crossing_Audio = ifelse(grepl("Crossing audio overlays for the visually impaired", Crosswalk_Features, fixed = TRUE), 1, 0),
           Tactile_Guide = ifelse(grepl("Tactile guide strips for the visually impaired", Crosswalk_Features, fixed = TRUE), 1, 0),
           Stop_Counter = 1)
  
  
  crosswalk_features <- survey_df %>% 
    select(Traffic_Light, Curb_Cuts, Crosswalk_Signals, 
           Crossing_Audio, Tactile_Guide) %>%
    summarize(Traffic_Light = sum(Traffic_Light == "Yes"),
              Curb_Cuts = sum(Curb_Cuts == "Yes"),
              Crosswalk_Signals = sum(Crosswalk_Signals == "Yes"),
              Crossing_Audio = sum(Crossing_Audio == "Yes"),
              Tactile_Guide = sum(Tactile_Guide == "Yes"),
              Total_Stops = n()) %>%
    gather("Crosswalk_Feature", "Count", -Total_Stops) %>%
    mutate(Percent_Total = format(Count / Total_Stops, digits = 2)) %>%
    select(Crosswalk_Feature, Count, Percent_Total)
  
  crosswalk_features_by_county <- survey_df %>% 
    select(County, Traffic_Light, Curb_Cuts, Crosswalk_Signals, 
           Crossing_Audio, Tactile_Guide) %>%
    group_by(County) %>%
    summarize(Traffic_Light = sum(Traffic_Light == "Yes"),
              Curb_Cuts = sum(Curb_Cuts == 'Yes'),
              Crosswalk_Signals = sum(Crosswalk_Signals == "Yes"),
              Crossing_Audio = sum(Crossing_Audio == "Yes"),
              Tactile_Guide = sum(Tactile_Guide == "Yes"),
              Total_Stops = n()) 
  
  crosswalk_features_by_city <- survey_df %>% 
    select(City, Traffic_Light, Curb_Cuts, Crosswalk_Signals, 
           Crossing_Audio, Tactile_Guide) %>%
    group_by(City) %>%
    summarize(Traffic_Light = sum(Traffic_Light == "Yes"),
              Curb_Cuts = sum(Curb_Cuts == 'Yes'),
              Crosswalk_Signals = sum(Crosswalk_Signals == "Yes"),
              Crossing_Audio = sum(Crossing_Audio == "Yes"),
              Tactile_Guide = sum(Tactile_Guide == "Yes"),
              Total_Stops = n()) 
  
  
  ## Observations
  # Informal Pathways
  informal_pathways <- summaryTableCreator(survey_df, "Informal_Pathways", NA)
  informal_pathways_by_county <- summaryTableCreator(survey_df, "Informal_Pathways", "County")
  informal_pathways_by_city <- summaryTableCreator(survey_df, "Informal_Pathways", "City")
  
  
  # Compete For Seat
  compete_for_seat <- summaryTableCreator(survey_df, "Compete_For_Seat", NA)
  compete_for_seat_by_county <- summaryTableCreator(survey_df, "Compete_For_Seat", "County")
  compete_for_seat_by_city <- summaryTableCreator(survey_df, "Compete_For_Seat", "City")
  
  # Cross Midblock
  cross_midblock <- summaryTableCreator(survey_df, "Cross_Midblock", NA)
  cross_midblock_by_county <- summaryTableCreator(survey_df, "Cross_Midblock", "County")
  cross_midblock_by_city <- summaryTableCreator(survey_df, "Cross_Midblock", "City")
  
  # Catch The Bus
  catch_the_bus <- summaryTableCreator(survey_df, "Catch_The_Bus", NA)
  catch_the_bus_by_county <- summaryTableCreator(survey_df, "Catch_The_Bus", "County")
  catch_the_bus_by_city <- summaryTableCreator(survey_df, "Catch_The_Bus", "City")
  
  # Dangerous Motorists
  dangerous_motorists <- summaryTableCreator(survey_df, "Dangerous_Motorists", NA)
  dangerous_motorists_by_county <- summaryTableCreator(survey_df, "Dangerous_Motorists", "County")
  dangerous_motorists_by_city <- summaryTableCreator(survey_df, "Dangerous_Motorists", "City")
  
  # First Visit
  first_visit <- summaryTableCreator(survey_df, "First_Visit", NA)
  first_visit_by_county <- summaryTableCreator(survey_df, "First_Visit", "County")
  first_visit_by_city <- summaryTableCreator(survey_df, "First_Visit", "City")
  
  # Regular User None
  regular_user_none <- summaryTableCreator(survey_df, "Regular_User_None", NA)
  regular_user_none_by_county <- summaryTableCreator(survey_df, "Regular_User_None", "County")
  regular_user_none_by_city <- summaryTableCreator(survey_df, "Regular_User_None", "City")
  
  
  observations <- survey_df %>% 
    select(Informal_Pathways, Compete_For_Seat, Cross_Midblock, 
           Catch_The_Bus, Dangerous_Motorists, First_Visit, Regular_User_None) %>%
    summarize(Informal_Pathways = sum(Informal_Pathways == "Yes"),
              Compete_For_Seat = sum(Compete_For_Seat == "Yes"),
              Cross_Midblock = sum(Cross_Midblock == "Yes"),
              Catch_The_Bus = sum(Catch_The_Bus == 'Yes'),
              Dangerous_Motorists = sum(Dangerous_Motorists == "Yes"),
              First_Visit = sum(First_Visit == "Yes"),
              Regular_User_None = sum(Regular_User_None == "Yes"),
              Total_Stops = n()) %>%
    gather("Observed_Behavior", "Count", -Total_Stops) %>%
    mutate(Percent_Total = format(Count / Total_Stops, digits = 2)) %>%
    select(Observed_Behavior, Count, Percent_Total)
  
  observations_by_county <- survey_df %>% 
    select(County, Informal_Pathways, Compete_For_Seat, Cross_Midblock, 
           Catch_The_Bus, Dangerous_Motorists, First_Visit, Regular_User_None) %>%
    group_by(County) %>%
    summarize(Informal_Pathways = sum(Informal_Pathways == "Yes"),
              Compete_For_Seat = sum(Compete_For_Seat == "Yes"),
              Cross_Midblock = sum(Cross_Midblock == "Yes"),
              Catch_The_Bus = sum(Catch_The_Bus == 'Yes'),
              Dangerous_Motorists = sum(Dangerous_Motorists == "Yes"),
              First_Visit = sum(First_Visit == "Yes"),
              Regular_User_None = sum(Regular_User_None == "Yes"),
              Total_Stops = n())
  
  observations <- survey_df %>% 
    select(Informal_Pathways, Compete_For_Seat, Cross_Midblock, 
           Catch_The_Bus, Dangerous_Motorists, First_Visit, Regular_User_None) %>%
    summarize(Informal_Pathways = sum(Informal_Pathways == "Yes"),
              Compete_For_Seat = sum(Compete_For_Seat == "Yes"),
              Cross_Midblock = sum(Cross_Midblock == "Yes"),
              Catch_The_Bus = sum(Catch_The_Bus == 'Yes'),
              Dangerous_Motorists = sum(Dangerous_Motorists == "Yes"),
              First_Visit = sum(First_Visit == "Yes"),
              Regular_User_None = sum(Regular_User_None == "Yes"),
              Total_Stops = n())
  
  
  #Modal Analysis
  
  modal_summary <- survey_df %>% 
    distinct(Stop_ID, Seating, Shelter, Trash_Can, Route_Number, Route_Schedule, Route_Map, Customer_Service, None_Of_The_Above, 
           Wayfinding_Accessibility, Sidewalk, Boarding_Area, Main_Street_Crosswalk, Cross_Street_Crosswalk, 
           Worn_Faded, No_Crosswalk, Traffic_Light, Curb_Cuts, Crosswalk_Signals, Crossing_Audio, Tactile_Guide,
           Total_Weekly_Avg_Pre_COVID_Boardings, Total_Weekly_Avg_Pre_COVID_Alightings,
           Total_Weekly_Avg_Pre_COVID_Ons_Offs, Total_Weekly_Avg_Post_COVID_Boardings, Total_Weekly_Avg_Post_COVID_Alightings,
           Total_Weekly_Avg_Post_COVID_Ons_Offs) %>%
    group_by(Seating, Shelter, Trash_Can, Route_Number, Route_Schedule, Route_Map, Customer_Service, None_Of_The_Above, 
             Wayfinding_Accessibility, Sidewalk, Boarding_Area, Main_Street_Crosswalk, Cross_Street_Crosswalk, 
             Worn_Faded, No_Crosswalk, Traffic_Light, Curb_Cuts, Crosswalk_Signals, Crossing_Audio, Tactile_Guide) %>%
    summarize(Count = n(),
              Cum_Weekly_Avg_Pre_COVID_Boardings = sum(Total_Weekly_Avg_Pre_COVID_Boardings, na.rm = TRUE),
              Cum_Weekly_Avg_Pre_COVID_Alightings = sum(Total_Weekly_Avg_Pre_COVID_Alightings, na.rm = TRUE),
              Cum_Weekly_Avg_Pre_COVID_Ons_Offs = sum(Total_Weekly_Avg_Pre_COVID_Ons_Offs, na.rm = TRUE),
              Cum_Weekly_Avg_Post_COVID_Boardings = sum(Total_Weekly_Avg_Post_COVID_Boardings, na.rm = TRUE),
              Cum_Weekly_Avg_Post_COVID_Alightings = sum(Total_Weekly_Avg_Post_COVID_Alightings, na.rm = TRUE),
              Cum_Weekly_Avg_Post_COVID_Ons_Offs = sum(Total_Weekly_Avg_Post_COVID_Ons_Offs, na.rm = TRUE))

  
  # Grading 
  grades <- survey_df %>%
                mutate(Grade = ifelse(Shelter == "Yes", 10, 0) +
                               ifelse(Seating == "No, there is no seating", 0, 10) +
                               ifelse(str_count(paste(Route_Number,  Route_Schedule,  Route_Map, sep = ""), "Yes") == 0 & Customer_Service == "Yes", 0, 
                                      ifelse(str_count(paste(Route_Number, Route_Schedule, Route_Map, sep = ""), "Yes") >= 1 & Wayfinding_Accessibility == "No", 4, 
                                             ifelse(str_count(paste(Route_Number, Route_Schedule, Route_Map, sep = ""), "Yes") >= 1 & Wayfinding_Accessibility == "Yes", 8, 0))) +
                               ifelse(Sidewalk == "Yes, in both directions" | Sidewalk == 'Yes, in only one direction', 25, 0) +
                               ifelse(Trash_Can == "Yes", 2, 0) +
                               ifelse(No_Crosswalk == "Yes", 0, 
                                      ifelse(Main_Street_Crosswalk == "Yes" & 
                                               str_count(paste(Traffic_Light, Curb_Cuts, Crosswalk_Signals, Crossing_Audio, Tactile_Guide, sep = ""), "Yes") == 0, 5, 
                                      ifelse(Main_Street_Crosswalk == "Yes" &
                                             str_count(paste(Traffic_Light, Curb_Cuts, Crosswalk_Signals, Crossing_Audio, Tactile_Guide, sep = ""), "Yes") > 0 &
                                             str_count(paste(Traffic_Light, Curb_Cuts, Crosswalk_Signals, Crossing_Audio, Tactile_Guide, sep = ""), "Yes") <= 2, 15,
                                      ifelse(Main_Street_Crosswalk == "Yes" &
                                               str_count(paste(Traffic_Light, Curb_Cuts, Crosswalk_Signals, Crossing_Audio, Tactile_Guide, sep = ""), "Yes") > 2 &
                                               str_count(paste(Traffic_Light, Curb_Cuts, Crosswalk_Signals, Crossing_Audio, Tactile_Guide, sep = ""), "Yes") <= 5, 25, 0)))) +
                               ifelse(Boarding_Area == "Grass or dirt" | Boarding_Area == "Gravel", 0,
                                      ifelse(Boarding_Area == "Asphalt", 5, 20)),
                       Letter_Grade = case_when(
                                          Grade >= 0 & Grade <= 59 ~ "F",
                                          Grade > 59 & Grade <= 69 ~ "D",
                                          Grade > 69 & Grade <= 79 ~ "C",
                                          Grade > 79 & Grade <= 89 ~ "B",
                                          Grade > 89 & Grade <= 100 ~ "A",
                                          TRUE ~ ""
                       ))

  grade_detail <- grades %>% 
                      select(Stop_ID, Letter_Grade, Grade, Seating,
                             Shelter, Trash_Can, Route_Number, Route_Map,
                             Route_Schedule, Wayfinding_Accessibility,
                             Sidewalk, Main_Street_Crosswalk, Boarding_Area,
                             Traffic_Light, Curb_Cuts, Crossing_Audio, Crosswalk_Signals, Tactile_Guide) %>%
                      dplyr::filter(Grade >= 80 & Grade < 90) 
  
  grade_summary <- summaryTableCreator(grades, "Letter_Grade", NA)
    
  grade_list <- grades$Grade
  hist(grade_list)
  
  ######################### EXPORT RESULTS #########################
  saveWorkbook(wb, here("data","tidy_data","Bus_Stop_Census_Results.xlsx", overwrite = TRUE)
