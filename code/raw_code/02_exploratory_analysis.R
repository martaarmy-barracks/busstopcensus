
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
    select(Stop.ID, Total_Weekly_Avg_Pre_COVID_Boardings, Total_Weekly_Avg_Pre_COVID_Alightings,
           Total_Weekly_Avg_Pre_COVID_Ons_Offs, Total_Weekly_Avg_Post_COVID_Boardings, Total_Weekly_Avg_Post_COVID_Alightings,
           Total_Weekly_Avg_Post_COVID_Ons_Offs) %>%
    group_by(Stop.ID) %>%
    summarize(Total_Weekly_Avg_Pre_COVID_Boardings = sum(Total_Weekly_Avg_Pre_COVID_Boardings, na.rm = TRUE),
              Total_Weekly_Avg_Pre_COVID_Alightings = sum(Total_Weekly_Avg_Pre_COVID_Alightings, na.rm = TRUE),
              Total_Weekly_Avg_Pre_COVID_Ons_Offs = sum(Total_Weekly_Avg_Pre_COVID_Ons_Offs, na.rm = TRUE),
              Total_Weekly_Avg_Post_COVID_Boardings = sum(Total_Weekly_Avg_Post_COVID_Boardings, na.rm = TRUE),
              Total_Weekly_Avg_Post_COVID_Alightings = sum(Total_Weekly_Avg_Post_COVID_Alightings, na.rm = TRUE),
              Total_Weekly_Avg_Post_COVID_Ons_Offs = sum(Total_Weekly_Avg_Post_COVID_Ons_Offs, na.rm = TRUE))
  
  boarding_alighting_summary$Stop.ID <- as.character(boarding_alighting_summary$Stop.ID)
  
  duplicate_stops <- boarding_alighting_summary[duplicated(boarding_alighting_summary$Stop.ID), ]
  
  survey_df <- survey_df %>%
    inner_join(boarding_alighting_summary, by = c("Stop_ID" = "Stop.ID")) 
  
  # Import Legend
  legend <- read.csv(here("data", "raw_data", "supplemental", "Summary Data Legend.csv"), header = TRUE)
  
  ######################### CREATE EXCEL WORKBOOK FOR FINDINGS #########################
  wb <- createWorkbook()
  
  addWorksheet(wb, "Legend")
  addWorksheet(wb, "Survey Responses")
  addWorksheet(wb, "Boarding and Alighting Data")
  addWorksheet(wb, "Boarding and Alighting Summary")
  addWorksheet(wb, "Survey Statistics")
  addWorksheet(wb, "Seating")
  addWorksheet(wb, "Shelter")
  addWorksheet(wb, "Trash Can")
  addWorksheet(wb, "Cleanliness")
  addWorksheet(wb, "Line of Sight")
  addWorksheet(wb, "Route Number")
  addWorksheet(wb, "Route Schedule")
  addWorksheet(wb, "Route Map")
  addWorksheet(wb, "Wayfinding by Category")
  addWorksheet(wb, "Wayfinding Summary")
  addWorksheet(wb, "Wayfinding Accessibility")
  addWorksheet(wb, "Lighting")
  addWorksheet(wb, "Sidewalks")
  addWorksheet(wb, "Obstacles")
  addWorksheet(wb, "Obstacle Descriptions")
  addWorksheet(wb, "Boarding Area")
  addWorksheet(wb, "Main Street Crosswalk")
  addWorksheet(wb, "Cross Street Crosswalk")
  addWorksheet(wb, "Worn or Faded Crosswalk")
  addWorksheet(wb, "No Crosswalk")
  addWorksheet(wb, "Crosswalks Summary")
  addWorksheet(wb, "Traffic Lights")
  addWorksheet(wb, "Curb Cuts")
  addWorksheet(wb, "Crosswalk Signals")
  addWorksheet(wb, "Crossing Audio")
  addWorksheet(wb, "Tactile Guide Strips")
  addWorksheet(wb, "Crosswalk Features Summary")
  addWorksheet(wb, "Informal Pathways")
  addWorksheet(wb, "Competing for Seating")
  addWorksheet(wb, "Crossing Midblock")
  addWorksheet(wb, "Running to Catch Bus")
  addWorksheet(wb, "Dangerous Motorist Behavior")
  addWorksheet(wb, "First Time User - None Observed")
  addWorksheet(wb, "Regular User - None Observed")
  addWorksheet(wb, "Observed Behavior Summary")
  addWorksheet(wb, "Additional Comments")
  addWorksheet(wb, "Modal Analysis")
  addWorksheet(wb, "Grading Detail")
  addWorksheet(wb, "Grading Summary")
  addWorksheet(wb, "Report Tables")
  
  writeData(wb, "Legend", legend, startCol = 1, startRow = 1, colNames = TRUE)
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
  
  survey_df$Line_of_Sight <- factor(survey_df$Line_of_Sight, 
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
  
  survey_df$Boarding_Area <- factor(survey_df$Boarding_Area,
                                levels = c("Concrete sidewalk",
                                           "Grass or dirt",
                                           "Asphalt",
                                           "Brick pavers",
                                           "Gravel"))
  
  survey_df$Main_Street_Crosswalk <- factor(survey_df$Main_Street_Crosswalk,
                                levels = c("Yes",
                                           "No"))

  survey_df$Cross_Street_Crosswalk <- factor(survey_df$Cross_Street_Crosswalk,
                                            levels = c("Yes",
                                                       "No"))
  
  survey_df$Worn_Faded <- factor(survey_df$Worn_Faded,
                                            levels = c("Yes",
                                                       "No"))
  
  survey_df$No_Crosswalk <- factor(survey_df$No_Crosswalk,
                                            levels = c("Yes",
                                                       "No"))
  
  survey_df$Traffic_Light <- factor(survey_df$Traffic_Light,
                                   levels = c("Yes",
                                              "No"))
  
  survey_df$Curb_Cuts <- factor(survey_df$Curb_Cuts,
                                   levels = c("Yes",
                                              "No"))
  
  survey_df$Crosswalk_Signals <- factor(survey_df$Crosswalk_Signals,
                                   levels = c("Yes",
                                              "No"))
  
  survey_df$Crossing_Audio <- factor(survey_df$Crossing_Audio,
                                   levels = c("Yes",
                                              "No"))
  
  survey_df$Tactile_Guide <- factor(survey_df$Tactile_Guide,
                                   levels = c("Yes",
                                              "No"))
  
  survey_df$Informal_Pathways <- factor(survey_df$Informal_Pathways,
                                   levels = c("Yes",
                                              "No"))
  
  survey_df$Compete_For_Seat <- factor(survey_df$Compete_For_Seat,
                                   levels = c("Yes",
                                              "No"))
  
  survey_df$Cross_Midblock <- factor(survey_df$Cross_Midblock,
                                   levels = c("Yes",
                                              "No"))
  
  survey_df$Catch_The_Bus <- factor(survey_df$Catch_The_Bus,
                                   levels = c("Yes",
                                              "No"))
  
  survey_df$Dangerous_Motorists <- factor(survey_df$Dangerous_Motorists,
                                   levels = c("Yes",
                                              "No"))
  
  ## ANALYSIS ###################################################################################################################
  # Survey Statistics
  total_surveys <- survey_df %>% 
    summarize(Total_Survey_Count = n())
  
  goal_summary <- survey_df %>% distinct(Stop_ID) %>%
    summarize(Total_Unique_Stops_surveyed = n(),
              Percent_of_Goal = n()/ 2500)
  
  ## Top Overall and Weekly Surveyor Results
  all_surveyors <- survey_df %>% distinct(Email_Masked, Stop_ID) %>%
    group_by(Email_Masked) %>%
    summarize(Total_Surveyed = n()) %>%
    mutate(Email_Masked = ifelse(Email_Masked == "", "Not Specified", Email_Masked))
  
  ## Routes Survey Completion
  route_stop_list <- route_data_2019 %>% distinct(route_short_name, route_long_name, stop_id) #%>%
  
  
  distinct_stop_list <- survey_df %>% distinct(Stop_ID) %>%
    mutate(Stop_ID_Join_Key = Stop_ID)
  
  route_stops_surveyed <- route_stop_list %>% left_join(distinct_stop_list, by = c("stop_id"="Stop_ID"), keep = TRUE) %>%
    select(route_short_name,route_long_name, stop_id, Stop_ID_Join_Key) %>%
    mutate(Stop_Survey_Count = ifelse(is.na(Stop_ID_Join_Key), 0, 1)) %>%
    group_by(route_short_name, route_long_name) %>%
    summarize(Total_Route_Stops = n(),
              Total_Route_Stops_Surveyed = sum(Stop_Survey_Count),
              Percent_Surveyed = Total_Route_Stops_Surveyed/ Total_Route_Stops)
  
  essential_service <- c("2","4", "5", "6", "12", "15", "19", "21", "26", "39", "40",
                         "42", "49", "50", "51", "55", "60","66", "71", "73","74","78","81",
                         "82", "83", "84", "86","87", "89", "95", "102", "107", "110",
                         "111","115", "116", "117", "120", "121", "124", "125", "172", "178","180",
                         "181","185", "186", "191", "192", "193", "196", "188", "816")
  
  total_stops_essential <- route_stop_list %>% 
    dplyr::filter(route_short_name %in% essential_service) %>%
    left_join(distinct_stop_list, by = c("stop_id"="Stop_ID"), keep = TRUE) %>%
    distinct(stop_id, Stop_ID_Join_Key) %>%
    mutate(Stop_Survey_Count = ifelse(is.na(Stop_ID_Join_Key), 0, 1)) %>%
    summarize(Total_Stops = n(),
              Total_Stops_Surveyed = sum(Stop_Survey_Count),
              Percent_Surveyed = Total_Stops_Surveyed/ Total_Stops)
  
  
  # Calculating County Percentages
  county_results <- final_stop_list %>% left_join(distinct_stop_list, by = c("stop_id"="Stop_ID"), keep = TRUE) %>%
    select(stop_id, Stop_ID_Join_Key, Stop_Lat, Stop_Lon) %>%
    mutate(County = map.where(database="county", 
                              Stop_Lon, Stop_Lat),
           County = case_when(
             County == "georgia,fulton" ~ "Fulton",
             County == "georgia,de kalb" ~ "Dekalb",
             County == "georgia,clayton" ~ "Clayton",
             County == "georgia,douglas" ~ "Douglas",
             County == "georgia,gwinnett" ~ "Gwinnett",
             County == "georgia,cobb" ~ "Cobb",
             TRUE ~ ""),
           Stop_Surveyed = ifelse(is.na(Stop_ID_Join_Key), 0, 1)) %>%
    group_by(County) %>%
    summarize(Total_Stops = n(),
              Total_Surveyed = sum(Stop_Surveyed),
              Percent_Surveyed = sum(Stop_Surveyed) / n())
  
  df_list <- list(total_surveys, goal_summary, route_stops_surveyed,
                  total_stops_essential, county_results)
  wb <- writeDataToExcel(df_list, wb, "Survey Statistics")
  
  # Seating
  seating <- summaryTableCreator(survey_df, "Seating", NA)
  seating_by_county <- summaryTableCreator(survey_df, "Seating", "County")
  seating_by_city <- summaryTableCreator(survey_df, "Seating", "City")
  
  df_list <- list(seating, seating_by_county, seating_by_city)
  wb <- writeDataToExcel(df_list, wb, "Seating")
  
  # Shelter
  shelter <- summaryTableCreator(survey_df, "Shelter", NA)
  shelter_by_county <- summaryTableCreator(survey_df, "Shelter", "County")
  shelter_by_city <- summaryTableCreator(survey_df, "Shelter", "City")
  
  df_list <- list(shelter, shelter_by_county, shelter_by_city)
  wb <- writeDataToExcel(df_list, wb, "Shelter")
  
  # Trash Can
  trash_can <- summaryTableCreator(survey_df, "Trash_Can", NA)
  trash_can_by_county <- summaryTableCreator(survey_df, "Trash_Can", "County")
  trash_can_by_city <- summaryTableCreator(survey_df, "Trash_Can", "City")
  
  df_list <- list(trash_can, trash_can_by_county, trash_can_by_city)
  wb <- writeDataToExcel(df_list, wb, "Trash Can")
  
  ## Cleanliness
  cleanliness <- survey_df %>% 
                          select(Litter, Grafitti, Overflow, Dirty_Seating, Other) %>%
                          summarize(Litter = sum(Litter == "Yes"),
                                    Grafitti = sum(Grafitti == "Yes"),
                                    Overflow = sum(Overflow == "Yes"),
                                    Dirty_Seating = sum(Dirty_Seating == "Yes"),
                                    Other = sum(Other == "Yes"),
                                    Total_Surveys = n()) %>%
                          gather("Cleanliness_Issue", "Survey_Count", -Total_Surveys) %>%
                          mutate(Percent_Total = format(Survey_Count / Total_Surveys, digits = 2)) %>%
                          select(Cleanliness_Issue, Survey_Count, Total_Surveys, Percent_Total)
  
  litter_with_trash_can <- survey_df %>%
    select(Litter, Trash_Can) %>%
    dplyr::filter(Litter == "Yes") %>%
    group_by(Litter, Trash_Can) %>%
    summarize(Survey_Count = n()) %>%
    ungroup() %>%
    mutate(Percent_Total = format(Survey_Count / sum(Survey_Count), digits = 2))
  
  cleanliness_by_county <- survey_df %>% 
    select(County, Litter, Grafitti, Overflow, Dirty_Seating, Other) %>%
    group_by(County) %>%
    summarize(Litter = sum(Litter == "Yes"),
              Grafitti = sum(Grafitti == "Yes"),
              Overflow = sum(Overflow == "Yes"),
              Dirty_Seating = sum(Dirty_Seating == "Yes"),
              Other = sum(Other == "Yes"),
              Total_Surveys = n()) 
   
  cleanliness_by_city <- survey_df %>% 
    select(City, Litter, Grafitti, Overflow, Dirty_Seating, Other) %>%
    group_by(City) %>%
    summarize(Litter = sum(Litter == "Yes"),
              Grafitti = sum(Grafitti == "Yes"),
              Overflow = sum(Overflow == "Yes"),
              Dirty_Seating = sum(Dirty_Seating == "Yes"),
              Other = sum(Other == "Yes"),
              Total_Surveys = n())
  
  df_list <- list(cleanliness, litter_with_trash_can, cleanliness_by_county, cleanliness_by_city)
  wb <- writeDataToExcel(df_list, wb, "Cleanliness")
  
  ## Line of Sight
  line_of_sight <- simpleSummaryTableCreator(survey_df, "Line_of_Sight", NA)
  line_of_sight_by_county <- simpleSummaryTableCreator(survey_df, "Line_of_Sight", "County")
  line_of_sight_by_city <- simpleSummaryTableCreator(survey_df, "Line_of_Sight", "City")

  df_list <- list(line_of_sight, line_of_sight_by_county, line_of_sight_by_city)
  wb <- writeDataToExcel(df_list, wb, "Line of Sight")
  
  ## Wayfinding
  # Route Number
  route_number <- summaryTableCreator(survey_df, "Route_Number", NA)
  route_number_by_county <- summaryTableCreator(survey_df, "Route_Number", "County")
  route_number_by_city <- summaryTableCreator(survey_df, "Route_Number", "City")
  
  df_list <- list(route_number, route_number_by_county, route_number_by_city)
  wb <- writeDataToExcel(df_list, wb, "Route Number")
  
  # Route Schedule
  route_schedule <- summaryTableCreator(survey_df, "Route_Schedule", NA)
  route_schedule_by_county <- summaryTableCreator(survey_df, "Route_Schedule", "County")
  route_schedule_by_city <- summaryTableCreator(survey_df, "Route_Schedule", "City")
  
  df_list <- list(route_schedule, route_schedule_by_county, route_schedule_by_city)
  wb <- writeDataToExcel(df_list, wb, "Route Schedule")
  
  # Route Map
  route_map <- summaryTableCreator(survey_df, "Route_Map", NA)
  route_map_by_county <- summaryTableCreator(survey_df, "Route_Map", "County")
  route_map_by_city <- summaryTableCreator(survey_df, "Route_Map", "City")
  
  df_list <- list(route_map, route_map_by_county, route_map_by_city)
  wb <- writeDataToExcel(df_list, wb, "Route Map")
  
  # Wayfinding By Category
  wayfinding_summary <- survey_df %>% 
                            distinct(Stop_ID, Route_Number, Route_Schedule, Route_Map,
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
    distinct(Stop_ID, City, County, Shelter, Route_Number, Route_Schedule, Route_Map, Customer_Service,
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
  
  wayfinding_with_shelter <- wayfinding_detail %>%
    select(Wayfinding_Category, Shelter) %>%
    group_by(Wayfinding_Category, Shelter) %>%
    summarize(Stop_Count = n()) %>%
    ungroup() %>%
    mutate(Percent_Total = format(Stop_Count / sum(Stop_Count), digits = 2))
  
  wayfinding_detail <- wayfinding_detail %>% select(-Shelter)
  
  wayfinding_category <- summaryTableCreator(wayfinding_detail, "Wayfinding_Category", NA)
  wayfinding_category_by_county <- summaryTableCreator(wayfinding_detail, "Wayfinding_Category", "County")
  wayfinding_category_by_city <- summaryTableCreator(wayfinding_detail, "Wayfinding_Category", "City")
  
  df_list <- list(wayfinding_category, wayfinding_category_by_county, wayfinding_category_by_city)
  wb <- writeDataToExcel(df_list, wb, "Wayfinding by Category")

  #Wayfinding Summary
  wayfinding_by_county <- survey_df %>% 
    distinct(Stop_ID, County, Route_Number, Route_Schedule, Route_Map,
           Customer_Service, None_Of_The_Above) %>%
    group_by(County) %>%
    summarize(Route_Number = sum(Route_Number == "Yes"),
              Route_Schedule = sum(Route_Schedule == "Yes"),
              Route_Map = sum(Route_Map == "Yes"),
              Customer_Service = sum(Customer_Service == "Yes"),
              None_Of_The_Above = sum(None_Of_The_Above == "Yes"),
              Total_Stops = n())
  
  wayfinding_by_city <- survey_df %>% 
    distinct(Stop_ID, City, Route_Number, Route_Schedule, Route_Map,
           Customer_Service, None_Of_The_Above) %>%
    group_by(City) %>%
    summarize(Route_Number = sum(Route_Number == "Yes"),
              Route_Schedule = sum(Route_Schedule == "Yes"),
              Route_Map = sum(Route_Map == "Yes"),
              Customer_Service = sum(Customer_Service == "Yes"),
              None_Of_The_Above = sum(None_Of_The_Above == "Yes"),
              Total_Stops = n())
  
  df_list <- list(wayfinding_by_county, wayfinding_by_city)
  wb <- writeDataToExcel(df_list, wb, "Wayfinding Summary")
  
  
  ## Wayfinding Accessibility
  wayfinding_accessibility <- summaryTableCreator(survey_df, "Wayfinding_Accessibility", NA)
  wayfinding_accessibility_by_county <- summaryTableCreator(survey_df, "Wayfinding_Accessibility", "County")
  wayfinding_accessibility_by_city <- summaryTableCreator(survey_df, "Wayfinding_Accessibility", "City")
  
  df_list <- list(wayfinding_accessibility, wayfinding_accessibility_by_county, wayfinding_accessibility_by_city)
  wb <- writeDataToExcel(df_list, wb, "Wayfinding Accessibility")
  
  ## Lighting
  lighting <- simpleSummaryTableCreator(survey_df, "Lighting", NA)
  lighting_by_county <- simpleSummaryTableCreator(survey_df, "Lighting", "County")
  lighting_by_city <- simpleSummaryTableCreator(survey_df, "Lighting", "City")
  
  df_list <- list(lighting, lighting_by_county, lighting_by_city)
  wb <- writeDataToExcel(df_list, wb, "Lighting")
  
  ## Sidewalks
  sidewalk <- summaryTableCreator(survey_df, "Sidewalk", NA)
  sidewalk_by_county <- summaryTableCreator(survey_df, "Sidewalk", "County")
  sidewalk_by_city <- summaryTableCreator(survey_df, "Sidewalk", "City")  
  
  df_list <- list(sidewalk, sidewalk_by_county, sidewalk_by_city)
  wb <- writeDataToExcel(df_list, wb, "Sidewalks")
  
  ## Obstacles
  obstacles <- simpleSummaryTableCreator(survey_df, "Obstacles", NA)
  obstacles_by_county <- simpleSummaryTableCreator(survey_df, "Obstacles", "County")
  obstacles_by_city <- simpleSummaryTableCreator(survey_df, "Obstacles", "City")
   
  obstacles_description <- survey_df %>% 
    distinct(Stop_ID, Main_Street_or_Station, Nearest_Landmark, County,
             City, Routes, Direction, Obstacles, Obstacle_Desc) %>%
    dplyr::filter(Obstacles == "Yes", Obstacle_Desc != "")
  
  df_list <- list(obstacles, obstacles_by_county, obstacles_by_city)
  wb <- writeDataToExcel(df_list, wb, "Obstacles")
  writeData(wb, "Obstacle Descriptions", obstacles_description, startCol = 1, startRow = 1, colNames = TRUE)
  
  ## Boarding Areas
  boarding_area <- summaryTableCreator(survey_df, "Boarding_Area", NA)
  boarding_area_by_county <- summaryTableCreator(survey_df, "Boarding_Area", "County")
  boarding_area_by_city <- summaryTableCreator(survey_df, "Boarding_Area", "City")
  
  df_list <- list(boarding_area, boarding_area_by_county, boarding_area_by_city)
  wb <- writeDataToExcel(df_list, wb, "Boarding Area")
  
  ## Crosswalks
  # Main Street Crosswalk
  main_street_crosswalk <- summaryTableCreator(survey_df, "Main_Street_Crosswalk", NA)
  main_street_crosswalk_by_county <- summaryTableCreator(survey_df, "Main_Street_Crosswalk", "County")
  main_street_crosswalk_by_city <- summaryTableCreator(survey_df, "Main_Street_Crosswalk", "City")
  
  df_list <- list(main_street_crosswalk, main_street_crosswalk_by_county, main_street_crosswalk_by_city)
  wb <- writeDataToExcel(df_list, wb, "Main Street Crosswalk")
  
  # Cross Street Crosswalk
  cross_street_crosswalk <- summaryTableCreator(survey_df, "Cross_Street_Crosswalk", NA)
  cross_street_crosswalk_by_county <- summaryTableCreator(survey_df, "Cross_Street_Crosswalk", "County")
  cross_street_crosswalk_by_city <- summaryTableCreator(survey_df, "Cross_Street_Crosswalk", "City")
  
  df_list <- list(cross_street_crosswalk, cross_street_crosswalk_by_county, cross_street_crosswalk_by_city)
  wb <- writeDataToExcel(df_list, wb, "Cross Street Crosswalk")
  
  # Worn and Faded Crosswalk
  worn_faded <- summaryTableCreator(survey_df, "Worn_Faded", NA)
  worn_faded_by_county <- summaryTableCreator(survey_df, "Worn_Faded", "County")
  worn_faded_by_city <- summaryTableCreator(survey_df, "Worn_Faded", "City")
  
  df_list <- list(worn_faded, worn_faded_by_county, worn_faded_by_city)
  wb <- writeDataToExcel(df_list, wb, "Worn or Faded Crosswalk")
  
  # No Crosswalk
  no_crosswalk <- summaryTableCreator(survey_df, "No_Crosswalk", NA)
  no_crosswalk_by_county <- summaryTableCreator(survey_df, "No_Crosswalk", "County")
  no_crosswalk_by_city <- summaryTableCreator(survey_df, "No_Crosswalk", "City")
  
  df_list <- list(no_crosswalk, no_crosswalk_by_county, no_crosswalk_by_city)
  wb <- writeDataToExcel(df_list, wb, "No Crosswalk")
  
  crosswalk_detail <- survey_df %>% 
    distinct(Stop_ID, City, County, Main_Street_Crosswalk, Cross_Street_Crosswalk, Worn_Faded, No_Crosswalk,
             Total_Weekly_Avg_Pre_COVID_Boardings, Total_Weekly_Avg_Pre_COVID_Alightings,
             Total_Weekly_Avg_Pre_COVID_Ons_Offs, Total_Weekly_Avg_Post_COVID_Boardings, Total_Weekly_Avg_Post_COVID_Alightings,
             Total_Weekly_Avg_Post_COVID_Ons_Offs) %>%
    mutate(Crosswalk_Category = ifelse(Main_Street_Crosswalk == "Yes" & Cross_Street_Crosswalk == "No", "Main Street Crosswalk Only",
                                       ifelse(Main_Street_Crosswalk == "No" & Cross_Street_Crosswalk == "Yes", "Cross Street Crosswalk Only",
                                              ifelse(Main_Street_Crosswalk == "Yes" & Cross_Street_Crosswalk == "Yes", "Main and Cross Street Crosswalk",
                                                     ifelse(Main_Street_Crosswalk == "No" & Cross_Street_Crosswalk == "No", "No Crosswalk", "")))))
  
  crosswalk_category <- summaryTableCreator(crosswalk_detail, "Crosswalk_Category", NA)
  crosswalk_category_by_county <- summaryTableCreator(crosswalk_detail, "Crosswalk_Category", "County")
  crosswalk_category_by_city <- summaryTableCreator(crosswalk_detail, "Crosswalk_Category", "City")
  
  df_list <- list(crosswalk_category, crosswalk_category_by_county, crosswalk_category_by_city)
  wb <- writeDataToExcel(df_list, wb, "Crosswalks Summary")
  
  ## Crosswalk Features
  # Traffic Light
  traffic_light <- summaryTableCreator(survey_df, "Traffic_Light", NA)
  traffic_light_by_county <- summaryTableCreator(survey_df, "Traffic_Light", "County")
  traffic_light_by_city <- summaryTableCreator(survey_df, "Traffic_Light", "City")
  
  df_list <- list(traffic_light, traffic_light_by_county, traffic_light_by_city)
  wb <- writeDataToExcel(df_list, wb, "Traffic Lights")
  
  # Curb Cuts
  curb_cuts <- summaryTableCreator(survey_df, "Curb_Cuts", NA)
  curb_cuts_by_county <- summaryTableCreator(survey_df, "Curb_Cuts", "County")
  curb_cuts_by_city <- summaryTableCreator(survey_df, "Curb_Cuts", "City")
  
  df_list <- list(curb_cuts, curb_cuts_by_county, curb_cuts_by_city)
  wb <- writeDataToExcel(df_list, wb, "Curb Cuts")
  
  # Crosswalk Signals
  crosswalk_signals <- summaryTableCreator(survey_df, "Crosswalk_Signals", NA)
  crosswalk_signals_by_county <- summaryTableCreator(survey_df, "Crosswalk_Signals", "County")
  crosswalk_signals_by_city <- summaryTableCreator(survey_df, "Crosswalk_Signals", "City")
  
  df_list <- list(crosswalk_signals, crosswalk_signals_by_county, crosswalk_signals_by_city)
  wb <- writeDataToExcel(df_list, wb, "Crosswalk Signals")
  
  # Crossing Audio
  crossing_audio <- summaryTableCreator(survey_df, "Crossing_Audio", NA)
  crossing_audio_by_county <- summaryTableCreator(survey_df, "Crossing_Audio", "County")
  crossing_audio_by_city <- summaryTableCreator(survey_df, "Crossing_Audio", "City")
  
  df_list <- list(crossing_audio, crossing_audio_by_county, crossing_audio_by_city)
  wb <- writeDataToExcel(df_list, wb, "Crossing Audio")
  
  #Tactile Guide Strips
  tactile_guide <- summaryTableCreator(survey_df, "Tactile_Guide", NA)
  tactile_guide_by_county <- summaryTableCreator(survey_df, "Tactile_Guide", "County")
  tactile_guide_by_city <- summaryTableCreator(survey_df, "Tactile_Guide", "City")
  
  df_list <- list(tactile_guide, tactile_guide_by_county, tactile_guide_by_city)
  wb <- writeDataToExcel(df_list, wb, "Tactile Guide Strips")
  
  crosswalk_features <- survey_df %>% 
    distinct(Stop_ID, Traffic_Light, Curb_Cuts, Crosswalk_Signals, 
           Crossing_Audio, Tactile_Guide) %>%
    summarize(Traffic_Light = sum(Traffic_Light == "Yes"),
              Curb_Cuts = sum(Curb_Cuts == "Yes"),
              Crosswalk_Signals = sum(Crosswalk_Signals == "Yes"),
              Crossing_Audio = sum(Crossing_Audio == "Yes"),
              Tactile_Guide = sum(Tactile_Guide == "Yes"),
              Total_Stops = n()) %>%
    gather("Crosswalk_Feature", "Count", -Total_Stops) %>%
    mutate(Percent_Total = format(Count / Total_Stops, digits = 2)) %>%
    select(Crosswalk_Feature, Count, Total_Stops, Percent_Total)
  
  crosswalk_features_by_county <- survey_df %>% 
    distinct(Stop_ID, County, Traffic_Light, Curb_Cuts, Crosswalk_Signals, 
           Crossing_Audio, Tactile_Guide) %>%
    group_by(County) %>%
    summarize(Traffic_Light = sum(Traffic_Light == "Yes"),
              Curb_Cuts = sum(Curb_Cuts == 'Yes'),
              Crosswalk_Signals = sum(Crosswalk_Signals == "Yes"),
              Crossing_Audio = sum(Crossing_Audio == "Yes"),
              Tactile_Guide = sum(Tactile_Guide == "Yes"),
              Total_Stops = n()) 
  
  crosswalk_features_by_city <- survey_df %>% 
    distinct(Stop_ID, City, Traffic_Light, Curb_Cuts, Crosswalk_Signals, 
           Crossing_Audio, Tactile_Guide) %>%
    group_by(City) %>%
    summarize(Traffic_Light = sum(Traffic_Light == "Yes"),
              Curb_Cuts = sum(Curb_Cuts == 'Yes'),
              Crosswalk_Signals = sum(Crosswalk_Signals == "Yes"),
              Crossing_Audio = sum(Crossing_Audio == "Yes"),
              Tactile_Guide = sum(Tactile_Guide == "Yes"),
              Total_Stops = n()) 
  
  df_list <- list(crosswalk_features, crosswalk_features_by_county, crosswalk_features_by_city)
  wb <- writeDataToExcel(df_list, wb, "Crosswalk Features Summary")
  
  ## Observations
  # Informal Pathways
  informal_pathways <- simpleSummaryTableCreator(survey_df, "Informal_Pathways", NA)
  informal_pathways_by_county <- simpleSummaryTableCreator(survey_df, "Informal_Pathways", "County")
  informal_pathways_by_city <- simpleSummaryTableCreator(survey_df, "Informal_Pathways", "City")
  
  df_list <- list(informal_pathways, informal_pathways_by_county, informal_pathways_by_city)
  wb <- writeDataToExcel(df_list, wb, "Informal Pathways")
  
  # Compete For Seat
  compete_for_seat <- simpleSummaryTableCreator(survey_df, "Compete_For_Seat", NA)
  compete_for_seat_by_county <- simpleSummaryTableCreator(survey_df, "Compete_For_Seat", "County")
  compete_for_seat_by_city <- simpleSummaryTableCreator(survey_df, "Compete_For_Seat", "City")
  
  df_list <- list(compete_for_seat, compete_for_seat_by_county, compete_for_seat_by_city)
  wb <- writeDataToExcel(df_list, wb, "Competing for Seating")
  
  # Cross Midblock
  cross_midblock <- simpleSummaryTableCreator(survey_df, "Cross_Midblock", NA)
  cross_midblock_by_county <- simpleSummaryTableCreator(survey_df, "Cross_Midblock", "County")
  cross_midblock_by_city <- simpleSummaryTableCreator(survey_df, "Cross_Midblock", "City")
  
  df_list <- list(cross_midblock, cross_midblock_by_county, cross_midblock_by_city)
  wb <- writeDataToExcel(df_list, wb, "Crossing Midblock")
  
  # Catch The Bus
  catch_the_bus <- simpleSummaryTableCreator(survey_df, "Catch_The_Bus", NA)
  catch_the_bus_by_county <- simpleSummaryTableCreator(survey_df, "Catch_The_Bus", "County")
  catch_the_bus_by_city <- simpleSummaryTableCreator(survey_df, "Catch_The_Bus", "City")
  
  df_list <- list(catch_the_bus, catch_the_bus_by_county, catch_the_bus_by_city)
  wb <- writeDataToExcel(df_list, wb, "Running to Catch Bus")
  
  # Dangerous Motorists
  dangerous_motorists <- simpleSummaryTableCreator(survey_df, "Dangerous_Motorists", NA)
  dangerous_motorists_by_county <- simpleSummaryTableCreator(survey_df, "Dangerous_Motorists", "County")
  dangerous_motorists_by_city <- simpleSummaryTableCreator(survey_df, "Dangerous_Motorists", "City")
  
  df_list <- list(dangerous_motorists, dangerous_motorists_by_county, dangerous_motorists_by_city)
  wb <- writeDataToExcel(df_list, wb, "Dangerous Motorist Behavior")
  
  # First Visit
  first_visit <- simpleSummaryTableCreator(survey_df, "First_Visit", NA)
  first_visit_by_county <- simpleSummaryTableCreator(survey_df, "First_Visit", "County")
  first_visit_by_city <- simpleSummaryTableCreator(survey_df, "First_Visit", "City")
  
  df_list <- list(first_visit, first_visit_by_county, first_visit_by_city)
  wb <- writeDataToExcel(df_list, wb, "First Time User - None Observed")
  
  # Regular User None
  regular_user_none <- simpleSummaryTableCreator(survey_df, "Regular_User_None", NA)
  regular_user_none_by_county <- simpleSummaryTableCreator(survey_df, "Regular_User_None", "County")
  regular_user_none_by_city <- simpleSummaryTableCreator(survey_df, "Regular_User_None", "City")
  
  df_list <- list(regular_user_none, regular_user_none_by_county, regular_user_none_by_city)
  wb <- writeDataToExcel(df_list, wb, "Regular User - None Observed")
  
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
              Total_Surveys = n()) %>%
    gather("Observed_Behavior", "Survey_Count", -Total_Surveys) %>%
    mutate(Percent_Total = format(Survey_Count / Total_Surveys, digits = 2)) %>%
    select(Observed_Behavior, Survey_Count, Total_Surveys, Percent_Total)
  
  observations_by_county <- survey_df %>% 
    select(Stop_ID, County, Informal_Pathways, Compete_For_Seat, Cross_Midblock, 
           Catch_The_Bus, Dangerous_Motorists, First_Visit, Regular_User_None) %>%
    group_by(County) %>%
    summarize(Informal_Pathways = sum(Informal_Pathways == "Yes"),
              Compete_For_Seat = sum(Compete_For_Seat == "Yes"),
              Cross_Midblock = sum(Cross_Midblock == "Yes"),
              Catch_The_Bus = sum(Catch_The_Bus == 'Yes'),
              Dangerous_Motorists = sum(Dangerous_Motorists == "Yes"),
              First_Visit = sum(First_Visit == "Yes"),
              Regular_User_None = sum(Regular_User_None == "Yes"),
              Total_Surveys = n())
  
  observations_by_city <- survey_df %>% 
    select(Stop_ID, City, Informal_Pathways, Compete_For_Seat, Cross_Midblock, 
           Catch_The_Bus, Dangerous_Motorists, First_Visit, Regular_User_None) %>%
    group_by(City) %>%
    summarize(Informal_Pathways = sum(Informal_Pathways == "Yes"),
              Compete_For_Seat = sum(Compete_For_Seat == "Yes"),
              Cross_Midblock = sum(Cross_Midblock == "Yes"),
              Catch_The_Bus = sum(Catch_The_Bus == 'Yes'),
              Dangerous_Motorists = sum(Dangerous_Motorists == "Yes"),
              First_Visit = sum(First_Visit == "Yes"),
              Regular_User_None = sum(Regular_User_None == "Yes"),
              Total_Surveys = n())
  
  df_list <- list(observations, observations_by_county, observations_by_city)
  wb <- writeDataToExcel(df_list, wb, "Observed Behavior Summary")
  
  ## Anecdotes
  additional_comments <- survey_df %>% 
    distinct(Stop_ID, Main_Street_or_Station, Nearest_Landmark, County,
             City, Routes, Direction, Additional_Comments) %>%
    dplyr::filter(Additional_Comments != "")
  
  wb <- writeDataToExcel(list(additional_comments), wb, "Additional Comments")
  
  ## Modal Analysis
  modal_summary <- survey_df %>% 
    distinct(Stop_ID, Seating, Shelter, Trash_Can, Route_Number, Route_Schedule, Route_Map, Customer_Service, 
           Wayfinding_Accessibility, Sidewalk, Boarding_Area, Main_Street_Crosswalk, Cross_Street_Crosswalk,
           No_Crosswalk, Traffic_Light, Curb_Cuts, Crosswalk_Signals, Crossing_Audio, Tactile_Guide,
           Total_Weekly_Avg_Pre_COVID_Boardings, Total_Weekly_Avg_Pre_COVID_Alightings,
           Total_Weekly_Avg_Pre_COVID_Ons_Offs, Total_Weekly_Avg_Post_COVID_Boardings, Total_Weekly_Avg_Post_COVID_Alightings,
           Total_Weekly_Avg_Post_COVID_Ons_Offs) %>%
    mutate(Seating = ifelse(Seating == "No, there is no seating", "No", "Yes"),
           Wayfinding  = ifelse(str_count(paste(Route_Number, Route_Schedule, Route_Map, sep = ""), "Yes") > 0 & 
                                                       str_count(paste(Route_Number, Route_Schedule, Route_Map, sep = ""), "Yes") < 3 &
                                                       Customer_Service == "Yes", "Some Route Info and Customer Service Information",
                                                     ifelse(str_count(paste(Route_Number, Route_Schedule, Route_Map, sep = ""), "Yes") == 3 & 
                                                              Customer_Service == "Yes", "All Route Info and Customer Service Information", 
                                                            ifelse(str_count(paste(Route_Number,  Route_Schedule,  Route_Map, sep = ""), "Yes") == 0 & 
                                                                     Customer_Service == "Yes", "Customer Service Only", ""))),
           Crosswalks = ifelse(Main_Street_Crosswalk == "Yes" & Cross_Street_Crosswalk == "No", "Main Street Crosswalk Only",
                                       ifelse(Main_Street_Crosswalk == "No" & Cross_Street_Crosswalk == "Yes", "Cross Street Crosswalk Only",
                                              ifelse(Main_Street_Crosswalk == "Yes" & Cross_Street_Crosswalk == "Yes", "Main and Cross Street Crosswalk",
                                                     ifelse(Main_Street_Crosswalk == "No" & Cross_Street_Crosswalk == "No", "No Crosswalk", ""))))) %>%
    group_by(Seating, Shelter, Trash_Can, Wayfinding, Wayfinding_Accessibility, Sidewalk, Boarding_Area, 
             Crosswalks, Traffic_Light, Curb_Cuts, Crosswalk_Signals, Crossing_Audio, Tactile_Guide) %>%
    summarize(Stop_Count = n(),
              Cum_Weekly_Avg_Pre_COVID_Boardings = sum(Total_Weekly_Avg_Pre_COVID_Boardings, na.rm = TRUE),
              Cum_Weekly_Avg_Pre_COVID_Alightings = sum(Total_Weekly_Avg_Pre_COVID_Alightings, na.rm = TRUE),
              Cum_Weekly_Avg_Pre_COVID_Ons_Offs = sum(Total_Weekly_Avg_Pre_COVID_Ons_Offs, na.rm = TRUE),
              Cum_Weekly_Avg_Post_COVID_Boardings = sum(Total_Weekly_Avg_Post_COVID_Boardings, na.rm = TRUE),
              Cum_Weekly_Avg_Post_COVID_Alightings = sum(Total_Weekly_Avg_Post_COVID_Alightings, na.rm = TRUE),
              Cum_Weekly_Avg_Post_COVID_Ons_Offs = sum(Total_Weekly_Avg_Post_COVID_Ons_Offs, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Percent_Of_Total_Stops = format(Stop_Count / sum(Stop_Count), digits = 2),
           Percent_Cum_Weekly_Avg_Pre_COVID_Boardings = format(Cum_Weekly_Avg_Pre_COVID_Boardings / sum(Cum_Weekly_Avg_Pre_COVID_Boardings), digits = 8),
           Percent_Cum_Weekly_Avg_Pre_COVID_Alightings = format(Cum_Weekly_Avg_Pre_COVID_Alightings / sum(Cum_Weekly_Avg_Pre_COVID_Alightings), digits = 8),
           Percent_Cum_Weekly_Avg_Pre_COVID_Ons_Offs = format(Cum_Weekly_Avg_Pre_COVID_Ons_Offs / sum(Cum_Weekly_Avg_Pre_COVID_Ons_Offs), digits = 8),
           Percent_Cum_Weekly_Avg_Post_COVID_Boardings = format(Cum_Weekly_Avg_Post_COVID_Boardings / sum(Cum_Weekly_Avg_Post_COVID_Boardings), digits = 8),
           Percent_Cum_Weekly_Avg_Post_COVID_Alightings = format(Cum_Weekly_Avg_Post_COVID_Alightings / sum(Cum_Weekly_Avg_Post_COVID_Alightings), digits = 8),
           Percent_Cum_Weekly_Avg_Post_COVID_Ons_Offs = format(Cum_Weekly_Avg_Post_COVID_Ons_Offs / sum(Cum_Weekly_Avg_Post_COVID_Ons_Offs), digits = 8)) 

  wb <- writeDataToExcel(list(modal_summary), wb, "Modal Analysis")
  
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
                      distinct(Stop_ID, Stop_Lat, Stop_Lon, Letter_Grade, Grade, Seating,
                             Shelter, Trash_Can, Route_Number, Route_Map,
                             Route_Schedule, Wayfinding_Accessibility,
                             Sidewalk, Main_Street_Crosswalk, Boarding_Area,
                             Traffic_Light, Curb_Cuts, Crossing_Audio, Crosswalk_Signals, Tactile_Guide,
                             Total_Weekly_Avg_Pre_COVID_Boardings, Total_Weekly_Avg_Pre_COVID_Alightings,
                             Total_Weekly_Avg_Pre_COVID_Ons_Offs, Total_Weekly_Avg_Post_COVID_Boardings, Total_Weekly_Avg_Post_COVID_Alightings,
                             Total_Weekly_Avg_Post_COVID_Ons_Offs)
  
  duplicate_stops <- grade_detail[duplicated(grade_detail$Stop_ID), ]
  
  grade_average <- grade_detail %>%
    mutate(Weighted = Grade * Total_Weekly_Avg_Pre_COVID_Ons_Offs) %>%
    select(Grade, Total_Weekly_Avg_Pre_COVID_Ons_Offs, Weighted) %>%
    summarize(Total_Ons_Offs = sum(Total_Weekly_Avg_Pre_COVID_Ons_Offs),
              Total_Weighted = sum(Weighted)) %>%
    mutate(Average = Total_Weighted / Total_Ons_Offs)
  
  grade_response_summary <- grades %>%
    select(Letter_Grade, Grade, Seating,
           Shelter, Trash_Can, Route_Number, Route_Map,
           Route_Schedule, Customer_Service, Wayfinding_Accessibility,
           Sidewalk, Main_Street_Crosswalk, No_Crosswalk, Boarding_Area,
           Traffic_Light, Curb_Cuts, Crossing_Audio, Crosswalk_Signals, Tactile_Guide) %>%
    mutate(No_Wayfinding = ifelse(str_count(paste(Route_Number,  Route_Schedule,  Route_Map, sep = ""), "Yes") == 0 & Customer_Service == "Yes", "Yes", "No"),
           Non_Accessible_Wayfinding = ifelse(str_count(paste(Route_Number, Route_Schedule, Route_Map, sep = ""), "Yes") >= 1 & Wayfinding_Accessibility == "No", "Yes", "No"),
           Accessible_Wayfinding = ifelse(str_count(paste(Route_Number, Route_Schedule, Route_Map, sep = ""), "Yes") >= 1 & Wayfinding_Accessibility == "Yes", "Yes", "No"),
           No_Crosswalk = ifelse(No_Crosswalk == "Yes", "Yes", "No"),
           Main_Street_Crosswalk_Only = ifelse(Main_Street_Crosswalk == "Yes" & 
                                                 str_count(paste(Traffic_Light, Curb_Cuts, Crosswalk_Signals, Crossing_Audio, Tactile_Guide, sep = ""), "Yes") == 0, "Yes", "No"),
           Main_Street_Crosswalk_Some_Crossing_Amenities = ifelse(Main_Street_Crosswalk == "Yes" &
                                                              str_count(paste(Traffic_Light, Curb_Cuts, Crosswalk_Signals, Crossing_Audio, Tactile_Guide, sep = ""), "Yes") > 0 &
                                                              str_count(paste(Traffic_Light, Curb_Cuts, Crosswalk_Signals, Crossing_Audio, Tactile_Guide, sep = ""), "Yes") <= 2, "Yes", "No"),
           Main_Street_Crosswalk_Most_Crossing_Amenities = ifelse(Main_Street_Crosswalk == "Yes" &
                                                              str_count(paste(Traffic_Light, Curb_Cuts, Crosswalk_Signals, Crossing_Audio, Tactile_Guide, sep = ""), "Yes") > 2 &
                                                               str_count(paste(Traffic_Light, Curb_Cuts, Crosswalk_Signals, Crossing_Audio, Tactile_Guide, sep = ""), "Yes") <= 5, "Yes", "No")) %>%
    group_by(Letter_Grade) %>% 
    summarize(Stop_Count = n(),
              Seating = sum(Seating != "No, there is no seating"),
              Shelter = sum(Shelter == "Yes"),
              Trash_Can = sum(Trash_Can == "Yes"),
              No_Wayfinding = sum(No_Wayfinding == "Yes"),
              Non_Accessible_Wayfinding = sum(Non_Accessible_Wayfinding == "Yes"),
              Accessible_Wayfinding = sum(Accessible_Wayfinding == "Yes"),
              Sidewalk = sum(Sidewalk != "No"),
              No_Crosswalk = sum(No_Crosswalk == "Yes"),
              Main_Street_Crosswalk_Only = sum(Main_Street_Crosswalk_Only == "Yes"),
              Main_Street_Crosswalk_Some_Crossing_Amenities = sum(Main_Street_Crosswalk_Some_Crossing_Amenities == "Yes"),
              Main_Street_Crosswalk_Most_Crossing_Amenities = sum(Main_Street_Crosswalk_Most_Crossing_Amenities == "Yes"),
              Boarding_Area = sum(Boarding_Area == "Concrete sidewalk")) %>%
      mutate(Percent_Of_Total_Stops = format(Stop_Count / sum(Stop_Count), digits = 2),
             Percent_Seating = format(Seating / Stop_Count, digits = 2),
             Percent_Shelter = format(Shelter / Stop_Count, digits = 2),
             Percent_Trash_Can = format(Trash_Can / Stop_Count, digits = 2),
             Percent_No_Wayfinding = format(No_Wayfinding / Stop_Count, digits = 2),
             Percent_Non_Accessible_Wayfinding = format(Non_Accessible_Wayfinding / Stop_Count, digits = 2),
             Percent_Accessible_Wayfinding = format(Accessible_Wayfinding / Stop_Count, digits = 2),
             Percent_Sidewalk = format(Sidewalk / Stop_Count, digits = 2),
             Percent_No_Crosswalk = format(No_Crosswalk / Stop_Count, digits = 2),
             Percent_Main_Street_Crosswalk_Only = format(Main_Street_Crosswalk_Only / Stop_Count, digits = 2),
             Percent_Main_Street_Crosswalk_Some_Amenities = format(Main_Street_Crosswalk_Some_Crossing_Amenities / Stop_Count, digits = 2),
             Percent_Main_Street_Crosswalk_Most_Amenities = format(Main_Street_Crosswalk_Most_Crossing_Amenities / Stop_Count, digits = 2),
             Percent_Boarding_Area = format(Boarding_Area / Stop_Count, digits = 2)) %>%
    select(Letter_Grade, Stop_Count, Percent_Seating, Percent_Shelter, Percent_Trash_Can,
           Percent_No_Wayfinding, Percent_Non_Accessible_Wayfinding,
           Percent_Accessible_Wayfinding, Percent_Sidewalk, Percent_No_Crosswalk,
           Percent_No_Crosswalk, Percent_Main_Street_Crosswalk_Only, Percent_Main_Street_Crosswalk_Some_Amenities,
           Percent_Main_Street_Crosswalk_Most_Amenities,  Percent_Boarding_Area)
    
  
  wb <- writeDataToExcel(list(grade_detail), wb, "Grading Detail")
  
  grade_summary <- summaryTableCreator(grades, "Letter_Grade", NA)
  grade_summary_by_county <- summaryTableCreator(grades, "Letter_Grade", "County")
  grade_summary_by_city <- summaryTableCreator(grades, "Letter_Grade", "City")
  
  df_list <- list(grade_average, grade_summary, grade_summary_by_county, grade_summary_by_city)
  wb <- writeDataToExcel(df_list, wb, "Grading Summary")
    
  grade_list <- grades$Grade
  hist(grade_list)
  
  ######################### ADD REPORT TABLES ##########################
  df_list <- list(total_surveys, goal_summary,
                  route_stops_surveyed, total_stops_essential,
                  seating, shelter, trash_can,
                  cleanliness, litter_with_trash_can, line_of_sight, route_number,
                  route_schedule, route_map, wayfinding_category, wayfinding_with_shelter,
                  wayfinding_accessibility, lighting, sidewalk,
                  obstacles, boarding_area, main_street_crosswalk,
                  cross_street_crosswalk, worn_faded, no_crosswalk,
                  crosswalk_category, traffic_light, curb_cuts, crosswalk_signals,
                  crossing_audio, tactile_guide, crosswalk_features, informal_pathways,
                  compete_for_seat, cross_midblock, catch_the_bus, dangerous_motorists,
                  first_visit, regular_user_none, observations, grade_average, grade_summary)
  
  report_tables <- createReportTables(df_list)
  
  wb <- writeDataToExcel(report_tables, wb, "Report Tables")
  
  ######################### EXPORT RESULTS #########################
  saveWorkbook(wb, here("data","tidy_data","Bus_Stop_Census_2020_Summary_Results.xlsx"), overwrite = TRUE)

  
  
  ######################### GENERATE PLOTS #########################
  ## Map of Completed Surveys
  surveyed_stops_coordinates <- route_stop_list %>% left_join(distinct_stop_list, by = c("stop_id"="Stop_ID"), keep = TRUE) %>%
    select(route_short_name, stop_id, Stop_ID_Join_Key) %>%
    inner_join(stops, by = c("stop_id" = "stop_id")) %>%
    select(stop_id, Stop_ID_Join_Key, stop_lat, stop_lon)
  
  map <- leaflet(data = surveyed_stops_coordinates) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% #HERE.normalDayGrey) %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(~stop_lon, ~stop_lat,
                     radius = 2,
                     color = ~ifelse(is.na(Stop_ID_Join_Key),"red","blue"),
                     stroke = FALSE, fillOpacity = 1) %>%
    addLegend("bottomright", colors = c("blue", "red"), labels = c("Surveyed", "Not Surveyed"),
              title = "Stop Surveyed Legend",
              opacity = 1
    )
  map
  
  
  seating_coord <- survey_df %>% distinct(Stop_ID, Stop_Lat, Stop_Lon, Seating, Total_Weekly_Avg_Pre_COVID_Ons_Offs) %>%
    mutate(Seating_Combined = ifelse(Seating == "No, there is no seating", "No, there is no seating", "Yes, there is seating"),
           Total_Weekly_Avg_Pre_COVID_Ons_Offs = round(Total_Weekly_Avg_Pre_COVID_Ons_Offs, digits = 0)) %>%
    select(Stop_ID, Seating_Combined, Stop_Lat, Stop_Lon, Total_Weekly_Avg_Pre_COVID_Ons_Offs) %>%
    dplyr::filter(Seating_Combined == "No, there is no seating",
                  Total_Weekly_Avg_Pre_COVID_Ons_Offs > 200) %>%
    
    
    mutate(Ons_Offs = case_when(
      Total_Weekly_Avg_Pre_COVID_Ons_Offs >= 0 & Total_Weekly_Avg_Pre_COVID_Ons_Offs <= 100 ~ "0-100",
      Total_Weekly_Avg_Pre_COVID_Ons_Offs > 100 & Total_Weekly_Avg_Pre_COVID_Ons_Offs <= 200 ~ "101-200",
      Total_Weekly_Avg_Pre_COVID_Ons_Offs > 200 & Total_Weekly_Avg_Pre_COVID_Ons_Offs <= 300 ~ "201-300",
      Total_Weekly_Avg_Pre_COVID_Ons_Offs > 300 & Total_Weekly_Avg_Pre_COVID_Ons_Offs <= 400 ~ "301-400",
      Total_Weekly_Avg_Pre_COVID_Ons_Offs > 400 & Total_Weekly_Avg_Pre_COVID_Ons_Offs <= 500 ~ "401-500",
      Total_Weekly_Avg_Pre_COVID_Ons_Offs > 500 & Total_Weekly_Avg_Pre_COVID_Ons_Offs <= 600 ~ "501-600",
      Total_Weekly_Avg_Pre_COVID_Ons_Offs > 600 ~ ">600",
      TRUE ~ ""))
  
  # Create a continuous palette function
  binpal <- colorQuantile("Reds", seating_coord$Total_Weekly_Avg_Pre_COVID_Ons_Offs, n = 7)
  
  seating_map <- leaflet(data = seating_coord) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% #HERE.normalDayGrey) %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(~Stop_Lon, ~Stop_Lat,
                     radius = 4,
                     color = ~binpal(Total_Weekly_Avg_Pre_COVID_Ons_Offs),
                     stroke = FALSE, fillOpacity = 1) %>%
    addLegend("bottomright", colors = c("green", "red"), labels = c("Yes, there is seating", "No, there is no seating"),
              title = "Does the stop have a bench or other seating?",
              opacity = 1
    )
  seating_map 
  
  
  
  shelter_coord <- survey_df %>% distinct(Stop_ID, Stop_Lat, Stop_Lon, Shelter, Total_Weekly_Avg_Pre_COVID_Ons_Offs) %>%
    mutate(Total_Weekly_Avg_Pre_COVID_Ons_Offs = round(Total_Weekly_Avg_Pre_COVID_Ons_Offs, digits = 0)) %>%
    select(Stop_ID, Shelter, Stop_Lat, Stop_Lon, Total_Weekly_Avg_Pre_COVID_Ons_Offs) %>%
    dplyr::filter(Shelter == "No",
                  Total_Weekly_Avg_Pre_COVID_Ons_Offs > 200)
  
  # Create a continuous palette function
  binpal <- colorQuantile("Reds", shelter_coord$Total_Weekly_Avg_Pre_COVID_Ons_Offs, n = 7)
  
  shelter_map <- leaflet(data = shelter_coord) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% #HERE.normalDayGrey) %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(~Stop_Lon, ~Stop_Lat,
                     radius = 4,
                     color = ~binpal(Total_Weekly_Avg_Pre_COVID_Ons_Offs),
                     stroke = FALSE, fillOpacity = 1) %>%
    addLegend("bottomright", colors = c("green", "red"), labels = c("Yes, there is seating", "No, there is no seating"),
              title = "Does the stop have a bench or other seating?",
              opacity = 1
    )
  shelter_map 
  
  sidewalk_coord <- survey_df %>% distinct(Stop_ID, Sidewalk, Stop_Lat, Stop_Lon, Total_Weekly_Avg_Pre_COVID_Ons_Offs) %>%
    mutate(Total_Weekly_Avg_Pre_COVID_Ons_Offs = round(Total_Weekly_Avg_Pre_COVID_Ons_Offs, digits = 0)) %>%
    select(Stop_ID, Sidewalk, Stop_Lat, Stop_Lon, Total_Weekly_Avg_Pre_COVID_Ons_Offs) %>%
    dplyr::filter(Sidewalk == "No",
                  Total_Weekly_Avg_Pre_COVID_Ons_Offs > 200)
  
  #Create a continuous palette function
  binpal <- colorQuantile("Reds", sidewalk_coord$Total_Weekly_Avg_Pre_COVID_Ons_Offs, n = 4)
  
  sidewalk_map <- leaflet(data = sidewalk_coord) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% #HERE.normalDayGrey) %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(~Stop_Lon, ~Stop_Lat,
                     radius = 4,
                     color = ~binpal(sidewalk_coord$Total_Weekly_Avg_Pre_COVID_Ons_Offs),
                     stroke = FALSE, fillOpacity = 1) %>%
    addLegend("bottomright", colors = c("green", "yellow", "red"), labels = c("Yes, in both directions", "Yes, in only one direction", "No"),
              title = "Is there a paved sidewalk to the boarding area of the bus?",
              opacity = 1
    )
  sidewalk_map
  
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
  
  # Main Street Crosswalk Map
  main_crosswalk_coord <- survey_df %>% distinct(Stop_ID, Main_Street_Crosswalk, Stop_Lat, Stop_Lon, Total_Weekly_Avg_Pre_COVID_Ons_Offs) %>%
    mutate(Total_Weekly_Avg_Pre_COVID_Ons_Offs = round(Total_Weekly_Avg_Pre_COVID_Ons_Offs, digits = 0)) %>%
    select(Stop_ID, Main_Street_Crosswalk, Stop_Lat, Stop_Lon, Total_Weekly_Avg_Pre_COVID_Ons_Offs) %>%
    dplyr::filter(Main_Street_Crosswalk == "No",
                  Total_Weekly_Avg_Pre_COVID_Ons_Offs > 200)
  
  #Create a continuous palette function
  binpal <- colorQuantile("Reds", main_crosswalk_coord$Total_Weekly_Avg_Pre_COVID_Ons_Offs, n = 4)
  
  crosswalk_map <- leaflet(data = main_crosswalk_coord) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% #HERE.normalDayGrey) %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(~Stop_Lon, ~Stop_Lat,
                     radius = 4,
                     color = ~binpal(main_crosswalk_coord$Total_Weekly_Avg_Pre_COVID_Ons_Offs),
                     stroke = FALSE, fillOpacity = 1) %>%
    addLegend("bottomright", colors = c("green", "yellow", "red"), labels = c("Yes, in both directions", "Yes, in only one direction", "No"),
              title = "Is there a paved sidewalk to the boarding area of the bus?",
              opacity = 1
    )
  crosswalk_map
  
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
  
  grade_coord <- grades %>% distinct(Stop_ID, Letter_Grade, Stop_Lat, Stop_Lon, Total_Weekly_Avg_Pre_COVID_Ons_Offs) %>%
    mutate(Total_Weekly_Avg_Pre_COVID_Ons_Offs = round(Total_Weekly_Avg_Pre_COVID_Ons_Offs, digits = 0)) %>%
    select(Stop_ID, Letter_Grade, Stop_Lat, Stop_Lon, Total_Weekly_Avg_Pre_COVID_Ons_Offs) %>%
    dplyr::filter(Letter_Grade == "D",
                  Total_Weekly_Avg_Pre_COVID_Ons_Offs > 200)
  
  #Create a continuous palette function
  binpal <- colorQuantile("Reds", grade_coord$Total_Weekly_Avg_Pre_COVID_Ons_Offs, n = 4)
  
  grade_map <- leaflet(data = grade_coord) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% #HERE.normalDayGrey) %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(~Stop_Lon, ~Stop_Lat,
                     radius = 4,
                     color = ~binpal(Total_Weekly_Avg_Pre_COVID_Ons_Offs),
                     stroke = FALSE, fillOpacity = 1) %>%
    addLegend("bottomright", colors = c("green", "yellow", "red"), labels = c("Yes, in both directions", "Yes, in only one direction", "No"),
              title = "Is there a paved sidewalk to the boarding area of the bus?",
              opacity = 1
    )
  grade_map
               