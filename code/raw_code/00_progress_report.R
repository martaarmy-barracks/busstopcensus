  
  ## IMPORT DATA ######################################################################################################
  survey_df <- read.csv(here("data", "tidy_data", "pre_processed_survey_df.csv"))
  
  distinct_stop_list <- survey_df %>% distinct(Stop_ID) %>%
    mutate(Stop_ID_Join_Key = Stop_ID)
  
  goal_summary <- survey_df %>% distinct(Stop_ID) %>%
    mutate(Stop_ID_Join_Key = Stop_ID) %>%
    summarize(Total_Unique_Stops_surveyed = n(),
              Percent_of_Goal = n()/ 2500)
  
  ## Top Overall and Weekly Surveyor Results
  all_surveyors <- survey_df %>% distinct(Email, Stop_ID) %>%
    group_by(Email) %>%
    summarize(Total_Surveyed = n()) %>%
    dplyr::filter(!is.na(Email), grepl('@', Email))
  
  top_weekly_surveyors <- survey_df %>% select(Email, Timestamp_Sub, Stop_ID) %>%
    dplyr::filter(Timestamp_Sub >= "2020-11-15 00:00:00", # Change dates to match week of interest
                  Timestamp_Sub <= "2020-11-21 23:59:59") %>%
    distinct(Email, Stop_ID) %>%
    group_by(Email) %>%
    summarize(Count = n())
  prize_surveyors <- all_surveyors
  write.csv(prize_surveyors, here("products","Prize Surveyors.csv"))
  
  ## Routes Survey Completion
  essential_service <- c("2","4", "5", "6", "12", "15", "19", "21", "26", "39", "40",
                         "42", "49", "50", "51", "55", "60","66", "71", "73","74","78","81",
                         "82", "83", "84", "86","87", "89", "95", "102", "107", "110",
                         "111","115", "116", "117", "120", "121", "124", "125", "172", "178","180",
                         "181","185", "186", "191", "192", "193", "196", "188", "816")
  
  route_stop_list <- route_data %>% distinct(route_short_name, route_long_name, stop_id) %>%
    dplyr::filter(route_short_name %in% essential_service)
  
  route_stops_surveyed <- route_stop_list %>% left_join(distinct_stop_list, by = c("stop_id"="Stop_ID"), keep = TRUE) %>%
    select(route_short_name,route_long_name, stop_id, Stop_ID_Join_Key) %>%
    mutate(Stop_Survey_Count = ifelse(is.na(Stop_ID_Join_Key), 0, 1)) %>%
    group_by(route_short_name, route_long_name) %>%
    summarize(Total_Route_Stops = n(),
              Total_Route_Stops_Surveyed = sum(Stop_Survey_Count),
              Percent_Surveyed = Total_Route_Stops_Surveyed/ Total_Route_Stops)
  
  total_stops_essential <- route_stop_list %>% left_join(distinct_stop_list, by = c("stop_id"="Stop_ID"), keep = TRUE) %>%
    distinct(stop_id) %>%
    summarize(Total = n())
  
  
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
  
  ## This function that identifies the county the bus stop is located in was taken from this Stack Overflow thread:
  ##https://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county
  ## County Survey Results
  latlong2county <- function(pointsDF) {
    # Prepare SpatialPolygons object with one SpatialPolygon
    # per county
    counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
    IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
    counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                       proj4string=CRS("+proj=longlat +datum=WGS84"))
    
    # Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(pointsDF, 
                              proj4string=CRS("+proj=longlat +datum=WGS84"))
    
    # Use 'over' to get _indices_ of the Polygons object containing each point 
    indices <- over(pointsSP, counties_sp)
    
    # Return the county names of the Polygons object containing each point
    countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
    return(countyNames[indices])
  }
  stops_to_county_df <- surveyed_stops_coordinates %>% select(Stop_ID_Join_Key, stop_lat, stop_lon)
  
  just_coordinates <- stops_to_county_df %>% select(stop_lon, stop_lat)
  
  county_coord_results <- data.frame(County = latlong2county(just_coordinates))  
  
  county_results_bind <- stops_to_county_df %>% cbind(county_coord_results) %>%
                         mutate(Stop_Surveyed = ifelse(is.na(Stop_ID_Join_Key), 0, 1)) %>%
                         group_by(County) %>%
                         summarize(Total_Stops = n(),
                                   Total_Surveyed = sum(Stop_Surveyed),
                                   Percent_Surveyed = sum(Stop_Surveyed) / n())
  