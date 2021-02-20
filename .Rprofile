
  ## LOAD LIBRARIES -------------------------------------------------------------------------------------------------
  pkgs <- c("chron", "data.table", "dplyr", "geosphere", "ggplot2", # package names
            "googlesheets4", "here", "jsonlite", "leaflet", "stringdist", "stringr", "tidyr")
  
  inst = lapply(pkgs, library, character.only = TRUE) # load packages
  
  ## SOURCE FUNCTIONS -------------------------------------------------------------------------------------------------
  source(here("code","functions","bus_stop_census_code_functions.R"))
  
  ## ASSIGN METADATA --------------------------------------------------------------------------------------------------
  peak_am <- c(6,7,8) # denote peak am hours
  peak_pm <- c(15,16,17,18) # denote peak pm hours
  hour_order <- c(4,5,6,7,8,9,10,
                  11,12,13,14,15,
                  16,17,18,19,20,
                  21,22,23,0,1,2)
  
  ## IMPORT GTFS DATA -------------------------------------------------------------------------------------------------
  # List order -> c(stops,  routes, stop_times, shapes, trips, calendar, route_data, stop_data)
  gtfs_dec_2019_df_list <- importGTFS("2019-12-06")
  gtfs_dec_2020_df_list <- importGTFS("2020-12-04")
  
  
  
  
  ## SUMMARIZE STOP ROUTE DATA ----------------------------------------------------------------------------------------
  route_data_2019 <- gtfs_dec_2019_df_list[[7]]
  stop_route_summary_2019 <- route_data_2019 %>% distinct(stop_id,
                                                     stop_name,
                                                     stop_lon,
                                                     stop_lat,
                                                     route_short_name,
                                                     direction_id) %>%
                                             mutate(Direction = ifelse(direction_id == 1, "SB/WB", "NB/EB")) %>%
                                             arrange(stop_id, as.integer(route_short_name)) %>%
                                             select(-direction_id) %>%
                                             dplyr::group_by(stop_id, stop_name, stop_lon, stop_lat) %>%
                                             dplyr::summarise(Routes = paste(route_short_name, collapse = ", "),
                                                             Direction = paste(Direction, collapse = ", "))
  
  route_data_2020 <- gtfs_dec_2020_df_list[[7]]
  stop_route_summary_2020 <- route_data_2020 %>% distinct(stop_id,
                                                          stop_name,
                                                          stop_lon,
                                                          stop_lat,
                                                          route_short_name,
                                                          direction_id) %>%
                                                  mutate(Direction = ifelse(direction_id == 1, "SB/WB", "NB/EB")) %>%
                                                  arrange(stop_id, as.integer(route_short_name)) %>%
                                                  select(-direction_id) %>%
                                                  dplyr::group_by(stop_id, stop_name, stop_lon, stop_lat) %>%
                                                  dplyr::summarise(Routes = paste(route_short_name, collapse = ", "),
                                                                   Direction = paste(Direction, collapse = ", "))
  
  
  # Collect GTFS Stop Data to Compare to Survey Responses
  stop_comparison_data <- stop_route_summary_2019 %>% full_join(stop_route_summary_2020, by = c("stop_id")) %>%       # Join pre-pandemic stop data with end of 2020 stop data
    rowwise() %>%
    mutate(name_diff = ifelse(stop_name.x != stop_name.y, "FLAG", ""),
           name_dist = stringdist(stop_name.x, stop_name.y),
           lat_diff = ifelse(stop_lat.x != stop_lat.y, "FLAG", ""),
           lon_diff = ifelse(stop_lon.x != stop_lon.y, "FLAG", ""),
           coord_dist = distm(c(stop_lon.x,stop_lat.x), c(stop_lon.y, stop_lat.y), fun = distHaversine)) 
  
  final_stop_list <- stop_comparison_data %>%
    mutate(Stop_Name = ifelse(is.na(stop_name.x), stop_name.y, stop_name.x),
           Stop_Lat = ifelse(is.na(stop_lat.x), stop_lat.y, stop_lat.x),
           Stop_Lon = ifelse(is.na(stop_lon.x), stop_lon.y, stop_lon.x),
           Routes   = ifelse(is.na(Routes.x), Routes.y, Routes.x),
           Direction = ifelse(is.na(Direction.x), Direction.y, Direction.x)) %>%
    select(stop_id, Stop_Name, Stop_Lat, Stop_Lon, Routes, Direction) %>%
    separate(Stop_Name, c("Main_Street_or_Station", "Cross_Street"), sep = "@") %>%
    mutate(Main_Street_or_Station = trimws(Main_Street_or_Station),
           Cross_Street = trimws(Cross_Street))
