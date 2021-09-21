
  ## LOAD LIBRARIES -------------------------------------------------------------------------------------------------
  pkgs <- c("chron", "data.table", "dplyr", "geosphere", "ggplot2", # package names
            "googlesheets4", "here", "jsonlite", "leaflet", "maps",
            "openxlsx", "revgeo", "stringdist", "stringr", "tidyr")

  
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
  # The Bus Stop Census data collection began in Feb 4th, 2020 and ended on Dec 31st, 2020. At the start of the data collection,
  # the GTFS from Dec 6th, 2019 was used to provide the list of stops that were available to be surveyed. The service changes due
  # to COVID-19 resulted in major changes to the GTFS. For example, many of the routes that were on the 2019-12-06 GTFS version 
  # were removed in the versions released in March, April, and May as bus routes were suspended. As of 2020-12-04, nearly one year
  # after the release of the starting GTFS version, the GTFS only contained data for 53 of the 110 routes that were covered by
  # the survey at its launch in February. Throughout the course of the data collection, MARTA Army collected information for both
  # bus stops that had service and bus stops where the service was suspended. On the busstopcensus.com online survey page, these
  # bus stops were identified by color and shape, along with text notifying the surveyor that the bus stop had no routes currently
  # serving the stop.
  #
  # In order to provide accurate information for all bus stops surveyed during the data collection period, we are comparing the
  # GTFS data used at the start of data collection to the GTFS data that was in effect at the end of data collection. These
  # versions were the 2019-12-06 and 2020-12-04 versions respectively. Where the bus stop information has changed from the older
  # version to the newer version, the survey records will reflect the most recent information from the 2020-12-04 version.
  # Bus stops that appear on the 2020-12-04 version of the GTFS but not in the 2019-12-06 version have been incorporated into
  # this analysis. The final result is a comprehensive list of bus stops that are 1) Appeared in 2019-12-06 GTFS data but are 
  # no longer served by a MARTA bus route due to service suspension 2) Appeared in the 2019-12-06 GTFS data and continued to
  # have service during the pandemic or regained it since route suspensions occurred or 3) Did not appear on the 2019-12-06 GTFS
  # data set but was added at some point between that version and the 2020-12-04 version. 
  # For each stop in this list, the stop names and coordinates have been updated to be the latest version, whether that is from
  # the 2019-12-06 version or the 2020-12-04 version. This list is represented by the final_stop_list dataframe and is used
  # to normalize the Main_Street and the Nearest_Landmark fields in the survey records.
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
  
  rm(gtfs_dec_2019_df_list)
  rm(route_data_2019)
  
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
  

  rm(gtfs_dec_2020_df_list)
  rm(route_data_2020)
  
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
           Routes_Final   = ifelse(is.na(Routes.x), Routes.y, Routes.x),
           Direction_Final = ifelse(is.na(Direction.x), Direction.y, Direction.x)) %>%
    select(stop_id, Stop_Name, Stop_Lat, Stop_Lon, Routes_Final, Direction_Final) %>%
    separate(Stop_Name, c("Main_Street_or_Station", "Cross_Street"), sep = "@") %>%
    mutate(Main_Street_or_Station = trimws(Main_Street_or_Station),
           Cross_Street = trimws(Cross_Street))
