
  ## IMPORT LIBRARIES -------------------------------------------------------------------------------------------------
  library(jsonlite)
  library(tidyr)
  library(dplyr)
  library(data.table)
  library(chron)
  library(ggplot2)
  library(leaflet)
  library(here)
  
  ## ASSIGN METADATA --------------------------------------------------------------------------------------------------
  peak_am <- c(6,7,8)
  peak_pm <- c(15,16,17,18)
  hour_order <- c(4,5,6,7,8,9,10,
                  11,12,13,14,15,
                  16,17,18,19,20,
                  21,22,23,0,1,2)
  
  
  ## IMPORT GTFS DATA -------------------------------------------------------------------------------------------------
  stops <- fread(here("data", "raw_data", "gtfs","stops.txt"),stringsAsFactors = FALSE,
                 colClasses = c("character","character","character","double","double"))
  routes <- fread(here("data","raw_data", "gtfs","routes.txt"), stringsAsFactors = FALSE, 
                  colClasses = c("character","numeric","character","character",
                                 "integer","character","character","character"),
                  select = c("route_id","route_short_name","route_long_name","route_type"))
  stop_times <- fread(here("data","raw_data", "gtfs","stop_times.txt"),stringsAsFactors = FALSE, 
                      colClasses = c("character","character","character","character","integer"),
                      select = c("trip_id","arrival_time","stop_id","stop_sequence"))
  shapes <- fread(here("data","raw_data", "gtfs","shapes.txt"), stringsAsFactors = FALSE, 
                  colClasses = c("character","double","double","integer"))
  trips <- fread(here("data","raw_data", "gtfs","trips.txt"),stringsAsFactors = FALSE,
                 colClasses = c("character","integer","character",
                                "character","integer","character",
                                "character"),
                 drop = c("block_id"))
  calendar <- fread(here("data","raw_data", "gtfs","calendar.txt"), stringsAsFactors = FALSE, 
                    colClasses = c("integer","character","character",
                                   "character","character","character",
                                   "character","character","character",
                                   "character"),
                    select = c("service_id"))
  
  
  ## CLEAN GTFS DATA --------------------------------------------------------------------------------------------------
  # Add Service Descriptors
  calendar <- calendar %>% mutate(.,service_type = 
                                    with(.,case_when((service_id == 3) ~ "Saturday",
                                                     (service_id == 4) ~ "Sunday",
                                                     (service_id == 5) ~ "Weekday")))
  routes <- routes %>% mutate(.,route_type_desc = 
                                with(.,case_when((route_type == 3) ~ "Bus",
                                                 (route_type == 1) ~ "Rail")))
  
  # Format Stop Times To be 24 hour
  stop_times$arrival_time_adj <- ifelse(substr(stop_times$arrival_time,1,2) == "24",
                                        sub("24","00",stop_times$arrival),
                                        ifelse(substr(stop_times$arrival_time,1,2) == "25",
                                               sub("25","01",stop_times$arrival_time),
                                               ifelse(substr(stop_times$arrival_time,1,2) == "26",
                                                      sub("26","02",stop_times$arrival_time), stop_times$arrival_time)))
  
  ## JOIN GTFS DATA ---------------------------------------------------------------------------------------------------
  route_data <- inner_join(stop_times,trips,by="trip_id") %>%
    inner_join(.,stops,by="stop_id") %>%
    inner_join(.,routes,by="route_id") %>%
    inner_join(.,calendar,by="service_id") %>%
    filter(route_type_desc == "Bus")
  
  stop_data <- inner_join(stop_times,stops,by="stop_id") %>%
    select(trip_id,stop_id,stop_sequence,stop_name,arrival_time)
  
