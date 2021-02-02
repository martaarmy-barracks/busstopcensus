
  correctRecords <- function(survey_df, corrections_df, source_sheet) {
    
    corrections_subset <- subset(corrections_df, Correction_Source_Sheet == source_sheet)
    print("subsetted")
    for (row in 1:nrow(corrections_subset)) {
     if (corrections_subset[row, "Change"] == "Replace") {
       print("replace")
       record_id <- corrections_subset[row, "Record_ID"]
       record_id <- record_id[[1]]
       print(record_id)
       field <- corrections_subset[row, "Field"]
       field <- field[[1]]
       print(field)
       value <- corrections_subset[row, "Value"]
       print(value)
       print("variable assigned")
       survey_df[which(survey_df$Record_ID == record_id), field] <- value[[1]]
       print("value assigned")
     } else {
       print("delete")
       record_id <- corrections_subset[row, "Record_ID"]
       record_id <- record_id[[1]]
       print(record_id)
       survey_df <- subset(survey_df, Record_ID != record_id)
       print("record removed")
     }
    }
    
    return(survey_df)
  }

  corrections_df[corrections_df$Record_ID == 42, "Stop_ID"] <- "212715"
  # test_survey_df <- data.frame( x = c(1,2,3,4,5),
  #                               y = c("a", "b", "c", "d", "e"),
  #                               z = c("f", "g", "h", "i", "j"))
  # 
  # corrections_test_df <- data.frame(record = c(1,3,4),
  #                                   field = c("y", "","z"),
  #                                   action = c("replace", "delete", "replace"),
  #                                   value = c("s","","@"),
  #                                   source = c("id", "id", "missing"))
  # 
  # correctRecords(test_survey_df, corrections_test_df, "missing")
  
  importGTFS <- function(gtfs_date) {
    stops <- fread(here("data", "raw_data", "gtfs", gtfs_date, "stops.txt"), # set relative path to data source
                            stringsAsFactors = FALSE,
                            colClasses = c("character","character","character","double","double"))
    
    routes <- fread(here("data","raw_data", "gtfs", gtfs_date, "routes.txt"), 
                             stringsAsFactors = FALSE, 
                             colClasses = c(route_id = "character",
                                            route_short_name = "character",
                                            route_long_name = "character",
                                            route_type = "integer"),
                             select = c("route_id","route_short_name","route_long_name","route_type"))
    
    stop_times <- fread(here("data","raw_data", "gtfs", gtfs_date, "stop_times.txt"),
                                 stringsAsFactors = FALSE, 
                                 colClasses = c("character","character","character","character","integer"),
                                 select = c("trip_id","arrival_time","stop_id","stop_sequence"))
    
    shapes <- fread(here("data","raw_data", "gtfs", gtfs_date, "shapes.txt"), 
                             stringsAsFactors = FALSE, 
                             colClasses = c("character","double","double","integer"))
    
    trips <- fread(here("data","raw_data", "gtfs", gtfs_date, "trips.txt"),
                            stringsAsFactors = FALSE,
                            colClasses = c("character","integer","character",
                                           "character","integer","character",
                                           "character"),
                            drop = c("block_id"))
    
    calendar <- fread(here("data","raw_data", "gtfs", gtfs_date, "calendar.txt"), 
                               stringsAsFactors = FALSE, 
                               colClasses = c("integer","character","character",
                                              "character","character","character",
                                              "character","character","character",
                                              "character"),
                               select = c("service_id"))
    
    
    ## CLEAN GTFS DATA --------------------------------------------------------------------------------------------------
    # Convert service_id to day of the week text
    calendar <- calendar %>% mutate(.,service_type = 
                                                        with(.,case_when((service_id == 3) ~ "Saturday",
                                                                         (service_id == 4) ~ "Sunday",
                                                                         (service_id == 5) ~ "Weekday")))
    # Convert route_type to bus or rail
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
      dplyr::filter(route_type_desc == "Bus")
    
    stop_data <- inner_join(stop_times,stops,by="stop_id") %>%
      select(trip_id,stop_id,stop_sequence,stop_name,arrival_time)
    
    return(list(stops, routes, stop_times, shapes, trips, calendar, route_data, stop_data))
  }