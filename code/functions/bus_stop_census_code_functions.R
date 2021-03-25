  # This script contains functions used in the Bus Stop Census Code

  # The correctRecords functions applies corrections specified in the Bus Stop Census Corrections Log, located here,
  # https://docs.google.com/spreadsheets/d/1n8RErNhOXrNptVyul5rTuE-ZTdSzkljB0cCAV8hBJkw/edit?usp=sharing, to the raw data.
  correctRecords <- function(survey_df, corrections_df, source_sheet) {
    corrections_subset <- subset(corrections_df, Correction_Source_Sheet == source_sheet) # Subset corrections data based on the sheet the corrections originated from
    print("subsetted") # Print command showing the corrections data has been subsetted
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
    
    # Convert stop hours that are into the next day to regular clock hours (e.g. 25:12:05 becomes 01:12:05)
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
    
    
    ## CREATE SUMMARY TABLES WITH RIDERSHIP DATA ------------------------------------------------------------------------
    summaryTableCreator <- function(df, field, geo) {
      require(dplyr)
      if (is.na(geo)) {
        summary_df <- df %>% 
          distinct(Stop_ID, !! rlang::sym(field), Total_Weekly_Avg_Pre_COVID_Boardings, Total_Weekly_Avg_Pre_COVID_Alightings,
                   Total_Weekly_Avg_Pre_COVID_Ons_Offs, Total_Weekly_Avg_Post_COVID_Boardings, Total_Weekly_Avg_Post_COVID_Alightings,
                   Total_Weekly_Avg_Post_COVID_Ons_Offs) %>%
          group_by_(field) %>%
          summarize(Count = n(),
                    Cum_Weekly_Avg_Pre_COVID_Boardings = sum(Total_Weekly_Avg_Pre_COVID_Boardings, na.rm = TRUE),
                    Cum_Weekly_Avg_Pre_COVID_Alightings = sum(Total_Weekly_Avg_Pre_COVID_Alightings, na.rm = TRUE),
                    Cum_Weekly_Avg_Pre_COVID_Ons_Offs = sum(Total_Weekly_Avg_Pre_COVID_Ons_Offs, na.rm = TRUE),
                    Cum_Weekly_Avg_Post_COVID_Boardings = sum(Total_Weekly_Avg_Post_COVID_Boardings, na.rm = TRUE),
                    Cum_Weekly_Avg_Post_COVID_Alightings = sum(Total_Weekly_Avg_Post_COVID_Alightings, na.rm = TRUE),
                    Cum_Weekly_Avg_Post_COVID_Ons_Offs = sum(Total_Weekly_Avg_Post_COVID_Ons_Offs, na.rm = TRUE)) %>%
          mutate(Percent_Of_Total_Stops = format(Count / sum(Count), digits = 2),
                 Percent_Cum_Weekly_Avg_Pre_COVID_Boardings = format(Cum_Weekly_Avg_Pre_COVID_Boardings / sum(Cum_Weekly_Avg_Pre_COVID_Boardings), digits = 2),
                 Percent_Cum_Weekly_Avg_Pre_COVID_Alightings = format(Cum_Weekly_Avg_Pre_COVID_Alightings / sum(Cum_Weekly_Avg_Pre_COVID_Alightings), digits = 2),
                 Percent_Cum_Weekly_Avg_Pre_COVID_Ons_Offs = format(Cum_Weekly_Avg_Pre_COVID_Ons_Offs / sum(Cum_Weekly_Avg_Pre_COVID_Ons_Offs), digits = 2),
                 Percent_Cum_Weekly_Avg_Post_COVID_Boardings = format(Cum_Weekly_Avg_Post_COVID_Boardings / sum(Cum_Weekly_Avg_Post_COVID_Boardings), digits = 2),
                 Percent_Cum_Weekly_Avg_Post_COVID_Alightings = format(Cum_Weekly_Avg_Post_COVID_Alightings / sum(Cum_Weekly_Avg_Post_COVID_Alightings), digits = 2),
                 Percent_Cum_Weekly_Avg_Post_COVID_Ons_Offs = format(Cum_Weekly_Avg_Post_COVID_Ons_Offs / sum(Cum_Weekly_Avg_Post_COVID_Ons_Offs), digits = 2))
      } else {
        summary_df <- df %>% 
          distinct(Stop_ID, !! rlang::sym(geo), !! rlang::sym(field), Total_Weekly_Avg_Pre_COVID_Boardings, Total_Weekly_Avg_Pre_COVID_Alightings,
                   Total_Weekly_Avg_Pre_COVID_Ons_Offs, Total_Weekly_Avg_Post_COVID_Boardings, Total_Weekly_Avg_Post_COVID_Alightings,
                   Total_Weekly_Avg_Post_COVID_Ons_Offs) %>%
          group_by_(geo, field) %>%
          summarize(Count = n(),
                    Cum_Weekly_Avg_Pre_COVID_Boardings = sum(Total_Weekly_Avg_Pre_COVID_Boardings, na.rm = TRUE),
                    Cum_Weekly_Avg_Pre_COVID_Alightings = sum(Total_Weekly_Avg_Pre_COVID_Alightings, na.rm = TRUE),
                    Cum_Weekly_Avg_Pre_COVID_Ons_Offs = sum(Total_Weekly_Avg_Pre_COVID_Ons_Offs, na.rm = TRUE),
                    Cum_Weekly_Avg_Post_COVID_Boardings = sum(Total_Weekly_Avg_Post_COVID_Boardings, na.rm = TRUE),
                    Cum_Weekly_Avg_Post_COVID_Alightings = sum(Total_Weekly_Avg_Post_COVID_Alightings, na.rm = TRUE),
                    Cum_Weekly_Avg_Post_COVID_Ons_Offs = sum(Total_Weekly_Avg_Post_COVID_Ons_Offs, na.rm = TRUE)) %>%
          mutate(Percent_Of_Total_Stops = format(Count / sum(Count), digits = 2),
                 Percent_Cum_Weekly_Avg_Pre_COVID_Boardings = format(Cum_Weekly_Avg_Pre_COVID_Boardings / sum(Cum_Weekly_Avg_Pre_COVID_Boardings), digits = 2),
                 Percent_Cum_Weekly_Avg_Pre_COVID_Alightings = format(Cum_Weekly_Avg_Pre_COVID_Alightings / sum(Cum_Weekly_Avg_Pre_COVID_Alightings), digits = 2),
                 Percent_Cum_Weekly_Avg_Pre_COVID_Ons_Offs = format(Cum_Weekly_Avg_Pre_COVID_Ons_Offs / sum(Cum_Weekly_Avg_Pre_COVID_Ons_Offs), digits = 2),
                 Percent_Cum_Weekly_Avg_Post_COVID_Boardings = format(Cum_Weekly_Avg_Post_COVID_Boardings / sum(Cum_Weekly_Avg_Post_COVID_Boardings), digits = 2),
                 Percent_Cum_Weekly_Avg_Post_COVID_Alightings = format(Cum_Weekly_Avg_Post_COVID_Alightings / sum(Cum_Weekly_Avg_Post_COVID_Alightings), digits = 2),
                 Percent_Cum_Weekly_Avg_Post_COVID_Ons_Offs = format(Cum_Weekly_Avg_Post_COVID_Ons_Offs / sum(Cum_Weekly_Avg_Post_COVID_Ons_Offs), digits = 2)) %>%
          arrange(!! rlang::sym(geo))
      }
      
      return(summary_df) 
    }
  }