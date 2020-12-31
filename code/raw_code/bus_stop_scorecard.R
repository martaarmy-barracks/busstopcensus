

  ## IMPORT LIBRARIES
  library(jsonlite)
  library(dplyr)
  library(data.table)
  library(chron)
  library(ggplot2)
  theme_set(theme_classic())
  
  start_time = Sys.time()
  peak_am <- c(6,7,8)
  peak_pm <- c(15,16,17,18)
  hour_order <- c(4,5,6,7,8,9,10,
                  11,12,13,14,15,
                  16,17,18,19,20,
                  21,22,23,0,1,2)
  
  ## IMPORT DATA
  #Import GTFS Data Sets
  stops <- fread("stops.txt",stringsAsFactors = FALSE,
                 colClasses = c("character","character","character","double","double"))
  routes <- fread("routes.txt", stringsAsFactors = FALSE, 
                  colClasses = c("character","numeric","character","character",
                                 "integer","character","character","character"),
                  select = c("route_id","route_short_name","route_long_name","route_type"))
  stop_times <- fread("stop_times.txt",stringsAsFactors = FALSE, 
                      colClasses = c("character","character","character","character","integer"),
                      select = c("trip_id","arrival_time","stop_id","stop_sequence"))
  shapes <- fread("shapes.txt", stringsAsFactors = FALSE, 
                  colClasses = c("character","double","double","integer"))
  trips <- fread("trips.txt",stringsAsFactors = FALSE,
                 colClasses = c("character","integer","character",
                                "character","integer","character",
                                "character"),
                 drop = c("block_id"))
  calendar <- fread("calendar.txt", stringsAsFactors = FALSE, 
                    colClasses = c("integer","character","character",
                                   "character","character","character",
                                   "character","character","character",
                                   "character"),
                    select = c("service_id"))

  ## CLEAN DATA
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
    
  
  ## JOIN DATA
  
  route_data <- inner_join(stop_times,trips,by="trip_id") %>%
    inner_join(.,stops,by="stop_id") %>%
    inner_join(.,routes,by="route_id") %>%
    inner_join(.,calendar,by="service_id") %>%
    filter(route_type_desc == "Bus")
  
  stop_data <- inner_join(stop_times,stops,by="stop_id") %>%
    select(trip_id,stop_id,stop_sequence,stop_name,arrival_time)
  
  
  
  ## SUMMARIZE DATA
  
  routes_per_stop <- route_data %>% distinct(stop_id, route_short_name) %>%
                                    group_by(stop_id) %>%
                                    summarize(route_count = n())
  
  route_freq_detail <- route_data %>% select(route_short_name, service_type, direction_id, 
                                              trip_id, arrival_time, arrival_time_adj) %>%
                                       mutate(arrival_datetime = 
                                                ifelse(as.numeric(substr(arrival_time,1,2))>23,
                                                    paste(Sys.Date() + 1,arrival_time_adj, sep = " "),
                                                    paste(Sys.Date(), arrival_time_adj, sep = " "))) %>%
                                       select(route_short_name, direction_id, service_type, 
                                              trip_id, arrival_datetime) %>%
                                       group_by(route_short_name, direction_id, service_type, trip_id) %>%
                                       summarize(start_time = min(arrival_datetime),
                                                 end_time   = max(arrival_datetime)) %>%
                                       arrange(route_short_name, direction_id, service_type, start_time) %>%
                                       mutate(id = row_number()) %>%
                                       ungroup() %>%
                                       mutate(start_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S"),
                                              end_time = as.POSIXct(end_time, format = "%Y-%m-%d %H:%M:%S"),
                                              hour = hour(start_time),
                                              day_type = ifelse(service_type %in% c("Saturday", "Sunday"),
                                                                  "Weekend","Weekday"),
                                              peak_type = ifelse(day_type == "Weekday",
                                                                 ifelse(hour %in% peak_am, "AM Peak",
                                                                        ifelse(hour %in% peak_pm, "PM Peak",
                                                                               "Off Peak")), "Off Peak"),
                                              headway = ifelse((route_short_name != lead(route_short_name) |
                                                               direction_id != lead(direction_id) |
                                                               service_type != lead(service_type)),
                                                               difftime(start_time, lag(start_time), units = c("min")),
                                                               difftime(lead(start_time), start_time, units = c("min"))))
  route_freq_detail$hour <- factor(route_freq_detail$hour, ordered = TRUE,
                                    levels = hour_order)
  
  freq_cat <- route_freq_detail %>% filter(!route_short_name %in% c(148, 143, 201, 221)) %>%
                                      select(route_short_name, day_type, peak_type, hour, headway) %>%
                                      group_by(route_short_name, day_type, peak_type, hour) %>%
                                      summarize(avg_headway = mean(headway, na.rm = TRUE)) %>%
                                      arrange(route_short_name, day_type, hour) %>%
                                      mutate(freq_type = case_when(avg_headway < 10 ~ "A",
                                                                   avg_headway >= 10 & avg_headway < 16 ~ "B",
                                                                   avg_headway >= 16 & avg_headway < 21 ~ "C",
                                                                   avg_headway >= 21 & avg_headway < 31 ~ "D",
                                                                   avg_headway >= 31 & avg_headway <= 60 ~ "E",
                                                                   avg_headway > 60 ~ "F"))

  freq_sum <- freq_cat %>% ungroup() %>%
                            select(day_type, hour, freq_type) %>%
                            group_by(day_type, hour, freq_type) %>%
                            summarize(route_count = n())

  
  
  # Histogram on a Categorical variable
  g <- ggplot(stop_freq_sum, aes(hour, stop_count))
  g + geom_bar(stat = "identity", aes(fill=freq_type), width = 0.5) + 
    theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
    labs(title="Histogram on Route Frequency", 
         subtitle="Hours across Frequency LOS by Day of Week",
         y = "Count of Stops",
         x = "Service Hour") +
    facet_grid(day_type ~ .) +
    scale_fill_discrete(name="Frequency LOS",
                        breaks=c("A", "B", "C", "D", "E", "F"),
                        labels=c("A: <10 mins", "B: 10-15 mins", "C: 16-20 mins",
                                 "D: 21-30 mins", "E: 31-60 mins", "F: >60 mins")) +
    scale_x_discrete(labels=c("4" = "4:00 AM", "5" = "5:00 AM", "6" = "6:00 AM",
                              "7" = "7:00 AM", "8" = "8:00 AM", "9" = "9:00 AM",
                              "10" = "10:00 AM","11" = "11:00 AM", "12" = "12:00 PM", 
                              "13" = "1:00 PM","14" = "2:00 PM", "15" = "3:00 PM", 
                              "16" = "4:00 PM","17" = "5:00 PM", "18" = "6:00 PM", 
                              "19" = "7:00 PM","20" = "8:00 PM", "21" = "9:00 PM", 
                              "22" = "10:00 PM","23" = "11:00 PM", "0" = "12:00 AM", 
                              "1" = "1:00 AM","2" = "2:00 AM"))
  
  
  stop_freq_detail <- route_data %>% filter(!route_short_name %in% c(148, 143, 201, 221)) %>%
                                     select(stop_id, service_type,trip_id, 
                                            arrival_time, arrival_time_adj) %>%
                                     mutate(arrival_datetime = 
                                              ifelse(as.numeric(substr(arrival_time,1,2))>23,
                                                     paste(Sys.Date() + 1,arrival_time_adj, sep = " "),
                                                     paste(Sys.Date(), arrival_time_adj, sep = " ")),
                                            arrival_datetime = as.POSIXct(arrival_datetime, format = "%Y-%m-%d %H:%M:%S")) %>%
                                     arrange(stop_id, service_type, arrival_datetime) %>%
                                     mutate(hour = hour(arrival_datetime),
                                            day_type = ifelse(service_type %in% c("Saturday", "Sunday"),
                                                              "Weekend","Weekday"),
                                            headway = ifelse((stop_id != lead(stop_id) |
                                                              service_type != lead(service_type)),
                                                              difftime(arrival_datetime, lag(arrival_datetime), units = c("min")),
                                                              difftime(lead(arrival_datetime), arrival_datetime, units = c("min"))))
  
  
  stop_freq_cat <- stop_freq_detail %>% select(stop_id, day_type, hour, headway) %>%
                                        group_by(stop_id, day_type, hour) %>%
                                        summarize(avg_headway = mean(headway, na.rm = TRUE)) %>%
                                        arrange(stop_id, day_type, hour) %>%
                                        mutate(freq_type = case_when(avg_headway < 10 ~ "A",
                                                                     avg_headway >= 10 & avg_headway < 16 ~ "B",
                                                                     avg_headway >= 16 & avg_headway < 21 ~ "C",
                                                                     avg_headway >= 21 & avg_headway < 31 ~ "D",
                                                                     avg_headway >= 31 & avg_headway <= 60 ~ "E",
                                                                     avg_headway > 60 ~ "F"))
  
  stop_freq_sum <- stop_freq_cat %>% ungroup() %>%
                                 select(day_type, hour, freq_type) %>%
                                 group_by(day_type, hour, freq_type) %>%
                                 summarize(stop_count = n())
  