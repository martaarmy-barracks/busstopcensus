
  # Import Bus Stop Census Survey Responses
  raw_df <- read.csv(here("data", "raw_data", "survey", "2021-01-01_raw_bus_stop_census_responses.csv"))

  # Clean data of identical records
  raw_df_deduped <- raw_df %>% distinct_at(vars(-Record_ID), .keep_all = TRUE)
  
  # Merge 'Another.HUGE.inconvenience' into 'Additional_Comments' field and remove
  raw_df_deduped_clean <- raw_df_deduped %>% mutate(Additional_Comments = 
                                                        ifelse(is.na(Another.HUGE.inconvenience), Additional_Comments,
                                                               paste(Additional_Comments, Another.HUGE.inconvenience, sep = " "))) %>%
                                             select(-Another.HUGE.inconvenience)
  
  # Remove trailing columns
  raw_df_deduped_clean <- raw_df_deduped_clean %>% select(-To, -Date, -Subject, -Cc, -X.From)
  
  # Convert Timestamp To R Readable Format
  raw_df_deduped_clean$Timestamp_Sub <- substring(raw_df_deduped_clean$Timestamp, 0, 24) # Take substring of timestamp to reformat
  raw_df_deduped_clean$Timestamp_Sub <- as.POSIXct(raw_df_deduped_clean$Timestamp_Sub, format = "%a %b %d %Y %H:%M:%S") # Convert to POSIXct
  raw_df_deduped_clean <- raw_df_deduped_clean %>% mutate(Timestamp = Timestamp_Sub) %>%
                                                   select(-Timestamp_Sub)
  
  # Collect GTFS Stop Data to Compare to Survey Responses
  stop_comparison_data <- stops %>% select(stop_id, stop_name, stop_lat, stop_lon) %>%
                                    separate(stop_name, c("main_street_or_station", "cross_street"), sep = "@")
  
  # Join Survey Responses and GTFS Data
  raw_df_deduped_clean_joined <- raw_df_deduped_clean %>% left_join(stop_comparison_data, by = c("stop_id"))
  
  
  #Data Output
  write.csv(survey_df, file = here("data", "tidy_data", "pre_processed_survey_df.csv"))
  
            