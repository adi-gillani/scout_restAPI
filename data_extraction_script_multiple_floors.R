library(jsonlite)
library(dplyr)

#use the script below to extract data from NeedInsights RestAPI

# PART 1 - Extracting data for Daily Unique Footfall Count - both first_floor and ground_floor 

#extracting data for ground_floor store daily footfall

#feeding the API URL, extracting data and data framing it!
daily_ground_floor_footfall_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_count_unique_days&zone_code=9590&time_start=2019-10-21&time_stop=2019-11-24&format=json"
ground_floor_footfall_daily <- fromJSON(daily_ground_floor_footfall_url)
ground_floor_footfall_daily_df <- as.data.frame(ground_floor_footfall_daily)

#renaming columns
ground_floor_cols <- c("date", "zone_code", "zone_name", "mac_count", "ground_floor_footfall")
colnames(ground_floor_footfall_daily_df) <- ground_floor_cols

#feed, extract, frame
daily_first_floor_footfall_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_count_unique_days&zone_code=9591&time_start=2019-10-21&time_stop=2019-11-24&format=json"
first_floor_store_daily <- fromJSON(daily_first_floor_footfall_url)
first_floor_footfall_daily_df <- as.data.frame(first_floor_store_daily)

#renaming columns
first_floor_cols <- c("date", "zone_code", "zone_name", "mac_count", "first_floor_footfall")
colnames(first_floor_footfall_daily_df) <- first_floor_cols

#combining first_floor and ground_floor visitors
daily_footfall_daily <- merge(ground_floor_footfall_daily_df, first_floor_footfall_daily_df, by = "date")
daily_footfall_daily <- daily_footfall_daily[,c("date","first_floor_footfall","ground_floor_footfall")]

#calculating conversion
daily_footfall_daily <- mutate(daily_footfall_daily, conversion = (as.numeric(daily_footfall_daily$first_floor_footfall)/as.numeric(daily_footfall_daily$ground_floor_footfall)))


#assigning appropriate column types

daily_footfall_daily$date <- as.Date(daily_footfall_daily$date)
daily_footfall_daily$first_floor_footfall <- as.numeric(daily_footfall_daily$first_floor_footfall)
daily_footfall_daily$ground_floor_footfall <- as.numeric(daily_footfall_daily$ground_floor_footfall)
daily_footfall_daily$conversion <- as.numeric(daily_footfall_daily$conversion)

#exporting the data to a csv
write.csv(daily_footfall_daily, "D:/Sen Heng/Scout Data - October 21 - November 24/daily_footfall_trend.csv", row.names = FALSE)


#PART 2 - Extracting data for Hourly Unique Footfall Count

#feed, extract, frame - ground_floor STORE

ground_floor_store_hourly_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_count_unique_hours&zone_code=9590&time_start=2019-10-21&time_stop=2019-11-24&format=json"
ground_floor_store_hourly <- fromJSON(ground_floor_store_hourly_url)
ground_floor_store_hourly_df <- as.data.frame(ground_floor_store_hourly)

#renaming columns
hourly_ground_floor_cols <- c("time", "zone_code", "zone_name", "count_macs", "ground_floor_footfall")
colnames(ground_floor_store_hourly_df) <- hourly_ground_floor_cols

#feed, extract, frame - first_floor STORE

first_floor_store_hourly_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_count_unique_hours&zone_code=9591&time_start=2019-10-21&time_stop=2019-11-24&format=json"
first_floor_store_hourly <- fromJSON(first_floor_store_hourly_url)
first_floor_store_hourly_df <- as.data.frame(first_floor_store_hourly)

#renaming columns
first_floor_hourly_cols <- c("time", "zone_code", "zone_name", "count_macs", "first_floor_footfall")
colnames(first_floor_store_hourly_df) <- first_floor_hourly_cols

#combining first_floor and ground_floor hourly visitors

hourly_footfall_trend <- inner_join(ground_floor_store_hourly_df, first_floor_store_hourly_df, by = "time")
hourly_footfall_trend <- hourly_footfall_trend[,c("time", "ground_floor_footfall", "first_floor_footfall")]

#calculating conversion
hourly_footfall_trend <- mutate(hourly_footfall_trend, conversion = as.numeric(hourly_footfall_trend$first_floor_footfall)/as.numeric(hourly_footfall_trend$ground_floor_footfall))

#exportinf the data to a csv
write.csv(hourly_footfall_trend, "D:/Sen Heng/Scout Data - October 21 - November 24/hourly_footfall_trend.csv", row.names = FALSE)

# Extracting Average Duration per Week

#feed, extract, frame - ground_floor STORE

ground_floor_avg_duration_weekly_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_duration_average_weeks&zone_code=9590&time_start=2019-10-21&time_stop=2019-11-24&format=json"
ground_floor_avg_duration_weekly <- fromJSON(ground_floor_avg_duration_weekly_url)
ground_floor_avg_duration_weekly_df <- as.data.frame(ground_floor_avg_duration_weekly)

#renaming columns
ground_floor_avg_duration_weekly_cols <- c("date", "zone_code", "zone_name", "avg_duration_ground_floor", "count_macs")
colnames(ground_floor_avg_duration_weekly_df) <- ground_floor_avg_duration_weekly_cols

#feed, extract, frame - first_floor STORE

first_floor_avg_duration_weekly_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_duration_average_weeks&zone_code=9591&time_start=2019-10-21&time_stop=2019-11-24&format=json"
first_floor_avg_duration_weekly <- fromJSON(first_floor_avg_duration_weekly_url)
first_floor_avg_duration_weekly_df <- as.data.frame(first_floor_avg_duration_weekly)

#renaming columns
first_floor_avg_duration_weekly_cols <- c("date", "zone_code", "zone_name", "avg_duration_first_floor", "count_macs")
colnames(first_floor_avg_duration_weekly_df) <- first_floor_avg_duration_weekly_cols

#combining first_floor and ground_floor store avg weekly duration

avg_duration_weekly <- inner_join(ground_floor_avg_duration_weekly_df, first_floor_avg_duration_weekly_df, by = "date")
avg_duration_weekly <- avg_duration_weekly[,c("date", "avg_duration_ground_floor", "avg_duration_first_floor")]

#exporing the data to a csv
write.csv(avg_duration_weekly, "D:/Sen Heng/Scout Data - October 21 - November 24/avg_duration_weekly.csv", row.names = FALSE)

#PART 4 - Extracting Zone Visit Frequency per Week split in Bins

#feed, extract, frame - first_floor STORE
first_floor_visit_frequency_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_frequency_weeks&zone_code=9591&time_start=2019-10-21&time_stop=2019-11-24&format=json"
first_floor_visit_frequency <- fromJSON(first_floor_visit_frequency_url)
first_floor_visit_frequency_df <- as.data.frame(first_floor_visit_frequency)

#renaming columns
first_floor_visit_frequency_cols <- c("date", "zone_code", "zone_name", "visit_frequency", "label", "count_macs", "first_floor_percentage")
colnames(first_floor_visit_frequency_df) <- first_floor_visit_frequency_cols


#feed, extract, frame - ground_floor STORE
ground_floor_visit_frequency_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_frequency_weeks&zone_code=9590&time_start=2019-10-21&time_stop=2019-11-24&format=json"
ground_floor_visit_frequency <- fromJSON(ground_floor_visit_frequency_url)
ground_floor_visit_frequency_df <- as.data.frame(ground_floor_visit_frequency)

#renaming columns
ground_floor_visit_frequency_cols <- c("date", "zone_code", "zone_name", "visit_frequency", "label", "count_macs", "ground_floor_percentage")
colnames(ground_floor_visit_frequency_df) <- ground_floor_visit_frequency_cols

#combining data for first_floor and ground_floor store

visit_frequency <- inner_join(ground_floor_visit_frequency_df, first_floor_visit_frequency_df, by = c("date" = "date", "label" = "label"))
visit_frequency <- visit_frequency[,c("date", "label", "first_floor_percentage", "ground_floor_percentage")]

#exporting data to a csv
write.csv(visit_frequency, "D:/Sen Heng/Scout Data - October 21 - November 24/visit_frequency.csv",row.names = FALSE)


# PART 5 - Extracting Cell Phone Brands - Weekly
ground_floor_brands_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_brand_weeks&zone_code=9590&time_start=2019-10-21&time_stop=2019-11-24&format=json"
ground_floor_brands <- fromJSON(ground_floor_brands_url)
ground_floor_brands_df <- as.data.frame(ground_floor_brands)

#renaming columns
ground_floor_brands_col <- c("date", "zone_code", "zone_name", "brand_name", "count_macs", "percentage")
colnames(ground_floor_brands_df) <- ground_floor_brands_col

#exporting data to a csv
write.csv(ground_floor_brands_df, "D:/Sen Heng/Scout Data - October 21 - November 24/phone_brands_ground_floor.csv", row.names = FALSE)

#feed, extract, frame - first_floor STORE

first_floor_brands_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_brand_weeks&zone_code=9591&time_start=2019-10-21&time_stop=2019-11-24&format=json"
first_floor_brands <- fromJSON(first_floor_brands_url)
first_floor_brands_df <- as.data.frame(first_floor_brands)

#renaming columns
colnames(first_floor_brands_df) <- ground_floor_brands_col

#exporting data to a csv
write.csv(first_floor_brands_df, "D:/Sen Heng/Scout Data - October 21 - November 24/phone_brands_first_floor.csv", row.names = FALSE)


#PART 6 - Extracting Duration per Week divided in Slots

#feed, extract, frame - ground_floor STORE
ground_floor_zone_duration_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_duration_weeks&zone_code=9590&time_start=2019-10-21&time_stop=2019-11-24&format=json"
ground_floor_zone_duration <- fromJSON(ground_floor_zone_duration_url)
ground_floor_zone_duration_df <- as.data.frame(ground_floor_zone_duration)

#renaming columns
ground_floor_zone_cols <- c("date", "zone_code", "zone_name", "duration_interval", "duration_label", "count_macs", "ground_floor_percentage")
colnames(ground_floor_zone_duration_df) <- ground_floor_zone_cols


#feed, extract, frame - first_floor STORE
first_floor_zone_duration_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_duration_weeks&zone_code=9591&time_start=2019-10-21&time_stop=2019-11-24&format=json"
first_floor_zone_duration <- fromJSON(first_floor_zone_duration_url)
first_floor_zone_duration_df <- as.data.frame(first_floor_zone_duration)

#renaming columns
first_floor_zone_cols <- c("date", "zone_code", "zone_name", "duration_interval", "duration_label", "count_macs", "first_floor_percentage")
colnames(first_floor_zone_duration_df) <- first_floor_zone_cols

#combining first_floor and ground_floor store weekly zone duration

zone_duration_weekly <- inner_join(ground_floor_zone_duration_df, first_floor_zone_duration_df, by =c("date" = "date", "duration_label" = "duration_label"))
zone_duration_weekly <- zone_duration_weekly[, c("date", "duration_label", "ground_floor_percentage", "first_floor_percentage")]

#exporting data to a csv
write.csv(zone_duration_weekly, "D:/Sen Heng/Scout Data - October 21 - November 24/zone_duration_weekly.csv", row.names = FALSE)