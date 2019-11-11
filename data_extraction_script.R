library(jsonlite)
library(dplyr)

#use the script below to extract data from NeedInsights RestAPI

# PART 1 - Extracting data for Daily Unique Footfall Count - both inside and outside 

#extracting data for outside store daily footfall

#feeding the API URL, extracting data and data framing it!
daily_outside_footfall_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_count_unique_days&zone_code=444&time_start=2019-09-23&time_stop=2019-11-03&format=json"
outside_footfall_daily <- fromJSON(daily_outside_footfall_url)
outside_footfall_daily_df <- as.data.frame(outside_footfall_daily)

#renaming columns
outside_cols <- c("date", "zone_code", "zone_name", "mac_count", "outside_footfall")
colnames(outside_footfall_daily_df) <- outside_cols

#feed, extract, frame
daily_inside_footfall_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_count_unique_days&zone_code=901&time_start=2019-09-23&time_stop=2019-11-03&format=json"
inside_store_daily <- fromJSON(daily_inside_footfall_url)
inside_footfall_daily_df <- as.data.frame(inside_store_daily)

#renaming columns
inside_cols <- c("date", "zone_code", "zone_name", "mac_count", "inside_footfall")
colnames(inside_footfall_daily_df) <- inside_cols

#combining inside and outside visitors
daily_footfall_daily <- merge(outside_footfall_daily_df, inside_footfall_daily_df, by = "date")
daily_footfall_daily <- daily_footfall_daily[,c("date","inside_footfall","outside_footfall")]

#calculating conversion
daily_footfall_daily <- mutate(daily_footfall_daily, conversion = (as.numeric(daily_footfall_daily$inside_footfall)/as.numeric(daily_footfall_daily$outside_footfall)))

#exporting the data to a csv
write.csv(daily_footfall_daily, "D:/Philips Publika/Scout Data Files/daily_footfall_trend.csv", row.names = FALSE)


#PART 2 - Extracting data for Hourly Unique Footfall Count

#feed, extract, frame - OUTSIDE STORE

outside_store_hourly_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_count_unique_hours&zone_code=444&time_start=2019-09-23&time_stop=2019-11-03&format=json"
outside_store_hourly <- fromJSON(outside_store_hourly_url)
outside_store_hourly_df <- as.data.frame(outside_store_hourly)

#renaming columns
hourly_outside_cols <- c("time", "zone_code", "zone_name", "count_macs", "outside_footfall")
colnames(outside_store_hourly_df) <- hourly_outside_cols

#feed, extract, frame - INSIDE STORE

inside_store_hourly_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_count_unique_hours&zone_code=901&time_start=2019-09-23&time_stop=2019-11-03&format=json"
inside_store_hourly <- fromJSON(inside_store_hourly_url)
inside_store_hourly_df <- as.data.frame(inside_store_hourly)

#renaming columns
inside_hourly_cols <- c("time", "zone_code", "zone_name", "count_macs", "inside_footfall")
colnames(inside_store_hourly_df) <- inside_hourly_cols

#combining inside and outside hourly visitors

hourly_footfall_trend <- inner_join(outside_store_hourly_df, inside_store_hourly_df, by = "time")
hourly_footfall_trend <- hourly_footfall_trend[,c("time", "outside_footfall", "inside_footfall")]

#calculating conversion
hourly_footfall_trend <- mutate(hourly_footfall_trend, conversion = as.numeric(hourly_footfall_trend$inside_footfall)/as.numeric(hourly_footfall_trend$outside_footfall))

#exportinf the data to a csv
write.csv(hourly_footfall_trend, "D:/Philips Publika/Scout Data Files/hourly_footfall_trend.csv", row.names = FALSE)

# Extracting Average Duration per Week

#feed, extract, frame - OUTSIDE STORE

outside_avg_duration_weekly_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_duration_average_weeks&zone_code=444&time_start=2019-09-23&time_stop=2019-11-03&format=json"
outside_avg_duration_weekly <- fromJSON(outside_avg_duration_weekly_url)
outside_avg_duration_weekly_df <- as.data.frame(outside_avg_duration_weekly)

#renaming columns
outside_avg_duration_weekly_cols <- c("date", "zone_code", "zone_name", "avg_duration_outside", "count_macs")
colnames(outside_avg_duration_weekly_df) <- outside_avg_duration_weekly_cols

#feed, extract, frame - INSIDE STORE

inside_avg_duration_weekly_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_duration_average_weeks&zone_code=901&time_start=2019-09-23&time_stop=2019-11-03&format=json"
inside_avg_duration_weekly <- fromJSON(inside_avg_duration_weekly_url)
inside_avg_duration_weekly_df <- as.data.frame(inside_avg_duration_weekly)

#renaming columns
inside_avg_duration_weekly_cols <- c("date", "zone_code", "zone_name", "avg_duration_inside", "count_macs")
colnames(inside_avg_duration_weekly_df) <- inside_avg_duration_weekly_cols

#combining inside and outside store avg weekly duration

avg_duration_weekly <- inner_join(outside_avg_duration_weekly_df, inside_avg_duration_weekly_df, by = "date")
avg_duration_weekly <- avg_duration_weekly[,c("date", "avg_duration_outside", "avg_duration_inside")]

#exporing the data to a csv
write.csv(avg_duration_weekly, "D:/Philips Publika/Scout Data Files/avg_duration_weekly.csv", row.names = FALSE)

#PART 4 - Extracting Zone Visit Frequency per Week split in Bins

#feed, extract, frame - INSIDE STORE
inside_visit_frequency_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_frequency_weeks&zone_code=901&time_start=2019-09-23&time_stop=2019-11-03&format=json"
inside_visit_frequency <- fromJSON(inside_visit_frequency_url)
inside_visit_frequency_df <- as.data.frame(inside_visit_frequency)

#renaming columns
inside_visit_frequency_cols <- c("date", "zone_code", "zone_name", "visit_frequency", "label", "count_macs", "inside_percentage")
colnames(inside_visit_frequency_df) <- inside_visit_frequency_cols


#feed, extract, frame - OUTSIDE STORE
outside_visit_frequency_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_frequency_weeks&zone_code=444&time_start=2019-09-23&time_stop=2019-11-03&format=json"
outside_visit_frequency <- fromJSON(outside_visit_frequency_url)
outside_visit_frequency_df <- as.data.frame(outside_visit_frequency)

#renaming columns
outside_visit_frequency_cols <- c("date", "zone_code", "zone_name", "visit_frequency", "label", "count_macs", "outside_percentage")
colnames(outside_visit_frequency_df) <- outside_visit_frequency_cols

#combining data for inside and outside store

visit_frequency <- inner_join(outside_visit_frequency_df, inside_visit_frequency_df, by = c("date" = "date", "label" = "label"))
visit_frequency <- visit_frequency[,c("date", "label", "inside_percentage", "outside_percentage")]

#exporting data to a csv
write.csv(visit_frequency, "D:/Philips Publika/Scout Data Files/visit_frequency.csv",row.names = FALSE)


# PART 5 - Extracting Cell Phone Brands - Weekly
outside_brands_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_brand_weeks&zone_code=444&time_start=2019-09-23&time_stop=2019-11-03&format=json"
outside_brands <- fromJSON(outside_brands_url)
outside_brands_df <- as.data.frame(outside_brands)

#renaming columns
outside_brands_col <- c("date", "zone_code", "zone_name", "brand_name", "count_macs", "percentage")
colnames(outside_brands_df) <- outside_brands_col

#exporting data to a csv
write.csv(outside_brands_df, "D:/Philips Publika/Scout Data Files/phone_brands_outside.csv", row.names = FALSE)

#feed, extract, frame - INSIDE STORE

inside_brands_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_brand_weeks&zone_code=901&time_start=2019-09-23&time_stop=2019-11-03&format=json"
inside_brands <- fromJSON(inside_brands_url)
inside_brands_df <- as.data.frame(inside_brands)

#renaming columns
colnames(inside_brands_df) <- outside_brands_col

#exporting data to a csv
write.csv(inside_brands_df, "D:/Philips Publika/Scout Data Files/phone_brands_outside.csv", row.names = FALSE)


#PART 6 - Extracting Duration per Week divided in Slots

#feed, extract, frame - OUTSIDE STORE
outside_zone_duration_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_duration_weeks&zone_code=444&time_start=2019-09-23&time_stop=2019-11-03&format=json"
outside_zone_duration <- fromJSON(outside_zone_duration_url)
outside_zone_duration_df <- as.data.frame(outside_zone_duration)

#renaming columns
outside_zone_cols <- c("date", "zone_code", "zone_name", "duration_interval", "duration_label", "count_macs", "outside_percentage")
colnames(outside_zone_duration_df) <- outside_zone_cols


#feed, extract, frame - INSIDE STORE
inside_zone_duration_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_duration_weeks&zone_code=901&time_start=2019-09-23&time_stop=2019-11-03&format=json"
inside_zone_duration <- fromJSON(inside_zone_duration_url)
inside_zone_duration_df <- as.data.frame(inside_zone_duration)

#renaming columns
inside_zone_cols <- c("date", "zone_code", "zone_name", "duration_interval", "duration_label", "count_macs", "inside_percentage")
colnames(inside_zone_duration_df) <- inside_zone_cols

#combining inside and outside store weekly zone duration

zone_duration_weekly <- inner_join(outside_zone_duration_df, inside_zone_duration_df, by =c("date" = "date", "duration_label" = "duration_label"))
zone_duration_weekly <- zone_duration_weekly[, c("date", "duration_label", "outside_percentage", "inside_percentage")]

#exporting data to a csv
write.csv(zone_duration_weekly, "D:/Philips Publika/Scout Data Files/zone_duration_weekly.csv", row.names = FALSE)