library(jsonlite)
library(dplyr)

#use the script below to extract data from NeedInsights RestAPI

# PART 1 - Extracting data for Daily Unique Footfall Count - both actual and potential 

#extracting data for potential store daily footfall

#feeding the API URL, extracting data and data framing it!
daily_potential_footfall_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_count_unique_days&zone_code=5346&time_start=2019-12-24&time_stop=2019-12-29&format=json"
potential_footfall_daily <- fromJSON(daily_potential_footfall_url)
potential_footfall_daily_df <- as.data.frame(potential_footfall_daily)

#renaming columns
potential_cols <- c("date", "zone_code", "zone_name", "mac_count", "potential_footfall")
colnames(potential_footfall_daily_df) <- potential_cols

#feed, extract, frame
daily_actual_footfall_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_count_unique_days&zone_code=5341&time_start=2019-12-24&time_stop=2019-12-29&format=json"
actual_store_daily <- fromJSON(daily_actual_footfall_url)
actual_footfall_daily_df <- as.data.frame(actual_store_daily)

#renaming columns
actual_cols <- c("date", "zone_code", "zone_name", "mac_count", "actual_footfall")
colnames(actual_footfall_daily_df) <- actual_cols

#combining actual and potential visitors
daily_footfall_daily <- merge(potential_footfall_daily_df, actual_footfall_daily_df, by = "date")
daily_footfall_daily <- daily_footfall_daily[,c("date","actual_footfall","potential_footfall")]

#calculating conversion
daily_footfall_daily <- mutate(daily_footfall_daily, conversion = (as.numeric(daily_footfall_daily$actual_footfall)/as.numeric(daily_footfall_daily$potential_footfall)))


#assigning appropriate column types

daily_footfall_daily$date <- as.Date(daily_footfall_daily$date)
daily_footfall_daily$actual_footfall <- as.numeric(daily_footfall_daily$actual_footfall)
daily_footfall_daily$potential_footfall <- as.numeric(daily_footfall_daily$potential_footfall)
daily_footfall_daily$conversion <- as.numeric(daily_footfall_daily$conversion)

#exporting the data to a csv
write.csv(daily_footfall_daily, "./daily_footfall_trend.csv", row.names = FALSE)


#PART 2 - Extracting data for Hourly Unique Footfall Count

#feed, extract, frame - potential STORE

potential_store_hourly_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_count_unique_hours&zone_code=5346&time_start=2019-12-24&time_stop=2019-12-30&format=json"
potential_store_hourly <- fromJSON(potential_store_hourly_url)
potential_store_hourly_df <- as.data.frame(potential_store_hourly)

#renaming columns
hourly_potential_cols <- c("time", "zone_code", "zone_name", "count_macs", "potential_footfall")
colnames(potential_store_hourly_df) <- hourly_potential_cols

#feed, extract, frame - actual STORE

actual_store_hourly_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_count_unique_hours&zone_code=5341&time_start=2019-12-24&time_stop=2019-12-30&format=json"
actual_store_hourly <- fromJSON(actual_store_hourly_url)
actual_store_hourly_df <- as.data.frame(actual_store_hourly)

#renaming columns
actual_hourly_cols <- c("time", "zone_code", "zone_name", "count_macs", "actual_footfall")
colnames(actual_store_hourly_df) <- actual_hourly_cols

#combining actual and potential hourly visitors

hourly_footfall_trend <- inner_join(potential_store_hourly_df, actual_store_hourly_df, by = "time")
hourly_footfall_trend <- hourly_footfall_trend[,c("time", "potential_footfall", "actual_footfall")]

#calculating conversion
hourly_footfall_trend <- mutate(hourly_footfall_trend, conversion = as.numeric(hourly_footfall_trend$actual_footfall)/as.numeric(hourly_footfall_trend$potential_footfall))

#exportinf the data to a csv
write.csv(hourly_footfall_trend, "./hourly_footfall_trend.csv", row.names = FALSE)

# Extracting Average Duration per Day

#feed, extract, frame - potential STORE

potential_avg_duration_daily_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_duration_average_days&zone_code=5346&time_start=2019-12-24&time_stop=2019-12-29&format=json"
potential_avg_duration_daily <- fromJSON(potential_avg_duration_daily_url)
potential_avg_duration_daily_df <- as.data.frame(potential_avg_duration_daily)

#renaming columns
potential_avg_duration_daily_cols <- c("date", "zone_code", "zone_name", "avg_duration_potential", "count_macs")
colnames(potential_avg_duration_daily_df) <- potential_avg_duration_daily_cols

#feed, extract, frame - actual STORE

actual_avg_duration_daily_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_duration_average_days&zone_code=5341&time_start=2019-12-24&time_stop=2019-12-29&format=json"
actual_avg_duration_daily <- fromJSON(actual_avg_duration_daily_url)
actual_avg_duration_daily_df <- as.data.frame(actual_avg_duration_daily)

#renaming columns
actual_avg_duration_daily_cols <- c("date", "zone_code", "zone_name", "avg_duration_actual", "count_macs")
colnames(actual_avg_duration_daily_df) <- actual_avg_duration_daily_cols

#combining actual and potential store avg weekly duration

avg_duration_daily <- inner_join(potential_avg_duration_daily_df, actual_avg_duration_daily_df, by = "date")
avg_duration_daily <- avg_duration_daily[,c("date", "avg_duration_potential", "avg_duration_actual")]

#exporing the data to a csv
write.csv(avg_duration_daily, "./avg_duration_daily.csv", row.names = FALSE)

#PART 4 - Extracting Zone Visit Frequency per Week split in Bins

#feed, extract, frame - actual STORE
#actual_visit_frequency_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_frequency_weeks&zone_code=5341&time_start=2019-12-24&time_stop=2019-12-29&format=json"
#actual_visit_frequency <- fromJSON(actual_visit_frequency_url)
#actual_visit_frequency_df <- as.data.frame(actual_visit_frequency)

#renaming columns
#actual_visit_frequency_cols <- c("date", "zone_code", "zone_name", "visit_frequency", "label", "count_macs", "actual_percentage")
#colnames(actual_visit_frequency_df) <- actual_visit_frequency_cols


#feed, extract, frame - potential STORE
#potential_visit_frequency_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_frequency_weeks&zone_code=5346&time_start=2019-12-24&time_stop=2019-12-29&format=json"
#potential_visit_frequency <- fromJSON(potential_visit_frequency_url)
#potential_visit_frequency_df <- as.data.frame(potential_visit_frequency)

#renaming columns
#potential_visit_frequency_cols <- c("date", "zone_code", "zone_name", "visit_frequency", "label", "count_macs", "potential_percentage")
#colnames(potential_visit_frequency_df) <- potential_visit_frequency_cols

#combining data for actual and potential store

#visit_frequency <- inner_join(potential_visit_frequency_df, actual_visit_frequency_df, by = c("date" = "date", "label" = "label"))
#visit_frequency <- visit_frequency[,c("date", "label", "actual_percentage", "potential_percentage")]

#exporting data to a csv
#write.csv(visit_frequency, "./visit_frequency.csv",row.names = FALSE)


# PART 5 - Extracting Cell Phone Brands - Daily
potential_brands_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_brand_days&zone_code=5346&time_start=2019-12-24&time_stop=2019-12-29&format=json"
potential_brands <- fromJSON(potential_brands_url)
potential_brands_df <- as.data.frame(potential_brands)

#renaming columns
potential_brands_col <- c("date", "zone_code", "zone_name", "brand_name", "count_macs", "percentage")
colnames(potential_brands_df) <- potential_brands_col

#exporting data to a csv
write.csv(potential_brands_df, "./phone_brands_potential.csv", row.names = FALSE)

#feed, extract, frame - actual STORE

actual_brands_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_brand_days&zone_code=5341&time_start=2019-12-24&time_stop=2019-12-29&format=json"
actual_brands <- fromJSON(actual_brands_url)
actual_brands_df <- as.data.frame(actual_brands)

#renaming columns
colnames(actual_brands_df) <- potential_brands_col

#exporting data to a csv
write.csv(actual_brands_df, "./phone_brands_actual.csv", row.names = FALSE)


#PART 6 - Extracting Duration per Day divided in Slots

#feed, extract, frame - potential STORE
potential_zone_duration_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_duration_days&zone_code=5346&time_start=2019-12-24&time_stop=2019-12-29&format=json"
potential_zone_duration <- fromJSON(potential_zone_duration_url)
potential_zone_duration_df <- as.data.frame(potential_zone_duration)

#renaming columns
potential_zone_cols <- c("date", "zone_code", "zone_name", "duration_interval", "duration_label", "count_macs", "potential_percentage")
colnames(potential_zone_duration_df) <- potential_zone_cols


#feed, extract, frame - actual STORE
actual_zone_duration_url <- "https://customer.needinsights.com/rest/?api_key=4d76114e78ed1db951cea3fdc6178016644c374b&metric=zone_duration_days&zone_code=5341&time_start=2019-12-24&time_stop=2019-12-29&format=json"
actual_zone_duration <- fromJSON(actual_zone_duration_url)
actual_zone_duration_df <- as.data.frame(actual_zone_duration)

#renaming columns
actual_zone_cols <- c("date", "zone_code", "zone_name", "duration_interval", "duration_label", "count_macs", "actual_percentage")
colnames(actual_zone_duration_df) <- actual_zone_cols

#combining actual and potential store weekly zone duration

zone_duration_weekly <- inner_join(potential_zone_duration_df, actual_zone_duration_df, by =c("date" = "date", "duration_label" = "duration_label"))
zone_duration_weekly <- zone_duration_weekly[, c("date", "duration_label", "potential_percentage", "actual_percentage")]

#exporting data to a csv
write.csv(zone_duration_weekly, "./zone_duration_weekly.csv", row.names = FALSE)
