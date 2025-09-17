
library(rio)
library(tidyverse)
library(data.table)

# Clear the environment 
rm(list= ls(all=TRUE))

#import the data 

industrial_vessels <-fread("/Users/kaiwetzel/Documents/Post Graduation Documents /Bush School/LEDR/Data /industrial_vessels_v20240102.csv")
summary(industrial_vessels)

# Find the lat and longitude of desired area 

# Find the Peruvian Exclusive Economic Zone
industrial_vessels_economic_zone <- industrial_vessels %>% 
  filter(lat >= -20.2081 & lat <= -3.3921 & lon >= -84.6707 & lon <= -70.3796)
summary(industrial_vessels_economic_zone)


# Save the subset of the industrial_vessels to make it easier for replication 
# Save your filtered dataset to a CSV
# Example: Save to a "data" subfolder (make sure it exists)
write.csv(industrial_vessels_economic_zone, "/Users/kaiwetzel/Documents/Post Graduation Documents /Bush School/LEDR/Data /industrial_vessels_economic_zone.csv", row.names = FALSE)
file.info("industrial_vessels_economic_zone.csv")$size / (1024^2)  # Size in MB


#**MMSI Maritime Mobile Service identity** This is a 9 digit number assigned to a digital selective calling (DSC) radio. It acts as a digital fingerprint for vessels, making it easier for other boats and authorities to identify and communication with a ship at sea quickly.

#There are two parts of an MMSI number. The first three digits are the maritime identification digits (MID) that identify the country that issued the number. The last six digits are the Mobile Station Identifier (MSI), which can be any number from 0-9, and is unique to a vessel or radio.


# Filter the boats by Peruvian Vessels by MMSI number starts with 760. This will be the industrial vessels within the economic zone that are identified as Peruvian vessels. 
industrial_vessels_ez_Peru <- industrial_vessels_economic_zone %>%
  filter(grepl("^760", as.character(mmsi)))

#Now we what to find the percentage of Peruvian vessels that were observed in the Peruvian Economic Zone. 
6346/54798


#So 11.58% of all vessels in Peru's Economic Zone are identified as Peruvian. Therefore, 88.419% of vessels are foreign in the Economic Zone.


# Now to count out of the vessels that are matched to fishing, what proportion of them are Peruvian vessels and what are foreign.

# Load libraries 
library(dplyr)

# matched_fishing” if it was matched to a vessel broadcasting AIS and was identified as a fishing vessel
# matched_nonfishing if it was matched to a vessel broadcasting AIS and was identified as a non-fishing vessel
# matched_unknown” if it was matched to a vessel broadcasting AIS and its identity was unknown

# Do a sum of the matched_fishing 
sum(industrial_vessels_ez_Peru$matched_category == "matched_fishing", na.rm = TRUE)

# Find the portion of Peru ships that are matched fishing 
3319/6364

# Do a sum of the matched_nonfishing
sum(industrial_vessels_ez_Peru$matched_category == "matched_nonfishing", na.rm = TRUE)

# Find the portion of Peru ships that are matched nonfishing 
2112/6364

# Do a sum of the matched_unknown
sum(industrial_vessels_ez_Peru$matched_category == "matched_unknown", na.rm = TRUE)

# Find the portion of Peru ships that are matched unknown
933/6364



# Within the Economic Zone of Peru, and the ships that are Peruvian, 52.15% of the ships are matched fishing, 33.186% are matched non fishing and 14.6% of the vessels are matched unknown.



industrial_vessels_ez_Foreign <- industrial_vessels_economic_zone %>%
  filter(!grepl("^760", as.character(mmsi)))

# Do a sum of the matched_fishing 
sum(industrial_vessels_ez_Foreign$matched_category == "matched_fishing", na.rm = TRUE)

# Find the portion of Intl ships that are matched fishing 
1019/48434

# Do a sum of the matched_nonfishing
sum(industrial_vessels_ez_Foreign$matched_category == "matched_nonfishing", na.rm = TRUE)

# Find the portion of Intl ships that are matched_nonfishing
11587/48434

#  Do a sum of the matched_unknown
sum(industrial_vessels_ez_Foreign$matched_category == "matched_unknown", na.rm = TRUE)

# Find the portion of Intl ships that are matched_unknown
849/48434

# Do a sum of the unmatched
sum(industrial_vessels_ez_Foreign$matched_category == "unmatched", na.rm = TRUE)

# Find the portion of Intl ships that are unmatched
34979/48434



# Time to bring in the other dataset so that I can make matches of times to help identify the vessels by looking at the timestamp column. I plan to match by a one hour interval to best match the unidentified to the identified dataset I have from the globalfishing watch data. In theory, the identified fishing vessels from the global fishing watch data should allow us to make connections to the paper's data where vessels are not identified. First I will load the code from the other qmd file to get the data set. 

# Load in some libraries 
library(dplyr)
library(rio)
library(tidyverse)

# import the GeoJSON file to have the long and lat 
# load libraries 
library(sf)
library(geojsonR)

my_geojson <- st_read("/Users/kaiwetzel/Documents/Post Graduation Documents /Bush School/LEDR/Data /Peruvian Exclusive Economic Zone - 2017-01-01_2018-01-01/layer-reference-geometry/geometry.geojson") 
print(my_geojson)

# Convert Industrial_vessels to sf using lat/lon 


# Convert industrial_vessels to sf using lat/lon
industrial_vessels_sf <- st_as_sf(
  industrial_vessels,
  coords = c("lon", "lat"),
  crs = 4326
)

# Save the spatial object
saveRDS(industrial_vessels_sf, "industrial_vessels_sf.rds")

# Filter vessels inside the EEZ using spatial join
# Ensure the EEZ geometry is valid
my_geojson <- st_make_valid(my_geojson)

# Use spatial join to keep only vessels within the EEZ
vessels_in_eez <- st_join(industrial_vessels_sf, my_geojson, join = st_within)

# Optionally drop rows without matches (i.e., outside EEZ)
vessels_in_eez <- vessels_in_eez[!is.na(vessels_in_eez$EEZ_ID_column), ]

#  Save filtered spatial data
saveRDS(vessels_in_eez, "vessels_in_eez.rds")

# Make a plot 
library(ggplot2)

ggplot() +
  geom_sf(data = my_geojson, fill = NA, color = "blue") +
  geom_sf(data = vessels_in_eez, color = "red", size = 0.5) +
  theme_minimal() +
  ggtitle("Industrial Vessels within Peru's EEZ")

# the join didn't work so I am checking the CRS of the layers 
st_crs(industrial_vessels_sf)
st_crs(my_geojson)

# Plot them together 
ggplot() +
  geom_sf(data = my_geojson, fill = NA, color = "blue") +
  geom_sf(data = industrial_vessels_sf, color = "red", size = 0.5) +
  theme_minimal()
library(dplyr)

# Take a small sample
industrial_vessels_sf_sample <- industrial_vessels_sf %>% slice_sample(n = 1000)

# Plot only sample
ggplot() +
  geom_sf(data = my_geojson, fill = NA, color = "blue") +
  geom_sf(data = industrial_vessels_sf_sample, color = "red", size = 0.5) +
  theme_minimal() +
  ggtitle("Sample of Industrial Vessels and Peru EEZ")

#### Compare the first method of filtering to using the geojson 
# Box filtering
industrial_vessels_box <- industrial_vessels %>% 
  filter(lat >= -20.2081 & lat <= -3.3921 & lon >= -84.6707 & lon <= -70.3796)

# Polygon filtering
idx <- st_intersects(industrial_vessels_sf, my_geojson, sparse = FALSE)
vessels_in_eez <- industrial_vessels_sf[apply(idx, 1, any), ]

# Compare
cat("Vessels in box:", nrow(industrial_vessels_box), "\n")
cat("Vessels in EEZ polygon:", nrow(vessels_in_eez), "\n")



####Load in the first layer which is the Automatic Identification System (AIS) collected via satellites and terrestrial receivers

Peru_Economic_Zone_Layer1 <-import("/Users/kaiwetzel/Documents/Post Graduation Documents /Bush School/LEDR/Data /Peruvian Exclusive Economic Zone - 2017-01-01_2018-01-01/layer-activity-data-0/public-global-fishing-effort-v3.0.csv")

# get the summary for the first layer 
summary(Peru_Economic_Zone_Layer1)

# Read the line of the Markdown readme file for layer 1
markdown_text_layer1 <- readLines("/Users/kaiwetzel/Documents/Post Graduation Documents /Bush School/LEDR/Data /Peruvian Exclusive Economic Zone - 2017-01-01_2018-01-01/layer-activity-data-0/readme_public-global-fishing-effort-v3.0.md")

# Print the readme file 
print(markdown_text_layer1)



#Import second layer the Vessel Monitoring System (VMS) provided by the Peruvian Government's Ministry of Production, Fisheries Sector (PRODUCE)

Peru_Economic_Zone_Layer2 <-import("/Users/kaiwetzel/Documents/Post Graduation Documents /Bush School/LEDR/Data /Peruvian Exclusive Economic Zone - 2017-01-01_2018-01-01/layer-activity-data-1/public-peru-fishing-effort-v20211126.csv")

# get the summary for the second layer 
summary(Peru_Economic_Zone_Layer2)

# Read the lines of the Markdown readme file 
markdown_text_layer2 <- readLines("/Users/kaiwetzel/Documents/Post Graduation Documents /Bush School/LEDR/Data /Peruvian Exclusive Economic Zone - 2017-01-01_2018-01-01/layer-activity-data-1/readme_public-peru-fishing-effort.md")

# Print the readme file 
print(markdown_text_layer2)


#Import the third layer to track the vessel presence using the AIS Global Fishing Watch uses data about a vessel’s identity, type, location, speed, direction and more that is broadcast using the Automatic Identification System (AIS) and collected via satellites and terrestrial receivers

Peru_Economic_Zone_Layer3 <-import("/Users/kaiwetzel/Documents/Post Graduation Documents /Bush School/LEDR/Data /Peruvian Exclusive Economic Zone - 2017-01-01_2018-01-01/layer-activity-data-2/public-global-presence-v3.0.csv")


#  get the summary for the third layer 
summary(Peru_Economic_Zone_Layer3)

# Read the lines of the Markdown readme file 
markdown_text_layer3 <- readLines("/Users/kaiwetzel/Documents/Post Graduation Documents /Bush School/LEDR/Data /Peruvian Exclusive Economic Zone - 2017-01-01_2018-01-01/layer-activity-data-2/readme_public-global-presence-v3.0.md")

# Print the readme file 
print(markdown_text_layer3)

# Merge the data sets together to see all the filters into one year from 2017-2018
merged_2017_2018 <-bind_rows(Peru_Economic_Zone_Layer1,Peru_Economic_Zone_Layer2,Peru_Economic_Zone_Layer3)

# Take the Summary Stats of the longer data frame 
summary(merged_2017_2018)


# Do the same for each year, next year being 2018-2019
# To make life easier I will rename the data frame to PEZ for Peruvian Economic Zone

#Load in the first layer which is the Automatic Identification System (AIS) 
PEZ_2018_1 <-import("/Users/kaiwetzel/Documents/Post Graduation Documents /Bush School/LEDR/Data /Peruvian Exclusive Economic Zone - 2018-01-01_2019-01-01/layer-activity-data-0/public-global-fishing-effort-v3.0.csv")

# get the summary for the first layer 
summary(PEZ_2018_1)

# Import second layer the Vessel Monitoring System (VMS) 
PEZ_2018_2 <-import("/Users/kaiwetzel/Documents/Post Graduation Documents /Bush School/LEDR/Data /Peruvian Exclusive Economic Zone - 2018-01-01_2019-01-01/layer-activity-data-1/public-peru-fishing-effort-v20211126.csv")

# get the summary for the second layer 
summary(PEZ_2018_2)

# Import the third layer 
PEZ_2018_3 <-import("/Users/kaiwetzel/Documents/Post Graduation Documents /Bush School/LEDR/Data /Peruvian Exclusive Economic Zone - 2018-01-01_2019-01-01/layer-activity-data-2/public-global-presence-v3.0.csv")

#  get the summary for the third layer 
summary(PEZ_2018_3)

# Merge the data sets together to see all the filters into one year from 2018-2019
merged_2018_2019 <-bind_rows(PEZ_2018_1,PEZ_2018_2,PEZ_2018_3)

# Take the Summary Stats of the longer data frame 
summary(merged_2018_2019)



# Do the same for each year, next year being 2019-2020
# To make life easier I will rename the data frame to PEZ for Peruvian Economic Zone

#Load in the first layer which is the Automatic Identification System (AIS) 
PEZ_2019_1 <-import("/Users/kaiwetzel/Documents/Post Graduation Documents /Bush School/LEDR/Data /Peruvian Exclusive Economic Zone - 2019-01-01_2020-01-01/layer-activity-data-0/public-global-fishing-effort-v3.0.csv")

# get the summary for the first layer 
summary(PEZ_2019_1)

# Import second layer the Vessel Monitoring System (VMS) 
PEZ_2019_2 <-import("/Users/kaiwetzel/Documents/Post Graduation Documents /Bush School/LEDR/Data /Peruvian Exclusive Economic Zone - 2019-01-01_2020-01-01/layer-activity-data-1/public-peru-fishing-effort-v20211126.csv")

# get the summary for the second layer 
summary(PEZ_2019_2)

# Import the third layer 
PEZ_2019_3 <-import("/Users/kaiwetzel/Documents/Post Graduation Documents /Bush School/LEDR/Data /Peruvian Exclusive Economic Zone - 2019-01-01_2020-01-01/layer-activity-data-2/public-global-presence-v3.0.csv")

#  get the summary for the third layer 
summary(PEZ_2019_3)

# Merge the data sets together to see all the filters into one year from 2018-2019
merged_2019_2020 <-bind_rows(PEZ_2019_1,PEZ_2019_2,PEZ_2019_3)

# Take the Summary Stats of the longer data frame 
summary(merged_2019_2020)


# Do the same for each year, next year being 2020-2021
# To make life easier I will rename the data frame to PEZ for Peruvian Economic Zone

#Load in the first layer which is the Automatic Identification System (AIS) 
PEZ_2020_1 <-import("/Users/kaiwetzel/Documents/Post Graduation Documents /Bush School/LEDR/Data /Peruvian Exclusive Economic Zone - 2020-01-01_2021-01-01/layer-activity-data-0/public-global-fishing-effort-v3.0.csv")

# get the summary for the first layer 
summary(PEZ_2020_1)

# Import second layer the Vessel Monitoring System (VMS) 
PEZ_2020_2 <-import("/Users/kaiwetzel/Documents/Post Graduation Documents /Bush School/LEDR/Data /Peruvian Exclusive Economic Zone - 2020-01-01_2021-01-01/layer-activity-data-1/public-peru-fishing-effort-v20211126.csv")

# get the summary for the second layer 
summary(PEZ_2020_2)

# Import the third layer 
PEZ_2020_3 <-import("/Users/kaiwetzel/Documents/Post Graduation Documents /Bush School/LEDR/Data /Peruvian Exclusive Economic Zone - 2020-01-01_2021-01-01/layer-activity-data-2/public-global-presence-v3.0.csv")

#  get the summary for the third layer 
summary(PEZ_2020_3)

# Merge the data sets together to see all the filters into one year from 2020-2021
merged_2020_2021 <-bind_rows(PEZ_2020_1,PEZ_2020_2,PEZ_2020_3)

# Take the Summary Stats of the longer data frame 
summary(merged_2020_2021)

# Now merge all of the individual years into one big data set 2107-2021

total_merge <-bind_rows(merged_2017_2018,merged_2018_2019,merged_2019_2020,merged_2020_2021)
summary(total_merge)

# Save full 2017–2021 merged dataset
saveRDS(total_merge, "total_merge_2017_2021.rds")


#I am looking to do this so that I do not have trouble dropping NAs later. 
#Remerge the data so that there are datasets with just one filter from years 2017-2021.

#The idea is to drop the na's before the entire merge of the different filters 

#First we look to merge the first layer which is the Automatic Identification System (AIS) collected via satellites and terrestrial receivers for 2017-2021.

# Find out the coloumn names 
colnames(PEZ_2020_1)

# Join the data sets together using bind rows 
L1_2017_2021 = bind_rows(Peru_Economic_Zone_Layer1,PEZ_2018_1, PEZ_2019_1,PEZ_2020_1)

# Remove the IMO column from the dataset, we don't use it 
L1_2017_2021= L1_2017_2021  %>% select(-IMO)
summary(L1_2017_2021)

#Do the same for the second filter now

# Join the data sets together using bind rows 
L2_2017_2021 = bind_rows(Peru_Economic_Zone_Layer2,PEZ_2018_2, PEZ_2019_2,PEZ_2020_2)

summary(L2_2017_2021)

# Remove the flag column, because it is all Nas
L2_2017_2021 <- L2_2017_2021 %>% select(-Flag)

# Drop the Nas from the put together data set 
L2_2017_2021= L2_2017_2021 %>% 
drop_na()


#Do the same for the last filter now 


# Join the data sets together using bind rows 
L3_2017_2021 = bind_rows(Peru_Economic_Zone_Layer3,PEZ_2018_3, PEZ_2019_3,PEZ_2020_3)

summary(L3_2017_2021)

# Remove the IMO column 
L3_2017_2021= L3_2017_2021 %>% select(-IMO)
summary(L3_2017_2021)

#Now merge all the data sets together when having taken the NAs out before the entire merge 

complete = bind_rows(L1_2017_2021,L2_2017_2021,L3_2017_2021)

summary(complete)

rm(L1_2017_2021,L2_2017_2021,L3_2017_2021,
   merged_2017_2018, merged_2018_2019, merged_2019_2020, merged_2020_2021,
   Peru_Economic_Zone_Layer1, Peru_Economic_Zone_Layer2, Peru_Economic_Zone_Layer3,
   PEZ_2018_1,PEZ_2018_2, PEZ_2018_2, PEZ_2018_3,
   PEZ_2019_1, PEZ_2019_2, PEZ_2019_3,
   PEZ_2020_1, PEZ_2020_2, PEZ_2020_3)


#** March 21, 2025** 
#I want to add the foreign column, by giving any variable with a "PER" in the Flag column a 0 and all other flags a 1, to indicate that they are foreign. I also want to add another column for if it is a fishing vessel. I am also going to clean up the names within the data set to make everything cleaner and easier to work with. 
# Create foreign and fishing column 
complete <- complete %>%
  mutate(
    foreign = ifelse(substr(Flag, 1, 3) == "PER", 0, 1),
    fishing_vessel = ifelse(`Vessel Type` == "FISHING", 1, 0)
  )

# rename the columns 
complete <- complete %>%
  rename(vessel_type = `Vessel Type`,
         fishing_hours = `Apparent Fishing Hours`,
         time_range = `Time Range`,
         flag = `Flag`,
         entry_timestamp = `Entry Timestamp`,
         exit_timestamp = `Exit Timestamp`,
         gear_type = `Gear Type`,
         first_transmission_date = `First Transmission Date`,
         last_transmission_date = `Last Transmission Date`,
         vessel_presence_hours = `Vessel Presence Hours`,
         vessel_name = `Vessel Name`)

# Rearange the columns  
complete <- complete %>%
  select(MMSI, CallSign, vessel_name, flag, foreign, fishing_vessel,
         fishing_hours, vessel_presence_hours, time_range, entry_timestamp,
         exit_timestamp, first_transmission_date, last_transmission_date)
         
summary(complete)

library(data.table)
# After cleaning/renaming `complete`
saveRDS(complete, "complete_cleaned.rds")

# Replace all previous processing with this:
complete <- readRDS("complete_cleaned.rds")
setDT(complete)

# Ensure entry_timestamp is POSIXct
complete[, entry_timestamp := as.POSIXct(entry_timestamp, tz = "UTC")]

vessels_in_eez <- readRDS("vessels_in_eez.rds")
setDT(vessels_in_eez)
vessels_in_eez[, timestamp := as.POSIXct(timestamp, tz = "UTC")]

############################################################################
#Now that I have brought the other dataset into the QMD file, I want to try to match the timestamps in a range+-30 minutes in efforts to try to identify vessels. 

str(complete)  # Inspect the structure of complete


# Check if the conversion was successful
if (!is.data.table(vessels_in_eez)) setDT(vessels_in_eez)
vessels_in_eez[, timestamp := as.POSIXct(timestamp, tz = "UTC")]

# Define time window (e.g., ±30 minutes)
time_window <- 30 * 60  # 30 minutes in seconds

# Filter only rows where MMSI is NA
geo_na_mmsi_vessels <- vessels_in_eez[is.na(mmsi)]

# Precompute time range in 'complete'
complete[, entry_lower := entry_timestamp - time_window]
complete[, entry_upper := entry_timestamp + time_window]


# Make sure your tables are sorted (required for foverlaps)
setkey(complete, entry_lower, entry_upper)

# Create a dummy interval for timestamp (start = end)
geo_na_mmsi_vessels[, timestamp_end := timestamp]

# Set keys for query table
setkey(geo_na_mmsi_vessels, timestamp, timestamp_end)

# Run non-equi join using foverlaps
matched_df <- foverlaps(
  geo_na_mmsi_vessels,
  complete,
  by.x = c("timestamp", "timestamp_end"),
  by.y = c("entry_lower", "entry_upper"),
  type = "any",
  nomatch = 0L
)


print(matched_df)


#Now that I have created a matched df to a one hour time slot, I would like to check how accurate the matching process was based on timestamp.


cat("Total observations in industrial_vessels_economic_zone:", nrow(industrial_vessels_economic_zone), "\n")
cat("Total observations in complete:", nrow(complete), "\n")
cat("Total matched observations:", nrow(matched_df), "\n")
cat("Percentage of complete entries matched:", (nrow(matched_df) / nrow(complete)) * 100, "%\n")



#I want to check how close the matches are by calculating the time difference. This will help to refine the window for matching. 

matched_df[, time_diff := abs(timestamp - entry_timestamp)]

summary(matched_df$time_diff)
hist(as.numeric(matched_df$time_diff), breaks = 20, main = "Distribution of Time Differences",
     xlab = "Time Difference (Seconds)", col = "lightblue", border = "black")



#After looking at this visual, I would like to reduce the time_window to 5 minutes. 

# Ensure timestamps are in POSIXct format
vessels_in_eez[, timestamp := as.POSIXct(timestamp)]
complete[, entry_timestamp := as.POSIXct(entry_timestamp)]

# Define time window (e.g., 5 minutes)
time_window <- 5 * 60  # 30 minutes in seconds

# Filter only rows where MMSI is NA
geo_na_mmsi_vessels <- vessels_in_eez[is.na(mmsi)]

# Precompute time range in 'complete'
complete[, entry_lower := entry_timestamp - time_window]
complete[, entry_upper := entry_timestamp + time_window]

# Perform non-equi join ONLY on filtered data
matched_df_rev <- geo_na_mmsi_vessels[complete, 
                  on = .(timestamp >= entry_lower, 
                         timestamp <= entry_upper), 
                  nomatch = 0L]

print(matched_df_rev)


#I will compare the old to the new dataframe after the matching methodology.

cat("Old matched observations:", nrow(matched_df), "\n")
cat("New matched observations:", nrow(matched_df_rev), "\n")
cat("Percentage decrease:", ((nrow(matched_df) - nrow(matched_df_rev)) / nrow(matched_df)) * 100, "%\n")

# Run summary count of how often each combination from the complete dataset was matched to a 
# vessel with missing MMSI in industrial_vessels_economic_zone
matched_df_rev[, .N, by = .(MMSI, vessel_name, flag)][order(-N)]

# After the mathcing, assign a match confidence based on frequency 
# Step 1: Count number of matches per vessel
match_summary <-matched_df_rev[, .N, by = .(MMSI, vessel_name, flag)][order(-N)]

# Step 2: Add confidence score directly
match_summary[, confidence := ifelse(N > 50, "high",
                                     ifelse(N >= 20, "medium", "low"))]

# Step 3: Display results
print(match_summary)

# Optional: View high-confidence matches
match_summary[confidence == "high"]

## Create visuals for presentation, pie chart between peru, foreign and missing NA in MMSI 
# Load libraries
library(ggplot2)
library(dplyr)

# Categorize vessels
vessel_identity <- vessels_in_eez %>%
  mutate(
    identity = case_when(
      is.na(mmsi) ~ "Unknown MMSI",
      grepl("^760", as.character(mmsi)) ~ "Peruvian Vessels",
      TRUE ~ "Foreign Vessels"
    )
  ) %>%
  group_by(identity) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count / sum(count) * 100, 1),
         label = paste0(identity, ": ", percentage, "%"))

# Load additional library
library(ggrepel)

# Create pie chart with non-overlapping labels
ggplot(vessel_identity, aes(x = "", y = count, fill = identity)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  geom_label_repel(aes(label = label), 
                   position = position_stack(vjust = 0.5),
                   box.padding = 0.5,
                   show.legend = FALSE,
                   segment.color = "grey50") +
  labs(title = "Vessel Identity Distribution in Peru’s EEZ (2017–2021)") +
  scale_fill_brewer(palette = "Pastel1")

### Try to expand the exclusive economic zone by 5km, 10km, 15km 
# Expansion sizes (in degrees)
km_to_deg <- function(km) { km / 111 }  # Rough conversion
expand_5km  <- km_to_deg(5)   # ~0.045 degrees
expand_10km <- km_to_deg(10)  # ~0.090 degrees
expand_15km <- km_to_deg(15)  # ~0.135 degrees

# Original bounding box
lat_min <- -20.2081
lat_max <- -3.3921
lon_min <- -84.6707
lon_max <- -70.3796

# 5km expanded box
industrial_vessels_box_5km <- industrial_vessels %>% 
  filter(lat >= (lat_min - expand_5km) & lat <= (lat_max + expand_5km) &
           lon >= (lon_min - expand_5km) & lon <= (lon_max + expand_5km))

# 10km expanded box
industrial_vessels_box_10km <- industrial_vessels %>% 
  filter(lat >= (lat_min - expand_10km) & lat <= (lat_max + expand_10km) &
           lon >= (lon_min - expand_10km) & lon <= (lon_max + expand_10km))

# 15km expanded box
industrial_vessels_box_15km <- industrial_vessels %>% 
  filter(lat >= (lat_min - expand_15km) & lat <= (lat_max + expand_15km) &
           lon >= (lon_min - expand_15km) & lon <= (lon_max + expand_15km))

cat("Original box:", nrow(industrial_vessels_box), "\n")
cat("Expanded 5km:", nrow(industrial_vessels_box_5km), "\n")
cat("Expanded 10km:", nrow(industrial_vessels_box_10km), "\n")
cat("Expanded 15km:", nrow(industrial_vessels_box_15km), "\n")

## Find the different ratios of what type of vessels 
# Define a function to categorize vessels
categorize_vessels <- function(data) {
  data %>%
    mutate(category = case_when(
      is.na(mmsi) ~ "Missing_MMSI",                         # No MMSI = missing/dark vessel
      grepl("^760", as.character(mmsi)) ~ "Peruvian_Vessel", # MMSI starts with 760 = Peru
      TRUE ~ "Foreign_Vessel"                                # Everything else = foreign
    ))
}

# Apply it to each box
industrial_vessels_box_5km_categorized <- categorize_vessels(industrial_vessels_box_5km)
industrial_vessels_box_10km_categorized <- categorize_vessels(industrial_vessels_box_10km)
industrial_vessels_box_15km_categorized <- categorize_vessels(industrial_vessels_box_15km)

# Count for 5km
cat("\nCounts for 5km expanded box:\n")
industrial_vessels_box_5km_categorized %>%
  count(category)

# Count for 10km
cat("\nCounts for 10km expanded box:\n")
industrial_vessels_box_10km_categorized %>%
  count(category)

# Count for 15km
cat("\nCounts for 15km expanded box:\n")
industrial_vessels_box_15km_categorized %>%
  count(category)

### Get the visualization 

library(sf)

# Step 1: Set original bounding box limits
lat_min <- -20.2081
lat_max <- -3.3921
lon_min <- -84.6707
lon_max <- -70.3796

# Step 2: Expand distances
km_to_deg <- function(km) { km / 111 }
expand_5km  <- km_to_deg(5)
expand_10km <- km_to_deg(10)
expand_15km <- km_to_deg(15)

# Step 3: Create sf polygons manually
create_box_sf <- function(lat_min, lat_max, lon_min, lon_max) {
  st_polygon(list(matrix(
    c(
      lon_min, lat_min,
      lon_min, lat_max,
      lon_max, lat_max,
      lon_max, lat_min,
      lon_min, lat_min
    ),
    ncol = 2, byrow = TRUE
  ))) %>%
    st_sfc(crs = 4326) %>%
    st_sf()
}

# Create each box
box_original <- create_box_sf(lat_min, lat_max, lon_min, lon_max)

box_5km <- create_box_sf(lat_min - expand_5km, lat_max + expand_5km,
                         lon_min - expand_5km, lon_max + expand_5km)

box_10km <- create_box_sf(lat_min - expand_10km, lat_max + expand_10km,
                          lon_min - expand_10km, lon_max + expand_10km)

box_15km <- create_box_sf(lat_min - expand_15km, lat_max + expand_15km,
                          lon_min - expand_15km, lon_max + expand_15km)

# Combine for plotting
boxes_combined <- rbind(
  st_sf(geometry = box_original$geometry, type = "Original"),
  st_sf(geometry = box_5km$geometry, type = "5km Expansion"),
  st_sf(geometry = box_10km$geometry, type = "10km Expansion"),
  st_sf(geometry = box_15km$geometry, type = "15km Expansion")
)

# Plot
ggplot() +
  geom_sf(data = boxes_combined, aes(color = type, linetype = type), fill = NA, size = 1) +
  geom_sf(data = my_geojson, fill = NA, color = "black", size = 0.8) +  # Peru EEZ boundary
  scale_color_manual(values = c(
    "Original" = "blue",
    "5km Expansion" = "green",
    "10km Expansion" = "orange",
    "15km Expansion" = "red"
  )) +
  scale_linetype_manual(values = c(
    "Original" = "solid",
    "5km Expansion" = "dotted",
    "10km Expansion" = "dotdash",
    "15km Expansion" = "dashed"
  )) +
  theme_minimal() +
  labs(title = "Expansion of Bounding Boxes Around Peru EEZ",
       subtitle = "Original boundary plus 5km, 10km, and 15km expansions",
       x = "Longitude", y = "Latitude",
       color = "Boundary Type",
       linetype = "Boundary Type") +
  coord_sf(xlim = c(-85, -70), ylim = c(-21, -2))

###########################################
