library(tidyverse)
library(readr)
library(cancensus)
library(rjson)
library(jsonlite)
library(httr)
library(stringr)


api_key <- Sys.getenv("CANCENSUS_API_KEY")
options(cancensus.api_key=api_key)
options(cancensus.cache_path ="cansensus_cache")

output_folder_11 <- "census_data11"
output_folder_16 <- "census_data16"
output_folder_21 <- "census_data21"

# Choose the city or cities you would like 
cities <- c("Oakville")
level <- "CSD"

# OR filter data for top 50 pop cities
regions_11 <- list_census_regions("CA11")# Get all census regions
csd_regions_11 <- regions_11[regions_11$level == "CSD", ]  # Filter for Census Subdivisions (CSDs)
csd_regions_11 <- csd_regions_11 %>%
  arrange(desc(pop)) %>%  # Sort by population in descending order
  head(60)  # Select the top 50

# loop through the cities in the top 50 list
for (i in 1:nrow(csd_regions_11)) {
  csd_regions_11$name <- gsub("[^A-Za-z0-9_À-ÿ\\-]", "_", csd_regions_11$name) # Replace special characters with underscore
}

### Year 2011 ###
vectors_2011 = c("v_CA11F_199", "v_CA11F_200", "v_CA11F_201", "v_CA11F_202", "v_CA11F_203",
                 "v_CA11F_86", "v_CA11F_95", "v_CA11F_98", "v_CA11F_101", "v_CA11F_104", "v_CA11F_107", "v_CA11F_211",
                 "v_CA11F_212", "v_CA11F_213", "v_CA11F_214", "v_CA11F_215", "v_CA11F_163",
                 "v_CA11N_2449", "v_CA11N_2450", "v_CA11N_2451", "v_CA11N_2356", "v_CA11N_2359",
                 "v_CA11N_2362", "v_CA11N_2365", "v_CA11N_2368", "v_CA11N_2371", "v_CA11N_2374",
                 "v_CA11N_2377", "v_CA11N_2380", "v_CA11N_2383", "v_CA11N_2386", "v_CA11N_2548",
                 "v_CA11N_2549", "v_CA11N_2550", "v_CA11N_2551", "v_CA11N_2552", "v_CA11N_2553",
                 "v_CA11N_2554", "v_CA11N_2555", "v_CA11N_2556", "v_CA11N_2557", "v_CA11N_2558",
                 "v_CA11N_2559", "v_CA11N_2560", "v_CA11N_2606", "v_CA11N_19", "v_CA11N_22",
                 "v_CA11N_46", "v_CA11N_505", "v_CA11N_517", "v_CA11N_547", "v_CA11N_760",
                 "v_CA11N_820", "v_CA11N_1084", "v_CA11N_1162", "v_CA11N_1204", "v_CA11N_1258",
                 "v_CA11N_1264", "v_CA11N_2282", "v_CA11N_2283", "v_CA11N_2284", "v_CA11N_2285",
                 "v_CA11N_2286", "v_CA11N_2287", "v_CA11N_2288", "v_CA11N_2289", "v_CA11N_2290",
                 "v_CA11N_2291", "v_CA11N_2292", "v_CA11N_1804","v_CA11N_1807", "v_CA11N_1813", "v_CA11N_1816",
                 "v_CA11N_1819", "v_CA11N_1822", "v_CA11N_1993", "v_CA11N_1996", "v_CA11N_1999",
                 "v_CA11N_2035", "v_CA11N_2041", "v_CA11N_2044", "v_CA11N_2047", "v_CA11N_2050",
                 "v_CA11N_2053", "v_CA11N_2056", "v_CA11N_2059", "v_CA11N_2062", "v_CA11N_2194",
                 "v_CA11N_2197", "v_CA11N_2200", "v_CA11N_2203", "v_CA11N_2206", "v_CA11N_2209")

vectors_df_2011 <- list_census_vectors("CA11")

vectors_df_2011 <- vectors_df_2011[vectors_df_2011$vector %in% vectors_2011,]

# Reorder according to vectors_2011
vectors_df_2011 <- vectors_df_2011[match(vectors_2011, vectors_df_2011$vector), ]

regions_2011 <- list_census_regions("CA11") # Get all census regions
for (i in 1:nrow(regions_2011)) {
  regions_2011$name <- gsub("[^A-Za-z0-9_À-ÿ\\-]", "_", regions_2011$name) # Replace special characters with underscore
}
csd_regions_2011 <- regions_2011[regions_2011$level == level & regions_2011$name %in% csd_regions_11$name, ]
print(csd_regions_2011)


for (i in 1:nrow(csd_regions_2011)) {
  csd_id_11 <- csd_regions_2011$region[i]
  csd_name_11 <- csd_regions_2011$name[i]
  data_11 <- get_census(
    dataset = "CA11",
    regions = list(CSD = csd_id_11),
    vectors = vectors_2011, 
    level = "DA"
  )  
  # Save CSV to the specified folder
  write.csv(data_11, file = file.path(output_folder_11, paste0(csd_name_11, "_11.csv")))
}

### Year 2016 ###
vectors_2016 = c("v_CA16_454","v_CA16_463","v_CA16_466","v_CA16_469","v_CA16_472","v_CA16_475","v_CA16_2207","v_CA16_2209","v_CA16_2208", "v_CA16_408","v_CA16_409",
            "v_CA16_410","v_CA16_411","v_CA16_417","v_CA16_479","v_CA16_480","v_CA16_481","v_CA16_482","v_CA16_483",
            "v_CA16_2309","v_CA16_2312","v_CA16_2315","v_CA16_2318","v_CA16_2321",
            "v_CA16_2324","v_CA16_2327","v_CA16_2330","v_CA16_2333","v_CA16_2336","v_CA16_2339",
            "v_CA16_2342","v_CA16_2570","v_CA16_2540","v_CA16_5795","v_CA16_5798",
            "v_CA16_5801","v_CA16_5804","v_CA16_5807","v_CA16_5810","v_CA16_5816","v_CA16_5819",
            "v_CA16_5822","v_CA16_5825","v_CA16_5828",
            "v_CA16_2427","v_CA16_2428","v_CA16_2429","v_CA16_2430","v_CA16_2431","v_CA16_2432","v_CA16_2433","v_CA16_2434","v_CA16_2435","v_CA16_2436",
            "v_CA16_2437","v_CA16_2438","v_CA16_2439","v_CA16_2440","v_CA16_2441","v_CA16_2442",
            "v_CA16_4893","v_CA16_4894","v_CA16_4895","v_CA16_4896","v_CA16_4891","v_CA16_4892",
            "v_CA16_4897","v_CA16_4898","v_CA16_4898","v_CA16_4899","v_CA16_4900","v_CA16_4901",
            "v_CA16_5099","v_CA16_5102","v_CA16_5108","v_CA16_5117","v_CA16_5120","v_CA16_5123",
            "v_CA16_5603","v_CA16_5606","v_CA16_5609","v_CA16_5663","v_CA16_5669","v_CA16_5672","v_CA16_5675",
            "v_CA16_5678","v_CA16_5681","v_CA16_5684","v_CA16_5687","v_CA16_5690",
            "v_CA16_4002","v_CA16_4806","v_CA16_4014","v_CA16_4044","v_CA16_4266","v_CA16_4329","v_CA16_4611","v_CA16_4698",
            "v_CA16_4743","v_CA16_4800","v_CA16_3408","v_CA16_3411","v_CA16_3435")

vectors_df_2016 <- list_census_vectors("CA16")

vectors_df_2016 <- vectors_df_2016[vectors_df_2016$vector %in% vectors_2016,]

regions_2016 <- list_census_regions("CA16") # Get all census regions
for (i in 1:nrow(regions_2016)) {
  regions_2016$name <- gsub("[^A-Za-z0-9_À-ÿ\\-]", "_", regions_2016$name) # Replace special characters with underscore
}
csd_regions_2016 <- regions_2016[regions_2016$level == level & regions_2016$name %in% csd_regions_11$name, ]
print(csd_regions_2016)

for (i in 1:nrow(csd_regions_2016)) {
  csd_id_16 <- csd_regions_2016$region[i]
  csd_name_16 <- csd_regions_2016$name[i]
  data_16 <- get_census(
    dataset = "CA16",
    regions = list(CSD = csd_id_16),
    vectors = vectors_2016, 
    level = "DA"
  )  
  # Save CSV to the specified folder
  write.csv(data_16, file = file.path(output_folder_16, paste0(csd_name_16, "_16.csv")))
}


### Year 2021 ###
vectors_2021 = c("v_CA21_434", "v_CA21_435", "v_CA21_440", "v_CA21_441", "v_CA21_442",
                 "v_CA21_456", "v_CA21_477", "v_CA21_480", "v_CA21_483", "v_CA21_486",
                 "v_CA21_489", "v_CA21_493", "v_CA21_494", "v_CA21_495", "v_CA21_496",
                 "v_CA21_497", "v_CA21_560", "v_CA21_561", "v_CA21_562", "v_CA21_722",
                 "v_CA21_725", "v_CA21_728", "v_CA21_731", "v_CA21_734", "v_CA21_737",
                 "v_CA21_740", "v_CA21_740", "v_CA21_743", "v_CA21_746", "v_CA21_749",
                 "v_CA21_752", "v_CA21_945", "v_CA21_946", "v_CA21_947", "v_CA21_948",
                 "v_CA21_949", "v_CA21_950", "v_CA21_951", "v_CA21_952", "v_CA21_953",
                 "v_CA21_954", "v_CA21_955", "v_CA21_956", "v_CA21_957", "v_CA21_958", 
                 "v_CA21_959", "v_CA21_960", "v_CA21_1040", "v_CA21_1085", "v_CA21_4407",
                 "v_CA21_4410", "v_CA21_4434", "v_CA21_4971", "v_CA21_4974", "v_CA21_4977",
                 "v_CA21_5208", "v_CA21_5109", "v_CA21_5205", "v_CA21_5094", "v_CA21_5202",
                 "v_CA21_4809", "v_CA21_4306", "v_CA21_4307", "v_CA21_4309", "v_CA21_4310",
                 "v_CA21_4311", "v_CA21_4312", "v_CA21_4313", "v_CA21_4314", "v_CA21_4315", 
                 "v_CA21_4317", "v_CA21_4318", "v_CA21_5820", "v_CA21_5832", "v_CA21_5841", 
                 "v_CA21_5844", "v_CA21_5868", "v_CA21_5880", "v_CA21_5889", "v_CA21_5892",
                 "v_CA21_6498", "v_CA21_6501", "v_CA21_6504", "v_CA21_6570", "v_CA21_6576",
                 "v_CA21_6579", "v_CA21_6582", "v_CA21_6585", "v_CA21_6588", "v_CA21_6591",
                 "v_CA21_6594", "v_CA21_6597", "v_CA21_7638", "v_CA21_7641", "v_CA21_7644", 
                 "v_CA21_7647", "v_CA21_7650", "v_CA21_7653", "v_CA21_7659", "v_CA21_7662", 
                 "v_CA21_7665", "v_CA21_7668", "v_CA21_7671")

vectors_df_2021 <- list_census_vectors("CA21")

vectors_df_2021 <- vectors_df_2021[vectors_df_2021$vector %in% vectors_2021,]

regions_2021 <- list_census_regions("CA21") # Get all census regions
for (i in 1:nrow(regions_2021)) {
  regions_2021$name <- gsub("[^A-Za-z0-9_À-ÿ\\-]", "_", regions_2021$name) # Replace special characters with underscore
}
csd_regions_2021 <- regions_2021[regions_2021$level == level & regions_2021$name %in% csd_regions_11$name, ]
print(csd_regions_2021)



for (i in 1:nrow(csd_regions_2021)) {
  csd_id_21 <- csd_regions_2021$region[i]
  csd_name_21 <- csd_regions_2021$name[i]
  print(csd_name_21)
  data_21 <- get_census(
    dataset = "CA21",
    regions = list(CSD = csd_id_21),
    vectors = vectors_2021, 
    level = "DA"
  )  
  # Save CSV to the specified folder
  write.csv(data_21, file = file.path(output_folder_21, paste0(csd_name_21, "_21.csv")))
}

### Merge all the data into one file ###

# Give Vectors df a id column in the first column
vectors_df_2011$id <- 1:nrow(vectors_df_2011)
vectors_df_2016$id <- 1:nrow(vectors_df_2016)
vectors_df_2021$id <- 1:nrow(vectors_df_2021)

# Merge all the vectors data into one file by joining on the id column
vectors_df <- merge(vectors_df_2011, vectors_df_2016, by = "id", all = TRUE)
vectors_df <- merge(vectors_df, vectors_df_2021, by = "id", all = TRUE)

# Save the merged data to a CSV file
write.csv(vectors_df, file = "vectors_list.csv")


### AIR TEMP WORK ###

# Load required libraries
library(dplyr)
library(stringr)
library(cancensus)
library(jsonlite)

airtemp_output_folder_21 <- "airtemp_census_data21"

# ---- Load and prepare data ----

# Load the JSON file
json_data <- read_json("methoData.json", simplifyVector = TRUE)

# Convert air temperature data to a data frame
airtemps_df <- as.data.frame(json_data$airTemperatures)

# Let's assume your hourly air temperature data is stored in a data frame called airtemps_df
# And it has columns like: datetime, date, airTemperature, metro

# Convert airTemperature to numeric just in case
airtemps_df$airTemperature <- as.numeric(airtemps_df$airTemperature)

# Group by date and metro, and calculate the daily average temperature
daily_avg_temps <- airtemps_df %>%
  group_by(date, metro) %>%
  summarise(
    avg_air_temp = mean(airTemperature, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

# View the first few rows
head(daily_avg_temps)


# 1. Filter regions_2021 to keep only CSD-level regions
csd_regions <- regions_2021 %>%
  filter(level == "CSD")

# 2. Clean the metro names from daily_avg_temps and region names for comparison
clean_names <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("\\s*\\([^\\)]+\\)", "") %>%  # Remove content in parentheses
    str_replace_all("_", " ") %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_squish()
}

# Clean the metro and region names
daily_avg_temps <- daily_avg_temps %>%
  mutate(clean_metro = clean_names(metro))

csd_regions <- csd_regions %>%
  mutate(clean_name = clean_names(name))

# 3. Filter regions_2021 where the cleaned name is in the cleaned metro list
matched_regions <- daily_avg_temps %>%
  filter(clean_metro %in% csd_regions$clean_name)

# View result
head(matched_regions)

matched_regions <- matched_regions %>%
  filter(date >= "2021-01-01")

# Filter duplicate metro
matched_regions <- matched_regions %>%
  distinct(clean_metro, .keep_all = TRUE)


# 4. Merge the matched regions with the CSD-level census data
merged_data <- matched_regions %>%
  left_join(csd_regions, by = c("clean_metro" = "clean_name"))

# Filter duplicate metro
merged_data <- merged_data %>%
  distinct(clean_metro, .keep_all = TRUE)


### Airtemps Year 2021 ###
airtemps_csd_regions_2021 <- csd_regions_2021 %>%
  mutate(clean_name = clean_names(name)) %>%
  filter(clean_name %in% merged_data$clean_metro)

for (i in 1:nrow(airtemps_csd_regions_2021)) {
  csd_id_21 <- airtemps_csd_regions_2021$region[i]
  csd_name_21 <- airtemps_csd_regions_2021$name[i]
  print(airtemps_csd_regions_2021)
  airtemp_data_21 <- get_census(
    dataset = "CA21",
    regions = list(CSD = csd_id_21),
    vectors = vectors_2021, 
    level = "CSD"
  )  
  #merge with airtemps
  airtemp_data_21 <- airtemp_data_21 %>%
    mutate(clean_name = clean_names(`Region Name`)) %>%
    left_join(matched_regions, by = c("clean_name" = "clean_metro"))
  
  airtemp_data_21 <- airtemp_data_21 %>%
    select(-clean_name, -metro, -n_obs)
  
  # Save CSV to the specified folder
  write.csv(airtemp_data_21, file = file.path(airtemp_output_folder_21, paste0(csd_name_21, "_21.csv")))
}










