library(PerformanceAnalytics)
library(tidyverse)
library(readxl)
library(readr)
library(ggplot2)
library(scales)
library(corrplot)
library(glmnet)
library(sandwich)
options(warn=-1)
library(tseries)
library(lmtest)
library(stargazer)
library(DataCombine)
library(ivreg)
library(forecast)
library("margins")
library(cancensus)
library(rjson)
library(jsonlite)
library(httr)
library(stringr)

options(cancensus.api_key='CensusMapper_92d778fbc9cacfa147fe91e0e2f9d66e')
options(cancensus.cache_path ="cansensus_cache")

output_folder_11 <- "census_data11"

# Choose the city or cities you would like 
cities <- c("Oakville")
level <- "CSD"

### Year 2011 ###
vectors_2011 = c("v_CA11F_101", "v_CA11F_104", "v_CA11F_107", "v_CA11F_211", "v_CA11F_212", 
                 "v_CA11F_213", "v_CA11F_200", "v_CA11F_202", "v_CA11N_19", "v_CA11N_22", 
                 "v_CA11N_46", "v_CA11N_70", "v_CA11N_79", "v_CA11N_262", "v_CA11N_505", 
                 "v_CA11N_517", "v_CA11N_547", "v_CA11N_760", "v_CA11N_820", "v_CA11N_1084", 
                 "v_CA11N_1162", "v_CA11N_1204", "v_CA11N_1258", "v_CA11N_1264", "v_CA11N_1774", 
                 "v_CA11N_1783", "v_CA11N_1786", "v_CA11N_1789", "v_CA11N_1792", "v_CA11N_1804", 
                 "v_CA11N_1813", "v_CA11N_1816", "v_CA11N_1819", "v_CA11N_1822", "v_CA11N_1993", 
                 "v_CA11N_1996", "v_CA11N_1999", "v_CA11N_2035", "v_CA11N_2041", "v_CA11N_2044", 
                 "v_CA11N_2047", "v_CA11N_2050", "v_CA11N_2053", "v_CA11N_2056", "v_CA11N_2059", 
                 "v_CA11N_2062", "v_CA11N_2200", "v_CA11N_2203", "v_CA11N_2206", "v_CA11N_2282", 
                 "v_CA11N_2284", "v_CA11N_2285", "v_CA11N_2286", "v_CA11N_2287", "v_CA11N_2289", 
                 "v_CA11N_2291", "v_CA11N_2292", "v_CA11N_2302", "v_CA11N_2305", "v_CA11N_2308", 
                 "v_CA11N_2311", "v_CA11N_2314", "v_CA11N_2317", "v_CA11N_2320", "v_CA11N_2323", 
                 "v_CA11N_2332", "v_CA11N_2356", "v_CA11N_2359", "v_CA11N_2362", "v_CA11N_2365", 
                 "v_CA11N_2368", "v_CA11N_2371", "v_CA11N_2374", "v_CA11N_2377", "v_CA11N_2386", 
                 "v_CA11N_2534", "v_CA11N_2535", "v_CA11N_2536", "v_CA11N_2537", "v_CA11N_2538", 
                 "v_CA11N_2539", "v_CA11N_2540", "v_CA11N_2541", "v_CA11N_2548", "v_CA11N_2549", 
                 "v_CA11N_2550", "v_CA11N_2551", "v_CA11N_2552", "v_CA11N_2553", "v_CA11N_2554", 
                 "v_CA11N_2555", "v_CA11N_2558")

regions_2011 <- list_census_regions("CA11") # Get all census regions
csd_regions_2011 <- regions_2011[regions_2011$level == level & regions_2011$name %in% cities, ]
print(csd_regions_11)

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

regions_2016 <- list_census_regions("CA16") # Get all census regions
csd_regions_2016 <- regions_2016[regions_2016$level == level & regions_2016$name %in% cities, ]
print(csd_regions_16)

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
vectors_2021 = c("v_CA21_454","v_CA21_463","v_CA21_466","v_CA21_469","v_CA21_472","v_CA21_475","v_CA21_2207","v_CA21_2209","v_CA21_2208", "v_CA21_408","v_CA21_409",
            "v_CA21_410","v_CA21_411","v_CA21_417","v_CA21_479","v_CA21_480","v_CA21_481","v_CA21_482","v_CA21_483",
            "v_CA21_2309","v_CA21_2312","v_CA21_2315","v_CA21_2318","v_CA21_2321",
            "v_CA21_2324","v_CA21_2327","v_CA21_2330","v_CA21_2333","v_CA21_2336","v_CA21_2339",
            "v_CA21_2342","v_CA21_2570","v_CA21_2540","v_CA21_5795","v_CA21_5798",
            "v_CA21_5801","v_CA21_5804","v_CA21_5807","v_CA21_5810","v_CA21_5816","v_CA21_5819",
            "v_CA21_5822","v_CA21_5825","v_CA21_5828",
            "v_CA21_2427","v_CA21_2428","v_CA21_2429","v_CA21_2430","v_CA21_2431","v_CA21_2432","v_CA21_2433","v_CA21_2434","v_CA21_2435","v_CA21_2436",
            "v_CA21_2437","v_CA21_2438","v_CA21_2439","v_CA21_2440","v_CA21_2441","v_CA21_2442",
            "v_CA21_4893","v_CA21_4894","v_CA21_4895","v_CA21_4896","v_CA21_4891","v_CA21_4892",
            "v_CA21_4897","v_CA21_4898","v_CA21_4898","v_CA21_4899","v_CA21_4900","v_CA21_4901",
            "v_CA21_5099","v_CA21_5102","v_CA21_5108","v_CA21_5117","v_CA21_5120","v_CA21_5123",
            "v_CA21_5603","v_CA21_5606","v_CA21_5609","v_CA21_5663","v_CA21_5669","v_CA21_5672","v_CA21_5675",
            "v_CA21_5678","v_CA21_5681","v_CA21_5684","v_CA21_5687","v_CA21_5690",
            "v_CA21_4002","v_CA21_4806","v_CA21_4014","v_CA21_4044","v_CA21_4266","v_CA21_4329","v_CA21_4611","v_CA21_4698",
            "v_CA21_4743","v_CA21_4800","v_CA21_3408","v_CA21_3411","v_CA21_3435")

regions_2021 <- list_census_regions("CA21") # Get all census regions
csd_regions_2021 <- regions_2021[regions_2021$level == level & regions_2021$name %in% cities, ]
print(csd_regions_21)

for (i in 1:nrow(csd_regions_2021)) {
  csd_id_21 <- csd_regions_2021$region[i]
  csd_name_21 <- csd_regions_2021$name[i]
  data_21 <- get_census(
    dataset = "CA21",
    regions = list(CSD = csd_id_21),
    vectors = vectors_2021, 
    level = "DA"
  )  
  # Save CSV to the specified folder
  write.csv(data_21, file = file.path(output_folder_21, paste0(csd_name_21, "_21.csv")))
}
