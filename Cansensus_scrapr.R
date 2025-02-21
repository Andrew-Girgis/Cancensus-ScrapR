
# setwd("C:/Users/profs/OneDrive/Documents/anindya")
library(PerformanceAnalytics)
library(tidyverse)
library(readxl)
library(readr)
#library(dbplyr)
library(ggplot2)

library(scales)
#install.packages("corrplot")
library(corrplot)
##install.packages("orcutt")
#library(orcutt)
##install.packages("glmnet")
library(glmnet)
#install.packages("Rtools")
#library(Rtools)
##install.packages("sandwich")
library(sandwich)

##install.packages("tseries")
options(warn=-1)
library(tseries)
##install.packages("lmtest")
library(lmtest)

##install.packages("stargazer")
library(stargazer)

#install.packages("DataCombine")
library(DataCombine)

#install.packages("ivreg")
library(ivreg)

##install.packages("forecast")
library(forecast)

#install.packages("margins")
library("margins")

#install.packages("cancensus")

library(cancensus)

#install.packages("rjson")
library(rjson)

# set_cache_path("C:/Users/profs/OneDrive/Documents/anindya", install = TRUE)


options(cancensus.api_key='CensusMapper_92d778fbc9cacfa147fe91e0e2f9d66e')
options(cancensus.cache_path ="cansensus_cache")


# To view available Census datasets
print(list_census_datasets())

explore_census_regions(dataset = "CA16")

#census_data <- get_census(dataset='CA16', regions=list(CMA="59933"), 
#                         vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='DA')

#allows me to find variables by geography
list_census_regions('CA16') %>% 
  filter(level == "CSD", name %in% c("Oakville"))

#downloading data for Oakville

Oakdata1 <- get_census(dataset='CA16', regions=list(CSD="3524001"), 
                       vectors=c("v_CA21_1","v_CA21_4","v_CA21_7"), level='DA')

Oakdata1 <- get_census(dataset='CA16', regions=list(CSD="3524001"), 
                       vectors=c("v_CA16_7","v_CA16_25","v_CA16_43","v_CA16_64","v_CA16_82","v_CA16_100","v_CA16_118","v_CA16_136", "v_CA16_154","v_CA16_172",
                                 "v_CA16_190","v_CA16_208","v_CA16_226","v_CA16_247","v_CA16_265","v_CA16_283","v_CA16_301","v_CA16_319","v_CA16_2207",
                                 "v_CA16_401","v_CA16_3957",
                                 "v_CA16_379","v_CA16_4986","v_CA16_2398","v_CA16_2213","v_CA16_2208","v_CA16_2209"), level='DA')


#downloading data for Stratford
censusstrat16_data <- get_census(dataset='CA16', regions=list(CSD="3531011"), 
                                 vectors=c("v_CA16_408","v_CA16_409","v_CA16_410","v_CA16_2207"), level='DA')
write.csv(censusstrat16_data,"censusdata16.csv")

censusstrat21_data <- get_census(dataset='CA21', regions=list(CSD="3531011"), vectors=c("v_CA21_818"), level='DA')
write.csv(censusstrat21_data,"censusdata21.csv")


#allows me to find variables by geography
list_census_regions('CA16') %>% 
  filter(level == "CSD", name %in% c("Oakville"))

census_data1 <- get_census(dataset='CA16', regions=list(CSD="3524001"), 
                           vectors=c("v_CA16_401","v_CA16_409","v_CA16_410","v_CA16_3954","v_CA16_3957"), level='DA')

write.csv(census_data1,"census_data1.csv")
#v_CA16_401: Population, 2016 v_CA16_409: Single-detached house   v_CA16_410: Apartment in a building that has five or more storeys v_CA16_3954: Total - Visible minority for the population in private households - 25% sample data      v_CA16_3957: Total visible minority population


census_data2 <- get_census(dataset='CA16', regions=list(CSD="3524001"), 
                           vectors=c("v_CA16_401","v_CA16_379","v_CA16_4986","v_CA16_2398","v_CA16_2213"), level='DA')

#v_CA16_379: Average age      v_CA16_4986: Average after-tax income of households in 2015 ($)   
#v_CA16_2398: Median after-tax income of households in 2015 ($)
#v_CA16_379: Average age
#v_CA16_401: Population, 2016

#v_CA16_4986: Average after-tax income of households in 2015 ($)  
#v_CA16_2213: Median after-tax income in 2015 among recipients ($)



write.csv(census_data2,"census_data2a.csv")

census_data3 <- get_census(dataset='CA21', regions=list(CSD="3524001"),vectors=c("v_CA21_1","v_CA21_2","v_CA21_386",
                                                                                 "v_CA21_907","v_CA21_560","v_CA21_818"), level='DA')
#v_CA21_1: Population, 2021   v_CA21_386: Average age v_CA21_907: Median after-tax income of household in 2020 ($) v_CA21_560: Median total income in 2020 among recipients ($)     v_CA21_818: Median total income in 2019 among recipients ($)


write.csv(census_data3, "census_data3.csv")

#now getting population


census_data4 <- get_census(dataset='CA21', regions=list(CSD="3524001"), 
                           vectors=c("v_CA21_14","v_CA21_32","v_CA21_50","v_CA21_71","v_CA21_89","v_CA21_107",
                                     "v_CA21_125","v_CA21_143", "v_CA21_161","v_CA21_179","v_CA21_197",
                                     "v_CA21_215","v_CA21_233","v_CA21_254","v_CA21_272","v_CA21_290","v_CA21_308","v_CA21_326","v_CA21_824","v_CA21_2207"), level='DA')

#v_CA21_14: 0 to 4 years      v_CA21_32: 5 to 9 years v_CA21_50: 10 to 14 years     v_CA21_71: 15 to 19 years     v_CA21_89: 20 to 24 years
#v_CA21_107: 25 to 29 years   v_CA21_125: 30 to 34 years    v_CA21_143: 35 to 39 years    v_CA21_161: 40 to 44 years    
#v_CA21_179: 45 to 49 years   v_CA21_197: 50 to 54 years    v_CA21_215: 55 to 59 years    v_CA21_233: 60 to 64 years    
#v_CA21_254: 65 to 69 years   v_CA21_272: 70 to 74 years    v_CA21_290: 75 to 79 years    v_CA21_308: 80 to 84 years    
#v_CA21_326: 85 years and over      v_CA21_824: Median after-tax income in 2019 among recipients ($)



write.csv(census_data4, "census_data4.csv")

census_data5 <- get_census(dataset='CA16', regions=list(CSD="3524001"), 
                           vectors=c("v_CA16_7","v_CA16_25","v_CA16_43","v_CA16_64","v_CA16_82","v_CA16_100","v_CA16_118","v_CA16_136", "v_CA16_154","v_CA16_172",
                                     "v_CA16_190","v_CA16_208","v_CA16_226","v_CA16_247","v_CA16_265","v_CA16_283","v_CA16_301","v_CA16_319","v_CA16_2213","v_CA16_2207"), level='DA')
#v_CA16_7: 0 to 4 years v_CA16_25: 5 to 9 years v_CA16_43: 10 to 14 years     v_CA16_64: 15 to 19 years     v_CA16_82: 
#20 to 24 years   v_CA16_100: 25 to 29 years    v_CA16_118: 30 to 34 years    v_CA16_136: 35 to 39 years    
#v_CA16_154: 40 to 44 years   v_CA16_172: 45 to 49 years    v_CA16_190: 50 to 54 years    v_CA16_208: 55 to 59 years    
#v_CA16_226: 60 to 64 years   v_CA16_247: 65 to 69 years    v_CA16_265: 70 to 74 years    v_CA16_283: 75 to 79 years
#v_CA16_301: 80 to 84 years   v_CA16_319: 85 years and over v_CA16_2213: Median after-tax income in 2015 among recipients ($)
#v_CA16_2207: Median total income in 2015 among recipients ($)


write.csv(census_data5, "census_data5.csv")

#CensusData <- read_csv("Census2016_subset.csv")
#summary(CensusData)

#now getting remaining data for Oakville. first creating 1 file for all variables I have 

Oakdata1 <- get_census(dataset='CA16', regions=list(CSD="3524001"), 
                       vectors=c("v_CA16_401","v_CA16_404","v_CA16_406","v_CA16_407","v_CA16_409","v_CA16_410","v_CA16_412","v_CA16_413","v_CA16_414","v_CA16_415","v_CA16_416","v_CA16_417",
                                 "v_CA16_7","v_CA16_25","v_CA16_43","v_CA16_64","v_CA16_82","v_CA16_100","v_CA16_118","v_CA16_136", "v_CA16_154","v_CA16_172",
                                 "v_CA16_190","v_CA16_208","v_CA16_226","v_CA16_244","v_CA16_2207","v_CA16_3957",
                                 "v_CA16_379","v_CA16_4986","v_CA16_2398","v_CA16_2213","v_CA16_2208","v_CA16_2209",
                                 "v_CA16_5666","v_CA16_3960","v_CA16_3963","v_CA16_3966","v_CA16_3969",
                                 "v_CA16_3972","v_CA16_3975","v_CA16_3978","v_CA16_3981","v_CA16_3984",
                                 "v_CA16_3987","v_CA16_3990","v_CA16_3993","v_CA16_3986",
                                 "v_CA16_5054","v_CA16_5057","v_CA16_5062","v_CA16_5072","v_CA16_5075"), level='DA')

#v_CA16_7: 0 to 4 years v_CA16_25: 5 to 9 years v_CA16_43: 10 to 14 years     v_CA16_64: 15 to 19 years     v_CA16_82: 
#20 to 24 years   v_CA16_100: 25 to 29 years    v_CA16_118: 30 to 34 years    v_CA16_136: 35 to 39 years    
#v_CA16_154: 40 to 44 years   v_CA16_172: 45 to 49 years    v_CA16_190: 50 to 54 years    v_CA16_208: 55 to 59 years    
#v_CA16_226: 60 to 64 years   v_CA16_247: 65 to 69 years    v_CA16_265: 70 to 74 years    v_CA16_283: 75 to 79 years
#v_CA16_301: 80 to 84 years   v_CA16_319: 85 years and over v_CA16_2213: Median after-tax income in 2015 among recipients ($)
#v_CA16_2207: Median total income in 2015 among recipients ($)
#v_CA16_401: Population, 2016 v_CA16_3957: Total visible minority population
#v_CA16_379: Average age      v_CA16_4986: Average after-tax income of households in 2015 ($)   
#v_CA16_2398: Median after-tax income of households in 2015 ($)
#v_CA16_4986: Average after-tax income of households in 2015 ($)  
#v_CA16_2213: Median after-tax income in 2015 among recipients ($)
#"v_CA16_2208","v_CA16_2209" median income among males and females

write.csv(Oakdata1, "Oakdata1.csv")

Oakdata2a <- get_census(dataset='CA16', regions=list(CSD="3524001"), 
                        vectors=c("v_CA16_454","v_CA16_463","v_CA16_466","v_CA16_469","v_CA16_472","v_CA16_475","v_CA16_2207","v_CA16_2209","v_CA16_2208", "v_CA16_408","v_CA16_409",
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
                                  "v_CA16_4743","v_CA16_4800","v_CA16_3408","v_CA16_3411","v_CA16_3435"),level='DA')


write.csv(Oakdata2a, "Oakdata2a.csv")

#Data from 21 census

Oakdata21 <- get_census(dataset='CA21', regions=list(CSD="3524001"), 
                        vectors=c("v_CA21_1","v_CA21_4","v_CA21_7","v_CA21_6","v_CA21_16", "v_CA21_34", "v_CA21_52"
                                  ,"v_CA21_73","v_CA21_91","v_CA21_109", "v_CA21_127", "v_CA21_145", "v_CA21_163", "v_CA21_181",
                                  "v_CA21_199","v_CA21_217", "v_CA21_235", "v_CA21_253", "v_CA21_258","v_CA21_388", "v_CA21_391",
                                  "v_CA21_434", "v_CA21_435", "v_CA21_436", "v_CA21_437", "v_CA21_438","v_CA21_439", "v_CA21_440",
                                  "v_CA21_441", "v_CA21_442", "v_CA21_459", "v_CA21_462", "v_CA21_480", "v_CA21_483", "v_CA21_486", 
                                  "v_CA21_489", "v_CA21_492", "v_CA21_493", "v_CA21_494", "v_CA21_495", "v_CA21_496", "v_CA21_497",
                                  "v_CA21_560", "v_CA21_674", "v_CA21_677", "v_CA21_680", "v_CA21_683", "v_CA21_686", "v_CA21_689", 
                                  "v_CA21_692", "v_CA21_695", "v_CA21_698", "v_CA21_701", "v_CA21_707", "v_CA21_710", "v_CA21_915",
                                  "v_CA21_916", "v_CA21_4407", "v_CA21_4410", "v_CA21_4434", "v_CA21_4878", "v_CA21_4881", "v_CA21_4884",
                                  "v_CA21_4887", "v_CA21_4890", "v_CA21_4893", "v_CA21_4896", "v_CA21_4899", "v_CA21_4902", "v_CA21_4905",
                                  "v_CA21_4908", "v_CA21_4911", "v_CA21_4914", "v_CA21_5868", "v_CA21_5871", "v_CA21_5898", "v_CA21_5901", 
                                  "v_CA21_5904", "v_CA21_5907", "v_CA21_5910", "v_CA21_6519", "v_CA21_6526", "v_CA21_6522", "v_CA21_6570", 
                                  "v_CA21_6573", "v_CA21_6576", "v_CA21_6579", "v_CA21_6582", "v_CA21_6585", "v_CA21_6588", "v_CA21_6591",
                                  "v_CA21_6594", "v_CA21_6597", "v_CA21_7635", "v_CA21_7644", "v_CA21_7647", "v_CA21_7650", "v_CA21_7653"),level='DA')

write.csv(Oakdata21, "Oakdata21.csv")

Oakdata22 <- get_census(dataset='CA21', regions=list(CSD="3524001"), 
                        vectors=c("v_CA21_1","v_CA21_4","v_CA21_7","v_CA21_6","v_CA21_16", "v_CA21_34", "v_CA21_52"
                                  ,"v_CA21_73","v_CA21_91","v_CA21_109", "v_CA21_127", "v_CA21_145", "v_CA21_163", "v_CA21_181",
                                  "v_CA21_199","v_CA21_217", "v_CA21_235", "v_CA21_253", "v_CA21_258","v_CA21_388", "v_CA21_391",
                                  "v_CA21_434", "v_CA21_435", "v_CA21_436", "v_CA21_437", "v_CA21_438","v_CA21_439", "v_CA21_440",
                                  "v_CA21_441", "v_CA21_442", "v_CA21_459", "v_CA21_462", "v_CA21_480", "v_CA21_483", "v_CA21_486", 
                                  "v_CA21_489", "v_CA21_492", "v_CA21_493", "v_CA21_494", "v_CA21_495", "v_CA21_496", "v_CA21_497"), level='DA')

write.csv(Oakdata22, "Oakdata22.csv")


Oakdata23 <- get_census(dataset='CA21', regions=list(CSD="3524001"), 
                        vectors=c("v_CA21_560", "v_CA21_674", "v_CA21_677", "v_CA21_680", "v_CA21_683", "v_CA21_686", "v_CA21_689", 
                                  "v_CA21_692", "v_CA21_695", "v_CA21_698", "v_CA21_701", "v_CA21_707", "v_CA21_710", "v_CA21_915",
                                  "v_CA21_916", "v_CA21_4407", "v_CA21_4410", "v_CA21_4434", "v_CA21_4878", "v_CA21_4881", "v_CA21_4884",
                                  "v_CA21_4887", "v_CA21_4890", "v_CA21_4893", "v_CA21_4896", "v_CA21_4899", "v_CA21_4902", "v_CA21_4905",
                                  "v_CA21_4908", "v_CA21_4911", "v_CA21_4914", "v_CA21_5868", "v_CA21_5871", "v_CA21_5898", "v_CA21_5901"),level='DA') 

write.csv(Oakdata23, "Oakdata23.csv")

Oakdata24 <- get_census(dataset='CA21', regions=list(CSD="3524001"), 
                        vectors=c("v_CA21_5904", "v_CA21_5907", "v_CA21_5910", "v_CA21_6519", "v_CA21_6526", "v_CA21_6522", "v_CA21_6570", 
                                  "v_CA21_6573", "v_CA21_6576", "v_CA21_6579", "v_CA21_6582", "v_CA21_6585", "v_CA21_6588", "v_CA21_6591",
                                  "v_CA21_6594", "v_CA21_6597", "v_CA21_7635", "v_CA21_7644", "v_CA21_7647", "v_CA21_7650", "v_CA21_7653"),level='DA')

write.csv(Oakdata24, "Oakdata24.csv")

Oakdata25 <- get_census(dataset='CA21', regions=list(CSD="3524001"), 
                        vectors=c("v_CA21_6498", "v_CA21_6501", "v_CA21_5868", "v_CA21_5871", "v_CA21_5877", "v_CA21_5895", "v_CA21_1085", 
                                  "v_CA21_945", "v_CA21_946", "v_CA21_947", "v_CA21_948", "v_CA21_949", "v_CA21_950", "v_CA21_951",
                                  "v_CA21_952", "v_CA21_953", "v_CA21_954", "v_CA21_955", "v_CA21_956", "v_CA21_957", "v_CA21_958","v_CA21_959","v_CA21_960"),level='DA')

write.csv(Oakdata25, "Oakdata25.csv")

#Assuming that 2a has all the data required for the analysis.
#Creating a for loop to get data for all CSDs in Ontario

### YEAR 2011 ###

list_census_regions('CA11')
regions_11 <- list_census_regions("CA11")# Get all census regions
csd_regions_11 <- regions_11[regions_11$level == "CSD", ]  # Filter for Census Subdivisions (CSDs)
print(csd_regions_11)

# filter data for top 50 pop cities
csd_regions_11 <- csd_regions_11 %>%
  arrange(desc(pop)) %>%  # Sort by population in descending order
  head(5)  # Select the top 50

# create a folder
output_folder_11 <- "census_data11"  # Change to your desired path
if (!dir.exists(output_folder_11)) {
  dir.create(output_folder_11, recursive = TRUE)
}

for (i in 1:nrow(csd_regions_11)) {
  csd_id_11 <- csd_regions_11$region[i]  
  csd_name_11 <- csd_regions_11$name[i]  
  print(csd_name_11)
  
  data_11 <- get_census(
    dataset = "CA11",
    regions = list(CSD = csd_id_11),
    vectors = c("v_CA11_454","v_CA11_463","v_CA11_466","v_CA11_469","v_CA11_472","v_CA11_475","v_CA11_2207","v_CA11_2209","v_CA11_2208", "v_CA11_408","v_CA11_409",
                "v_CA11_410","v_CA11_411","v_CA11_417","v_CA11_479","v_CA11_480","v_CA11_481","v_CA11_482","v_CA11_483",
                "v_CA11_2309","v_CA11_2312","v_CA11_2315","v_CA11_2318","v_CA11_2321",
                "v_CA11_2324","v_CA11_2327","v_CA11_2330","v_CA11_2333","v_CA11_2336","v_CA11_2339",
                "v_CA11_2342","v_CA11_2570","v_CA11_2540","v_CA11_5795","v_CA11_5798",
                "v_CA11_5801","v_CA11_5804","v_CA11_5807","v_CA11_5810","v_CA11_5816","v_CA11_5819",
                "v_CA11_5822","v_CA11_5825","v_CA11_5828",
                "v_CA11_2427","v_CA11_2428","v_CA11_2429","v_CA11_2430","v_CA11_2431","v_CA11_2432","v_CA11_2433","v_CA11_2434","v_CA11_2435","v_CA11_2436",
                "v_CA11_2437","v_CA11_2438","v_CA11_2439","v_CA11_2440","v_CA11_2441","v_CA11_2442",
                "v_CA11_4893","v_CA11_4894","v_CA11_4895","v_CA11_4896","v_CA11_4891","v_CA11_4892",
                "v_CA11_4897","v_CA11_4898","v_CA11_4898","v_CA11_4899","v_CA11_4900","v_CA11_4901",
                "v_CA11_5099","v_CA11_5102","v_CA11_5108","v_CA11_5117","v_CA11_5120","v_CA11_5123",
                "v_CA11_5603","v_CA11_5606","v_CA11_5609","v_CA11_5663","v_CA11_5669","v_CA11_5672","v_CA11_5675",
                "v_CA11_5678","v_CA11_5681","v_CA11_5684","v_CA11_5687","v_CA11_5690",
                "v_CA11_4002","v_CA11_4806","v_CA11_4014","v_CA11_4044","v_CA11_4266","v_CA11_4329","v_CA11_4611","v_CA11_4698",
                "v_CA11_4743","v_CA11_4800","v_CA11_3408","v_CA11_3411","v_CA11_3435"), 
    level = "DA"
  )  
  # Save CSV to the specified folder
  write.csv(data_11, file = file.path(output_folder_11, paste0(csd_name_11, "_11.csv")))
}


### YEAR 2016 ###


list_census_regions('CA16')
regions_16 <- list_census_regions("CA16")# Get all census regions
csd_regions_16 <- regions_16[regions_16$level == "CSD", ]  # Filter for Census Subdivisions (CSDs)
print(csd_regions_16)

# filter data for top 50 pop cities
csd_regions_16 <- csd_regions_16 %>%
  arrange(desc(pop)) %>%  # Sort by population in descending order
  head(5)  # Select the top 50

# create a folder
output_folder_16 <- "census_data16"  # Change to your desired path
if (!dir.exists(output_folder_16)) {
  dir.create(output_folder_16, recursive = TRUE)
}

for (i in 1:nrow(csd_regions_16)) {
  csd_id_16 <- csd_regions_16$region[i]  
  csd_name_16 <- csd_regions_16$name[i]  
  print(csd_name_16)
  
  data_16 <- get_census_16(
    dataset = "CA16",
    regions = list(CSD = csd_id_16),
    vectors = c("v_CA16_454","v_CA16_463","v_CA16_466","v_CA16_469","v_CA16_472","v_CA16_475","v_CA16_2207","v_CA16_2209","v_CA16_2208", "v_CA16_408","v_CA16_409",
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
                "v_CA16_4743","v_CA16_4800","v_CA16_3408","v_CA16_3411","v_CA16_3435"), 
    level = "DA"
  )
  # Save CSV to the specified folder
  write.csv(data_16, file = file.path(output_folder_16, paste0(csd_name_16, "_16.csv")))
}

### YEAR 2021 ###

list_census_regions('CA21')
regions_21 <- list_census_regions("CA21")# Get all census regions
csd_regions_21 <- regions_21[regions_21$level == "CSD", ]  # Filter for Census Subdivisions (CSDs)
print(csd_regions_21)

# filter data for top 50 pop cities
csd_regions_21 <- csd_regions_21 %>%
  arrange(desc(pop)) %>%  # Sort by population in descending order
  head(5)  # Select the top 50

# create a folder
output_folder_21 <- "census_data21"  # Change to your desired path
if (!dir.exists(output_folder_21)) {
  dir.create(output_folder_21, recursive = TRUE)
}
  
for (i in 1:nrow(csd_regions_21)) {
  csd_id_21 <- csd_regions_21$region[i]  
  csd_name_21 <- csd_regions_21$name[i]  
  print(csd_name_21)
  
  data_21 <- get_census(
    dataset = "CA21",
    regions = list(CSD = csd_id_21),
    vectors = c("v_CA21_454","v_CA21_463","v_CA21_466","v_CA21_469","v_CA21_472","v_CA21_475","v_CA21_2207","v_CA21_2209","v_CA21_2208", "v_CA21_408","v_CA21_409",
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
                "v_CA21_4743","v_CA21_4800","v_CA21_3408","v_CA21_3411","v_CA21_3435"), 
    level = "DA"
  )  
  # Save CSV to the specified folder
  write.csv(data_21, file = file.path(output_folder_21, paste0(csd_name_21, "_21.csv")))
}
  

# Define your vectors
vectors <- c("v_CA16_454","v_CA16_463","v_CA16_466","v_CA16_469","v_CA16_472","v_CA16_475","v_CA16_2207","v_CA16_2209","v_CA16_2208", 
             "v_CA16_408","v_CA16_409","v_CA16_410","v_CA16_411","v_CA16_417","v_CA16_479","v_CA16_480","v_CA16_481","v_CA16_482",
             "v_CA16_483","v_CA16_2309","v_CA16_2312","v_CA16_2315","v_CA16_2318","v_CA16_2321","v_CA16_2324","v_CA16_2327",
             "v_CA16_2330","v_CA16_2333","v_CA16_2336","v_CA16_2339","v_CA16_2342","v_CA16_2570","v_CA16_2540","v_CA16_5795",
             "v_CA16_5798","v_CA16_5801","v_CA16_5804","v_CA16_5807","v_CA16_5810","v_CA16_5816","v_CA16_5819","v_CA16_5822",
             "v_CA16_5825","v_CA16_5828","v_CA16_2427","v_CA16_2428","v_CA16_2429","v_CA16_2430","v_CA16_2431","v_CA16_2432",
             "v_CA16_2433","v_CA16_2434","v_CA16_2435","v_CA16_2436","v_CA16_2437","v_CA16_2438","v_CA16_2439","v_CA16_2440",
             "v_CA16_2441","v_CA16_2442","v_CA16_4893","v_CA16_4894","v_CA16_4895","v_CA16_4896","v_CA16_4891","v_CA16_4892",
             "v_CA16_4897","v_CA16_4898","v_CA16_4898","v_CA16_4899","v_CA16_4900","v_CA16_4901","v_CA16_5099","v_CA16_5102",
             "v_CA16_5108","v_CA16_5117","v_CA16_5120","v_CA16_5123","v_CA16_5603","v_CA16_5606","v_CA16_5609","v_CA16_5663",
             "v_CA16_5669","v_CA16_5672","v_CA16_5675","v_CA16_5678","v_CA16_5681","v_CA16_5684","v_CA16_5687","v_CA16_5690",
             "v_CA16_4002","v_CA16_4806","v_CA16_4014","v_CA16_4044","v_CA16_4266","v_CA16_4329","v_CA16_4611","v_CA16_4698",
             "v_CA16_4743","v_CA16_4800","v_CA16_3408","v_CA16_3411","v_CA16_3435")

# Get all available variables
variables <- list_census_vectors("CA16")

matched_results <- variables[variables$vector %in% vectors, , drop = FALSE]
print(matched_results)

# Save the matched results as a CSV file
write.csv(matched_results, "matched_variables.csv", row.names = FALSE)


print("Done!")