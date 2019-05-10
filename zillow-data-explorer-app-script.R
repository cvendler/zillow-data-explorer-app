
# Load the necessary libraries

library(plyr)
library(tigris)
library(zoo)
library(lubridate)
library(janitor)
library(tidyverse)

# Read in the csv file for median Zillow Home Value Index (ZHVI) by zip code from Zillow's website

zhvi_median_value <- read_csv(url("http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_AllHomes.csv")) %>% 
  
  # Use the "clean_names" function from the "janitor" package to pretty the
  # column names
  
  clean_names()

# Perform data cleaning on the ZHVI data

zhvi_median_value <- zhvi_median_value %>% 
  
  # Turn the data from wide format into long format using the "gather" function
  # on all columns corresponding to years-months, gathering them under the
  # variable name "period" and creating a new variable called
  # "median_zhvi_value" for each period's associated median ZHVI value
  
  gather(period, median_zhvi_value, 8:ncol(zhvi_median_value)) %>% 
  
  # Modify the "period" column so that it contains the cleaned period labels
  # created in the last line by keeping only the year, month, and separator
  
  mutate(period = str_sub(period, 2, 8)) %>% 
  
  # Replace the underscore separator with a hyphen separator for consistency
  # with standard styling
  
  mutate(period = str_replace(period, "_", "-")) %>% 
  
  # Turn the "period" variable, which currently is a string, into a date using
  # the "as.yearmon" and "as.Date" functions; doing so will allow the data to be
  # plotted showing changes over time in plotly graphs in the actual app
  
  mutate(period = as.Date(as.yearmon(period))) %>% 
  
  # Create a new "year" variable consisting of only the year from the "period"
  # column
  
  mutate(year = str_sub(period, 1, 4)) %>% 
  
  # Save the "year" variable as a year object using the lubridate "year"
  # function along with the "as.Date" function; this column will be used for the
  # leaflet map in the actual app, in which users can be shown summary
  # statistics by year
  
  mutate(year = year(as.Date(year, "%Y"))) %>% 
  
  # Rename the "region_name" variable to "zip_code" so that the column names are
  # more intuitive
  
  rename(zip_code = region_name) %>% 
  
  # Create a new "city_name" variable consisting of the "city" and "state"
  # variables for each observation so that the data can be grouped by
  # "city_name" and cities of the same name with different states will not be
  # grouped together (for example, "Los Angeles, CA")
  
  mutate(city_name = paste(city, state, sep = ", ")) %>% 
  
  # Modify the "county_name" variable so that it includes the "county_name" and
  # "state" variables for each observation so that the data can be grouped by
  # "county_name" and counties of the same name with different states will not
  # be grouped together (for example, "Los Angeles County, CA")
  
  mutate(county_name = paste(county_name, state, sep = ", ")) %>% 
  
  # Create a new "state_name" variable, which consists of the full name of each
  # state; doing so will allow the labels/options in the app to be less
  # confusing, as people may not know all of the two-letter state abbreviations
  # from memory
  
  mutate(state_name = case_when(state == "AL" ~ "Alabama", 
                                state == "AK" ~ "Alaska", 
                                state == "AZ" ~ "Arizona", 
                                state == "AR" ~ "Arkansas", 
                                state == "CA" ~ "California", 
                                state == "CO" ~ "Colorado", 
                                state == "CT" ~ "Connecticut", 
                                state == "DE" ~ "Delaware", 
                                state == "FL" ~ "Florida", 
                                state == "GA" ~ "Georgia", 
                                state == "HI" ~ "Hawaii", 
                                state == "ID" ~ "Idaho", 
                                state == "IL" ~ "Illinois", 
                                state == "IN" ~ "Indiana", 
                                state == "IA" ~ "Iowa", 
                                state == "KS" ~ "Kansas",
                                state == "KY" ~ "Kentucky",
                                state == "LA" ~ "Louisiana",
                                state == "ME" ~ "Maine",
                                state == "MD" ~ "Maryland",
                                state == "MA" ~ "Massachusetts",
                                state == "MI" ~ "Michigan",
                                state == "MN" ~ "Minnesota",
                                state == "MS" ~ "Mississippi",
                                state == "MO" ~ "Missouri",
                                state == "MT" ~ "Montana",
                                state == "NE" ~ "Nebraska", 
                                state == "NV" ~ "Nevada", 
                                state == "NH" ~ "New Hampshire",
                                state == "NJ" ~ "New Jersey",
                                state == "NM" ~ "New Mexico",
                                state == "NY" ~ "New York",
                                state == "NC" ~ "North Carolina",
                                state == "ND" ~ "North Dakota",
                                state == "OH" ~ "Ohio",
                                state == "OK" ~ "Oklahoma",
                                state == "OR" ~ "Oregon", 
                                state == "PA" ~ "Pennsylvania",
                                state == "RI" ~ "Rhode Island",
                                state == "SC" ~ "South Carolina",
                                state == "SD" ~ "South Dakota",
                                state == "TN" ~ "Tennessee",
                                state == "TX" ~ "Texas",
                                state == "UT" ~ "Utah",
                                state == "VT" ~ "Vermont",
                                state == "VA" ~ "Virginia",
                                state == "WA" ~ "Washington",
                                state == "DC" ~ "Washington, D.C.",
                                state == "WV" ~ "West Virginia",
                                state == "WI" ~ "Wisconsin",
                                state == "WY" ~ "Wyoming")) %>% 
  
  # Deselect the variables that are no longer needed
  
  select(-c(region_id, city, state, metro, size_rank))

# Read in the csv file for median value per square foot by zip code from Zillow's website

median_sqft_value <- read_csv(url("http://files.zillowstatic.com/research/public/Zip/Zip_MedianValuePerSqft_AllHomes.csv")) %>% 
  
  # Use the "clean_names" function from the "janitor" package to pretty the
  # column names
  
  clean_names()

# Perform data cleaning on the value per square foot data

median_sqft_value <- median_sqft_value %>% 
  
  # Turn the data from wide format into long format using the "gather" function
  # on all columns corresponding to years-months, gathering them under the
  # variable name "period" and creating a new variable called
  # "median_value_per_sqft" for each period's associated median value per square
  # foot
  
  gather(period, median_value_per_sqft, 8:ncol(median_sqft_value)) %>% 
  
  # Modify the "period" column so that it contains the cleaned period labels
  # created in the last line by keeping only the year, month, and separator
  
  mutate(period = str_sub(period, 2, 8)) %>% 
  
  # Replace the underscore separator with a hyphen separator for consistency
  # with standard styling
  
  mutate(period = str_replace(period, "_", "-")) %>% 
  
  # Turn the "period" variable, which currently is a string, into a date using
  # the "as.yearmon" and "as.Date" functions; doing so will allow the data to be
  # plotted showing changes over time in plotly graphs in the actual app
  
  mutate(period = as.Date(as.yearmon(period))) %>% 
  
  # Create a new "year" variable consisting of only the year from the "period"
  # column
  
  mutate(year = str_sub(period, 1, 4)) %>% 
  
  # Save the "year" variable as a year object using the lubridate "year"
  # function along with the "as.Date" function; this column will be used for the
  # leaflet map in the actual app, in which users can be shown summary
  # statistics by year
  
  mutate(year = year(as.Date(year, "%Y"))) %>% 
  
  # Rename the "region_name" variable to "zip_code" so that the column names are
  # more intuitive
  
  rename(zip_code = region_name) %>% 
  
  # Create a new "city_name" variable consisting of the "city" and "state"
  # variables for each observation so that the data can be grouped by
  # "city_name" and cities of the same name with different states will not be
  # grouped together (for example, "Los Angeles, CA")
  
  mutate(city_name = paste(city, state, sep = ", ")) %>% 
  
  # Modify the "county_name" variable so that it includes the "county_name" and
  # "state" variables for each observation so that the data can be grouped by
  # "county_name" and counties of the same name with different states will not
  # be grouped together (for example, "Los Angeles County, CA")
  
  mutate(county_name = paste(county_name, state, sep = ", ")) %>% 
  
  # Create a new "state_name" variable, which consists of the full name of each
  # state; doing so will allow the labels/options in the app to be less
  # confusing, as people may not know all of the two-letter state abbreviations
  # from memory
  
  mutate(state_name = case_when(state == "AL" ~ "Alabama", 
                                state == "AK" ~ "Alaska", 
                                state == "AZ" ~ "Arizona", 
                                state == "AR" ~ "Arkansas", 
                                state == "CA" ~ "California", 
                                state == "CO" ~ "Colorado", 
                                state == "CT" ~ "Connecticut", 
                                state == "DE" ~ "Delaware", 
                                state == "FL" ~ "Florida", 
                                state == "GA" ~ "Georgia", 
                                state == "HI" ~ "Hawaii", 
                                state == "ID" ~ "Idaho", 
                                state == "IL" ~ "Illinois", 
                                state == "IN" ~ "Indiana", 
                                state == "IA" ~ "Iowa", 
                                state == "KS" ~ "Kansas",
                                state == "KY" ~ "Kentucky",
                                state == "LA" ~ "Louisiana",
                                state == "ME" ~ "Maine",
                                state == "MD" ~ "Maryland",
                                state == "MA" ~ "Massachusetts",
                                state == "MI" ~ "Michigan",
                                state == "MN" ~ "Minnesota",
                                state == "MS" ~ "Mississippi",
                                state == "MO" ~ "Missouri",
                                state == "MT" ~ "Montana",
                                state == "NE" ~ "Nebraska", 
                                state == "NV" ~ "Nevada", 
                                state == "NH" ~ "New Hampshire",
                                state == "NJ" ~ "New Jersey",
                                state == "NM" ~ "New Mexico",
                                state == "NY" ~ "New York",
                                state == "NC" ~ "North Carolina",
                                state == "ND" ~ "North Dakota",
                                state == "OH" ~ "Ohio",
                                state == "OK" ~ "Oklahoma",
                                state == "OR" ~ "Oregon", 
                                state == "PA" ~ "Pennsylvania",
                                state == "RI" ~ "Rhode Island",
                                state == "SC" ~ "South Carolina",
                                state == "SD" ~ "South Dakota",
                                state == "TN" ~ "Tennessee",
                                state == "TX" ~ "Texas",
                                state == "UT" ~ "Utah",
                                state == "VT" ~ "Vermont",
                                state == "VA" ~ "Virginia",
                                state == "WA" ~ "Washington",
                                state == "DC" ~ "Washington, D.C.",
                                state == "WV" ~ "West Virginia",
                                state == "WI" ~ "Wisconsin",
                                state == "WY" ~ "Wyoming")) %>% 
  
  # Deselect the variables that are no longer needed
  
  select(-c(region_id, city, state, metro, size_rank))

# Read in the csv file for percent of homes increasing in value by zip code from
# Zillow's website

percent_homes_increased_value <- read_csv(url("http://files.zillowstatic.com/research/public/Zip/Zip_PctOfHomesIncreasingInValues_AllHomes.csv")) %>% 
  
  # Use the "clean_names" function from the "janitor" package to pretty the
  # column names
  
  clean_names()

# Perform data cleaning on the percent increasing data

percent_homes_increased_value <- percent_homes_increased_value %>% 
  
  # Turn the data from wide format into long format using the "gather" function
  # on all columns corresponding to years-months, gathering them under the
  # variable name "period" and creating a new variable called
  # "percent_increased" for each period's associated percentage of homes
  # increasing in value
  
  gather(period, percent_increased, 8:ncol(percent_homes_increased_value)) %>% 
  
  # Modify the "period" column so that it contains the cleaned period labels
  # created in the last line by keeping only the year, month, and separator
  
  mutate(period = str_sub(period, 2, 8)) %>% 
  
  # Replace the underscore separator with a hyphen separator for consistency
  # with standard styling
  
  mutate(period = str_replace(period, "_", "-")) %>% 
  
  # Turn the "period" variable, which currently is a string, into a date using
  # the "as.yearmon" and "as.Date" functions; doing so will allow the data to be
  # plotted showing changes over time in plotly graphs in the actual app
  
  mutate(period = as.Date(as.yearmon(period))) %>% 
  
  # Create a new "year" variable consisting of only the year from the "period"
  # column
  
  mutate(year = str_sub(period, 1, 4)) %>% 
  
  # Save the "year" variable as a year object using the lubridate "year"
  # function along with the "as.Date" function; this column will be used for the
  # leaflet map in the actual app in which users can be shown summary statistics
  # by year
  
  mutate(year = year(as.Date(year, "%Y"))) %>% 
  
  # Rename the "region_name" variable to "zip_code" so that the column names are
  # more intuitive
  
  rename(zip_code = region_name) %>% 
  
  # Create a new "city_name" variable consisting of the "city" and "state"
  # variables for each observation so that the data can be grouped by
  # "city_name" and cities of the same name with different states will not be
  # grouped together (for example, "Los Angeles, CA")
  
  mutate(city_name = paste(city, state, sep = ", ")) %>% 
  
  # Modify the "county_name" variable so that it includes the "county_name" and
  # "state" variables for each observation so that the data can be grouped by
  # "county_name" and counties of the same name with different states will not
  # be grouped together (for example, "Los Angeles County, CA")
  
  mutate(county_name = paste(county_name, state, sep = ", ")) %>% 
  
  # Create a new "state_name" variable, which consists of the full name of each
  # state; doing so will allow the labels/options in the app to be less
  # confusing, as people may not know all of the two-letter state abbreviations
  # from memory
  
  mutate(state_name = case_when(state == "AL" ~ "Alabama", 
                                state == "AK" ~ "Alaska", 
                                state == "AZ" ~ "Arizona", 
                                state == "AR" ~ "Arkansas", 
                                state == "CA" ~ "California", 
                                state == "CO" ~ "Colorado", 
                                state == "CT" ~ "Connecticut", 
                                state == "DE" ~ "Delaware", 
                                state == "FL" ~ "Florida", 
                                state == "GA" ~ "Georgia", 
                                state == "HI" ~ "Hawaii", 
                                state == "ID" ~ "Idaho", 
                                state == "IL" ~ "Illinois", 
                                state == "IN" ~ "Indiana", 
                                state == "IA" ~ "Iowa", 
                                state == "KS" ~ "Kansas",
                                state == "KY" ~ "Kentucky",
                                state == "LA" ~ "Louisiana",
                                state == "ME" ~ "Maine",
                                state == "MD" ~ "Maryland",
                                state == "MA" ~ "Massachusetts",
                                state == "MI" ~ "Michigan",
                                state == "MN" ~ "Minnesota",
                                state == "MS" ~ "Mississippi",
                                state == "MO" ~ "Missouri",
                                state == "MT" ~ "Montana",
                                state == "NE" ~ "Nebraska", 
                                state == "NV" ~ "Nevada", 
                                state == "NH" ~ "New Hampshire",
                                state == "NJ" ~ "New Jersey",
                                state == "NM" ~ "New Mexico",
                                state == "NY" ~ "New York",
                                state == "NC" ~ "North Carolina",
                                state == "ND" ~ "North Dakota",
                                state == "OH" ~ "Ohio",
                                state == "OK" ~ "Oklahoma",
                                state == "OR" ~ "Oregon", 
                                state == "PA" ~ "Pennsylvania",
                                state == "RI" ~ "Rhode Island",
                                state == "SC" ~ "South Carolina",
                                state == "SD" ~ "South Dakota",
                                state == "TN" ~ "Tennessee",
                                state == "TX" ~ "Texas",
                                state == "UT" ~ "Utah",
                                state == "VT" ~ "Vermont",
                                state == "VA" ~ "Virginia",
                                state == "WA" ~ "Washington",
                                state == "DC" ~ "Washington, D.C.",
                                state == "WV" ~ "West Virginia",
                                state == "WI" ~ "Wisconsin",
                                state == "WY" ~ "Wyoming")) %>% 
  
  # Deselect the variables that are no longer needed
  
  select(-c(region_id, city, state, metro, size_rank))

# Read in the csv file for percent of homes decreasing in value by zip code from
# Zillow's website

percent_homes_decreased_value <- read_csv(url("http://files.zillowstatic.com/research/public/Zip/Zip_PctOfHomesDecreasingInValues_AllHomes.csv")) %>% 
  
  # Use the "clean_names" function from the "janitor" package to pretty the
  # column names
  
  clean_names()

# Perform data cleaning on the percent decreasing data

percent_homes_decreased_value <- percent_homes_decreased_value %>% 
  
  # Turn the data from wide format into long format using the "gather" function
  # on all columns corresponding to years-months, gathering them under the
  # variable name "period" and creating a new variable called
  # "percent_decreased" for each period's associated percentage of homes
  # decreasing in value
  
  gather(period, percent_decreased, 8:ncol(percent_homes_decreased_value)) %>% 
  
  # Modify the "period" column so that it contains the cleaned period labels
  # created in the last line by only keeping the year, month, and separator
  
  mutate(period = str_sub(period, 2, 8)) %>% 
  
  # Replace the underscore separator with a hyphen separator for consistency
  # with standard styling
  
  mutate(period = str_replace(period, "_", "-")) %>% 
  
  # Turn the "period" variable, which currently is a string, into a date using
  # the "as.yearmon" and "as.Date" functions; doing so will allow the data to be
  # plotted showing changes over time in plotly graphs in the actual app
  
  mutate(period = as.Date(as.yearmon(period))) %>% 
  
  # Create a new "year" variable consisting of only the year from the "period"
  # column
  
  mutate(year = str_sub(period, 1, 4)) %>% 
  
  # Save the "year" variable as a year object using the lubridate "year"
  # function along with the "as.Date" function; this column will be used for the
  # leaflet map in the actual app in which users can be shown summary statistics
  # by year
  
  mutate(year = year(as.Date(year, "%Y"))) %>% 
  
  # Rename the "region_name" variable as "zip_code" so that the column names are
  # more intuitive
  
  rename(zip_code = region_name) %>% 
  
  # Create a new "city_name" variable consisting of the "city" and "state"
  # variables for each observation so that the data can be grouped by
  # "city_name" and cities of the same name with different states will not be
  # grouped together (for example, "Los Angeles, CA")
  
  mutate(city_name = paste(city, state, sep = ", ")) %>% 
  
  # Modify the "county_name" variable so that it includes the "county_name" and
  # "state" variables for each observation so that the data can be grouped by
  # "county_name" and counties of the same name with different states will not
  # be grouped together (for example, "Los Angeles County, CA")
  
  mutate(county_name = paste(county_name, state, sep = ", ")) %>% 
  
  # Create a new "state_name" variable which consists of the full name of each
  # state; doing so will allow the labels/options in the app to be less
  # confusing, as people may not know all of the two-letter state abbreviations
  # from memory
  
  mutate(state_name = case_when(state == "AL" ~ "Alabama", 
                                state == "AK" ~ "Alaska", 
                                state == "AZ" ~ "Arizona", 
                                state == "AR" ~ "Arkansas", 
                                state == "CA" ~ "California", 
                                state == "CO" ~ "Colorado", 
                                state == "CT" ~ "Connecticut", 
                                state == "DE" ~ "Delaware", 
                                state == "FL" ~ "Florida", 
                                state == "GA" ~ "Georgia", 
                                state == "HI" ~ "Hawaii", 
                                state == "ID" ~ "Idaho", 
                                state == "IL" ~ "Illinois", 
                                state == "IN" ~ "Indiana", 
                                state == "IA" ~ "Iowa", 
                                state == "KS" ~ "Kansas",
                                state == "KY" ~ "Kentucky",
                                state == "LA" ~ "Louisiana",
                                state == "ME" ~ "Maine",
                                state == "MD" ~ "Maryland",
                                state == "MA" ~ "Massachusetts",
                                state == "MI" ~ "Michigan",
                                state == "MN" ~ "Minnesota",
                                state == "MS" ~ "Mississippi",
                                state == "MO" ~ "Missouri",
                                state == "MT" ~ "Montana",
                                state == "NE" ~ "Nebraska", 
                                state == "NV" ~ "Nevada", 
                                state == "NH" ~ "New Hampshire",
                                state == "NJ" ~ "New Jersey",
                                state == "NM" ~ "New Mexico",
                                state == "NY" ~ "New York",
                                state == "NC" ~ "North Carolina",
                                state == "ND" ~ "North Dakota",
                                state == "OH" ~ "Ohio",
                                state == "OK" ~ "Oklahoma",
                                state == "OR" ~ "Oregon", 
                                state == "PA" ~ "Pennsylvania",
                                state == "RI" ~ "Rhode Island",
                                state == "SC" ~ "South Carolina",
                                state == "SD" ~ "South Dakota",
                                state == "TN" ~ "Tennessee",
                                state == "TX" ~ "Texas",
                                state == "UT" ~ "Utah",
                                state == "VT" ~ "Vermont",
                                state == "VA" ~ "Virginia",
                                state == "WA" ~ "Washington",
                                state == "DC" ~ "Washington, D.C.",
                                state == "WV" ~ "West Virginia",
                                state == "WI" ~ "Wisconsin",
                                state == "WY" ~ "Wyoming")) %>% 
  
  # Deselect the variables that are no longer needed
  
  select(-c(region_id, city, state, metro, size_rank))

# Create a new dataframe called "zillow_historical_data", joining all of the
# dataframes just created by "period", "year", "zip_code", "city_name",
# "county_name", "state_name"; this join is a full_join so that all observations
# from all dataframes are kept

zillow_historical_data <- join_all(list(zhvi_median_value, median_sqft_value, percent_homes_increased_value, percent_homes_decreased_value), 
                                   by = c("period", "year", "zip_code", "city_name", "county_name", "state_name"), 
                                   type = "full")

# Create a new dataframe called "edited_fips_codes", which contains the county
# FIPS codes for all U.S. counties; this dataframe will be joined to the
# "zillow_historical_data" dataframe so that the values can be joined with
# county shapefiles to create the leaflet maps in the app

edited_fips_codes <- fips_codes %>% 
  
  # Modify the "county_code" variable so that it now contains the state and
  # county codes pasted together to make each county's unique, 5-digit FIPS code
  
  mutate(county_code = paste0(state_code, county_code)) %>% 
  
  # Modify the "county" variable so that each entry is in title case as it is in
  # the "zillow_historical_data" dataframe
  
  mutate(county = str_to_title(county)) %>% 
  
  # Create a variable called "county_name" consisting of the "county" and
  # "state" variables for each observation, which matches the "county_name"
  # variable in the "zillow_historical_data" dataframe
  
  mutate(county_name = paste(county, state, sep = ", ")) %>% 
  
  # Deselect the variables that are no longer needed
  
  select(-c(state, county, state_code))

# Overwrite the "zillow_historical_data" dataframe by doing a full join of the
# old "zillow_historical_data" dataframe and the "edited_fips_codes" dataframe
# by the "county_name" and "state_name" variables

zillow_historical_data <- full_join(zillow_historical_data, edited_fips_codes, by = c("county_name", "state_name"))

# Write the "zillow_historical_data" dataframe to an rds file in the app folder
# so that it may be accessed and used by the app

write_rds(zillow_historical_data, path = "zillow-data-explorer-app/zillow_historical_data.rds")
