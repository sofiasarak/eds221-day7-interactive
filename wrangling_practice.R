#clear environment
rm(list = ls())

#attach packages
library(tidyverse)
library(palmerpenguins)
library(lubridate) #helps us work with dates

#data wrangling refresher
# 1. only include penguins at Biscoe and Dream Islands
# 2. remove the year and sex variables
# 3. add a new column called body_mass_kg with penguin mass converted from grams to kg
# 4. rename the island variable to location

penguins <- penguins
penguinsx <- penguins |>
  filter(island == c("Biscoe", "Dream")) %>% #could also use %in% instead of ==
  select(-year, -sex) %>%
  mutate(body_mass_kg = body_mass_g/1000) %>%
  rename(location = island)

# 1. limit to only Adelie penguins
# 2. remove any observations where flipper_length_mm is NA
# 3. group the data by sex
# 4. find the mean, sd, and sample size n() of flipper lengths for males and females

penguins_summary <- penguins |>
  filter(species == "Adelie") %>% #could also include the next row in the same (), sep by a comma
  filter(!is.na(flipper_length_mm)) %>% #keep rows that are not NA
  group_by(sex) %>% #group_by doesn't save anything; is just used with other operations
  summarize(mean = mean(flipper_length_mm, na.rm = TRUE),
            sd = sd(flipper_length_mm, na.rm = TRUE),
            sample_size = n()) #n() doesn't require an argument bc it's just counting obvs

#pasting in example data sets using datapasta!
# Tools > Addins > Browse Addins > paste as data.frame (from datapasta)

animals <- data.frame(
  stringsAsFactors = FALSE,
          location = c("lagoon", "bluff", "creek", "oaks", "bluff"),
           species = c("bobcat", "coyote", "fox", "squirrel", "bobcat"),
          maturity = c("adult", "juvenile", "adult", "juvenile", "adult")
)

sites <- data.frame(
  stringsAsFactors = FALSE,
          location = c("beach", "lagoon", "bluff", "oaks"),
    full_site_name = c("Goleta Beach","UCSB Lagoon",
                       "Ellwood Mesa","Fremont Campground"),
      jurisdiction = c("SB City", "UCSB", "SB City", "USFS")
)


library(dplyr)

#Mutating Joins

# practice with a full_join(), everything is kept! (keeps all rows and adds all columns)
full_join(animals, sites) #arguments are data frames

#left_join(); keep first data frame basically the same, and add on new columns from sites
left_join(animals, sites)

#right_join(); the opposite, will keep four rows from sites (but also an extra from many of many)
right_join(animals, sites)

#inner_join(); only keep rows where key matches in both x and y; never any NAs bc everything will match up
inner_join(animals, sites)

#note: can only join two tables at one time!

#Filtering Joins
#don't add on any columns! and order really matters

#semi_join()
semi_join(animals, sites)
#which is this the same as this code:
animals %>%
  filter(location %in% sites$location)

anti_join(animals, sites)
#which is same as:
animals %>%
  filter(!location %in% sites$location)

semi_join(sites, animals)


#Practice with lubridate
# you have to really make sure that you know the date stucture of your data!

my_date <- "06-30-2003" #we know it's in mdy
lubridate::mdy(my_date) #put it in ISO 8601 for us!

# new format for date
my_date <- "06-September-2017"
lubridate::dmy(my_date) #didn't like Sept

#another example
my_date <- "19160518"
lubridate::ymd(my_date)

#what happens if we give lubridate a date that doesn't make sense?
lubridate::mdy("1942-08-30") #couldn't figure it out

#working with date-times

time <- "2020-08-12 11:18"
time <- ymd_hm(time) #will return as the UTC time zone

#convert to PDT
with_tz(time, "America/Los_Angeles") #converted our time to the same time in UTC but in our time zone
# to directly define the time zone, add an argument tz= (have to look up the lubridate code for your time zone)

# extract info from our dates; can ask for each individual element on its own
week(time)
year(time)
day(time)

Sys.time() #run to find current time at our computer
#can also write code to figure out how long yoru script takes to run!

start_time <- Sys.time()

end_time <- Sys.time()

end_time - start_time

#Practice lubridate within a data frame
urchin_counts <- tribble( #tribble is a data structure really similar to a data frame
  ~date, ~species, ~size_mm,
  "10/3/2020", "purple", 55,
  "10/4/2020", "red", 48,
  "11/7/2020", "red", 67
)

urchin_counts %>%
  mutate(date = lubridate::mdy(date)) %>% #overwriting our date column into the correct format
  mutate(year = year(date), month = month(date), day = day(date)) #makes a column of year by pulling it out of the data frame!

day_1 <- lubridate::ymd("2020-01-06")
day_2 <- ymd("2020-5-18")
day_3 <- ymd("2020-05-19")

#create time interval and do math with time
time_interval <- interval(day_1, day_2)
time_length(time_interval, "week") #default is seconds
time_length(time_interval, "year")

time_length(c(day_1, day_2)) #doesn't work, so have to make a time interval to use time_length() in this way

# Practice with stringr
library(stringr)

#practice with str_detect() to detect string patterns
# returns TRUE/FALSE depending on whether the pattern is detected or not

my_string <- "Teddy loves eating salmon and socks."

# does the pattern "love" exist within the string?
my_string %>%
  str_detect("love")

my_string <- c("burrito", "fish taco", "Taco salad")

# does the vector element contain the pattern "fish"?
my_string %>%
  str_detect("fish")

#powerful when combined with dplyr functions

starwars %>%
  filter(str_detect(name, "Skywalker")) #finding the rows in which the column name contains "Skywalker"; arguments are str_detect(string, pattern, negate)

firewalkers <- starwars %>%
  mutate(name = str_replace(name, "Sky", "Fire")) #helpful for cleaning up your data!

#cleaning up white space
feedback <- c(" I ate   some nachos", "Wednesday morning.     ")

# remove the leading/trailing/duplicate spaces using str_squish
str_squish(feedback) #got rid of all extra spaces!

#remove just the leading and trailing spaces
str_trim(feedback)

#convert cases
str_to_lower(feedback)
str_to_upper(feedback)
str_to_sentence(feedback)
str_to_title(feedback)

#count matches in a string
str_count(feedback, patter = "nachos") #returns n values, where n is the number of elements in the vector
