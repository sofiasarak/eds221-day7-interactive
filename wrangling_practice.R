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
