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
  group_by(sex) %>%
  summarize(mean = mean(flipper_length_mm, na.rm = TRUE),
            sd = sd(flipper_length_mm, na.rm = TRUE),
            sample_size = n()) #n() doesn't require an argument bc it's just counting obvs

