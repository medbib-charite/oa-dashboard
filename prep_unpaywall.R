#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combining Unpaywall data for all years, keeping relevant columns ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load libraries ----
library(readxl)
library(tidyverse)
library(dplyr)
library(janitor)
library(jsonlite)

# License levels, ordered from most to least permissive, see https://creativecommons.org/about/cclicenses/
license_levels = c("cc-by", "cc-by-sa", "cc-by-nc", "cc-by-nc-sa", "cc-by-nd", "cc-by-nc-nd", "other license", "no license found")


# 2016-2020: Load and clean Unpaywall data, retrieved in 2021 ----
load("data/data_unpaywall.Rda")
unpaywall_2016_2020_raw <- data_unpaywall

unpaywall_2016_2020_unnested <- unpaywall_2016_2020_raw %>%
  mutate(year = as.double(year)) %>%
  unnest(oa_locations, keep_empty = TRUE) %>%
  mutate(license = case_when(grepl("specific|cc0|implied|pd", license, ignore.case = TRUE) ~ "other license",
                             TRUE ~ license)) %>%
  mutate(license = replace_na(license, "no license found")) %>%
  mutate(license = factor(license, levels = license_levels, ordered = TRUE))

# Find best license for each article based on license_levels
unpaywall_2016_2020_distinct_best_license <- unpaywall_2016_2020_unnested %>%
  group_by(doi) %>%
  mutate(min_license = min(license)) %>%
  mutate(best_license = min_license == license) %>%
  filter(best_license) %>%
  distinct(doi, .keep_all = TRUE) # NOTE this rather randomly chooses for each article one row with best license in disregard of other properties from license. FIXME if others are important

unpaywall_2016_2020_slim <- unpaywall_2016_2020_distinct_best_license %>%
  mutate(origin_unpaywall = "2016_2020") %>%
  select(doi,
         unpw_year = year,
         unpw_publisher = publisher,
         journal_is_in_doaj,
         license,
         # unpw_is_oa = is_oa, # already is in data to join
         # unpw_oa_status = oa_status,
         has_repository_copy,
         origin_unpaywall)


# 2021: Load and clean Unpaywall data, retrieved 2022-09-20 ----
unpaywall_2021_file <- "raw_data/2021_unpaywall_fetched_2022-09-20.xlsx"
unpaywall_2021_raw <- read_excel(unpaywall_2021_file)

# Remove doi duplicates keeping last entry (this is the newest as there were changed during the API request)
# https://datacornering.com/remove-duplicates-and-keep-last-in-r/
unpaywall_2021_clean <- unpaywall_2021_raw %>%
  clean_names() %>%
  mutate(year = as.double(year)) %>%
  group_by(doi) %>%
  filter(row_number() == n())

unpaywall_2021_oalocations <- unpaywall_2021_clean %>%
  select(doi, oa_locations) %>%
  drop_na(oa_locations) %>%
  filter(oa_locations != "[]") %>%
  rowwise() %>%
  mutate(oa_locations = list(fromJSON(oa_locations)))

unpaywall_2021_unnested <- unpaywall_2021_clean %>%
  select(-oa_locations) %>%
  left_join(unpaywall_2021_oalocations, by = "doi") %>%
  rename(updated_dataset = updated) %>%
  unnest(oa_locations, keep_empty = TRUE) %>%
  mutate(license = case_when(grepl("specific|cc0|implied|pd", license, ignore.case = TRUE) ~ "other license",
                             TRUE ~ license)) %>%
  mutate(license = replace_na(license, "no license found")) %>%
  mutate(license = factor(license, levels = license_levels, ordered = TRUE))

unpaywall_2021_distinct_best_license <- unpaywall_2021_unnested %>%
  group_by(doi) %>%
  mutate(min_license = min(license)) %>%
  mutate(best_license = min_license == license) %>%
  filter(best_license) %>%
  distinct(doi, .keep_all = TRUE) # NOTE this rather randomly chooses one license. FIXME if other properties from license are important

unpaywall_2021_slim <- unpaywall_2021_distinct_best_license %>%
  mutate(origin_unpaywall = "2021") %>%
  select(doi,
         unpw_year = year,
         unpw_publisher = publisher,
         journal_is_in_doaj,
         license,
         # unpw_is_oa = is_oa, # already is in data to join
         # unpw_oa_status = oa_status,
         has_repository_copy,
         origin_unpaywall)

# Combine Unpaywall data all years ----
unpaywall_2016_2021_slim <- rbind(unpaywall_2016_2020_slim, unpaywall_2021_slim) %>%
  distinct(doi, .keep_all = TRUE)
