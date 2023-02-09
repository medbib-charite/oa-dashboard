#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combining Unpaywall data for all years, keeping relevant columns ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load libraries ----
library(readxl)
library(tidyverse)
library(dplyr)
library(janitor)
library(jsonlite)


# 2016-2020: Load and clean Unpaywall data, retrieved in 2021 ----
load("data/data_unpaywall.Rda")
unpaywall_2016_2020_raw <- data_unpaywall

unpaywall_2016_2020_unnested <- unpaywall_2016_2020_raw %>%
  # 1. Unnest the list structure
  unnest(oa_locations, keep_empty = TRUE) #%>%
  # 2. Clean the license strings and create "others" category
  mutate(license = case_when(grepl("specific|cc0|implied|pd", license, ignore.case = TRUE) ~ "other license",
                             TRUE ~ license)) %>%
  mutate(origin_unpaywall = "2016_2020") %>%

unpaywall_2016_2020_slim <- unpaywall_2016_2020_unnested %>%
  # FIXME: add some columns in the selection (see data_2_roadoi.R), e.g. 'oa_locations'
  select(doi,
         unpw_year = year,
         unpw_publisher = publisher,
         journal_is_in_doaj,
         # unpw_is_oa = is_oa, # already is in data to join
         # unpw_oa_status = oa_status,
         has_repository_copy,
         origin_unpaywall)


# 2021: Load and clean Unpaywall data, retrieved 2022-09-20 ----
unpaywall_2021_file <- "raw_data/2021_unpaywall_fetched_2022-09-20_json.xlsx"
unpaywall_2021_raw <- read_excel(unpaywall_2021_file)

# Remove doi duplicates keeping last entry (this is the newest as there were changed during the API request)
# https://datacornering.com/remove-duplicates-and-keep-last-in-r/
unpaywall_2021_clean <- unpaywall_2021_raw %>%
  clean_names() %>%
  group_by(doi) %>%
  filter(row_number() == n())

unpaywall_2021_oalocations <- unpaywall_2021_clean %>%
  select(doi, oa_locations) %>%
  drop_na(oa_locations) %>%
  rowwise() %>%
  mutate(oa_locations = if_else(!is.na(oa_locations), list(fromJSON(oa_locations)), list(oa_locations)))

unpaywall_2021_unnested <- unpaywall_2021_clean %>%
  select(-oa_locations) %>%
  left_join(unpaywall_2021_oalocations, by = "doi") %>%
  unnest(oa_locations)

unpaywall_2021_slim <- unpaywall_2021_unnested %>%
  mutate(origin_unpaywall = "2021") %>%
  # FIXME: make list from string in 'oa_locations' https://stackoverflow.com/questions/24256044/comma-separated-string-to-list-in-r
  # FIXME: add some columns in the selection (see data_2_roadoi.R), e.g. 'oa_locations'
  select(doi = DOI,
         unpw_year = year,
         unpw_publisher = publisher,
         journal_is_in_doaj,
         # unpw_is_oa = is_oa, # already is in data to join
         # unpw_oa_status = oa_status,
         has_repository_copy,
         origin_unpaywall)

# Combine Unpaywall data all years ----
unpaywall_2016_2021_slim <- rbind(unpaywall_2016_2020_slim, unpaywall_2021_slim) %>%
  distinct(doi, .keep_all = TRUE)
  # FIXME: unnest 'oa_locations'
