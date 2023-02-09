#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combining Unpaywall data for all years, keeping relevant columns ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load libraries ----
library(readxl)
library(tidyverse)
library(dplyr)
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
unpaywall_2021_file <- "raw_data/2021_unpaywall_fetched_2022-09-20.xlsx"
unpaywall_2021_raw <- read_excel(unpaywall_2021_file)

# FIXME: still not working, problem with "NA" in json text; instead: reload excel into python and re-convert dict in this (and other?) column into JSON
unpaywall_2021_unnested <- unpaywall_2021_raw %>%
  mutate(oa_locations = str_replace_all(oa_locations, c("'" = '"', "None" = "null", "True" = "true", "False" = "false"))) %>%
  drop_na() %>%
  rowwise() %>%
  mutate(oa_locations = case_when(oa_locations == "[]" ~ list(),
                                  is.na(oa_locations) ~ list(),
                                  TRUE ~ list(fromJSON(oa_locations))))
  # https://stackoverflow.com/questions/37997283/extract-format-and-separate-json-already-stored-in-a-data-frame-column

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
