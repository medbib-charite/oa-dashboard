#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combining Unpaywall data for all years, keeping relevant columns ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load libraries ----
library(readxl)
library(tidyverse)
library(dplyr)


# Load and clean 2016-2020 unpaywall data, retrieved in 2021 ----
load("data/data_unpaywall.Rda")
unpaywall_2016_2020_raw <- data_unpaywall
unpaywall_2016_2020_slim <- unpaywall_2016_2020_raw %>%
  mutate(origin_unpaywall = "2016_2020") %>%
  select(doi,
         unpw_year = year,
         unpw_publisher = publisher,
         journal_is_in_doaj,
         # unpw_is_oa = is_oa, # already is in data to join
         # unpw_oa_status = oa_status,
         has_repository_copy,
         origin_unpaywall)

# Load and clean 2021 unpaywall data, retrieved 2022-09-20 ----
unpaywall_2021_file <- "raw_data/2021_unpaywall_fetched_2022-09-20.xlsx"
unpaywall_2021_raw <- read_excel(unpaywall_2021_file)

unpaywall_2021_slim <- unpaywall_2021_raw %>%
  mutate(origin_unpaywall = "2021") %>%
  select(doi = DOI,
         unpw_year = year,
         unpw_publisher = publisher,
         journal_is_in_doaj,
         # is_oa_upw = is_oa, # already is in data to join
         # oa_status_upw = oa_status,
         has_repository_copy,
         origin_unpaywall)

# Combine unpaywall data all years ----
unpaywall_2016_2021_slim <- rbind(unpaywall_2016_2020_slim, unpaywall_2021_slim) %>%
  distinct(doi, .keep_all = TRUE)
