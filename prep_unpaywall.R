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
  mutate(origin = "2016_2020") %>%
  select(doi,
         year,
         publisher,
         journal_is_in_doaj,
         is_oa,
         oa_status,
         has_repository_copy,
         origin)

# Load and clean 2021 unpaywall data, retrieved 2022-09-20 ----
unpaywall_2021_file <- "raw_data/2021_unpaywall_fetched_2022-09-20.xlsx"
unpaywall_2021_raw <- read_excel(unpaywall_2021_file)

unpaywall_2021_slim <- unpaywall_2021_raw %>%
  mutate(origin = "2021") %>%
  select(doi = DOI,
         year,
         publisher,
         journal_is_in_doaj,
         is_oa,
         oa_status,
         has_repository_copy,
         origin)

# Combine unpaywall data all years ----
# No removal of doi duplicates here! This must be done when joining with other data depending on the origin column.
unpaywall_2016_2021_slim <- rbind(unpaywall_2016_2020_slim, unpaywall_2021_slim)
