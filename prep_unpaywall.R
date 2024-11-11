#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Clean and combine Unpaywall data for all years, keeping relevant columns ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load libraries ----
library(readxl)
library(tidyverse)
library(dplyr)
library(janitor)
library(jsonlite)

# License levels, ordered from most to least permissive, see https://creativecommons.org/about/cclicenses/ ----
cc_licenses = c("cc-by", "cc-by-sa", "cc-by-nc", "cc-by-nc-sa", "cc-by-nd", "cc-by-nc-nd")
license_levels = c(cc_licenses, "other license", "no license found")

#' Unnest `oa_locations` and find best license according to `license_levels` for each doi
#' #' @returns A data frame.
find_best_license <- function(df) {
  # clean licenses
  unnest_clean <- df %>%
    unnest(oa_locations, keep_empty = TRUE, names_sep = "_") %>%
    rename(license = oa_locations_license) %>%
    mutate(license = case_when(is.na(license) ~ "no license found",
                               !license %in% cc_licenses ~ "other license",
                               TRUE ~ license)) %>%
    mutate(license = factor(license, levels = license_levels, ordered = TRUE))
  # Find best license for each doi
  find_best <- unnest_clean %>%
    group_by(doi) %>%
    mutate(min_license = min(license)) %>%
    mutate(best_license = min_license == license) %>%
    filter(best_license) %>%
    distinct(doi, .keep_all = TRUE) # NOTE this rather randomly chooses for each article one row with best license in disregard of other properties from license. FIXME if others are important
  return(find_best)
}

#' Add `oa_status_new` and `is_oa_new` for the dashboard's
#' definition of Open Access:
#' bronze articles with repository copy are given the status "green".
#' Bronze is not OA.
unpaywall_set_oa <- function(df) {
  df_new <- df %>%
    mutate(oa_status_new = case_when(oa_status == "bronze" & has_repository_copy == TRUE ~ "green",
                                     TRUE ~ oa_status)) %>%
    mutate(is_oa_new = if_else(oa_status_new %in% c("gold", "hybrid", "green"), TRUE, FALSE))
  return(df_new)
}

#' Slim a cleaned data frame containing Unpaywall data
#' to the columns needed for further usage.
#' Also add the column `origin_unpaywall` and correct data types.
#' @param unpw_df
#' @param dataset_years A character string giving provenance, will be filled
#' in column `origin_unpaywall`.
#' @param keep_oa_status If `TRUE` keeps the fields `is_oa` and `oa_status`,
#' otherwise drop them..
#' Default: `TRUE`
#' @returns A data frame.
unpaywall_slim <- function(unpw_df, dataset_years) {
  unpw_df <- unpw_df %>%
    mutate(year = as.double(year)) %>%
    mutate(origin_unpaywall = dataset_years) %>%
    select(doi,
           unpw_year = year,
           unpw_publisher = publisher,
           journal_is_in_doaj,
           license,
           unpw_is_oa = is_oa,
           unpw_oa_status = oa_status,
           has_repository_copy,
           is_oa_new,
           oa_status_new,
           origin_unpaywall)
  return(unpw_df)
}

# 2016-2020: Load and clean Unpaywall data, retrieved in 2021 ----
load("data/data_unpaywall.Rda")
unpaywall_2016_2020_raw <- data_unpaywall

unpaywall_2016_2020_slim <- unpaywall_2016_2020_raw %>%
  find_best_license() %>%
  unpaywall_set_oa() %>%
  unpaywall_slim("2016_2020")

# 2021: Load and clean Unpaywall data, retrieved 2022-09-20 ----
unpaywall_2021_file <- "raw_data/2021_unpaywall_fetched_2022-09-20.xlsx"
unpaywall_2021_raw <- read_excel(unpaywall_2021_file)

## Remove entries without oa_status (i.e. no result in Unpaywall) and
## remove doi duplicates keeping last entry
## (this is the newest as there were changes during running the API request)
## https://datacornering.com/remove-duplicates-and-keep-last-in-r/
unpaywall_2021_clean <- unpaywall_2021_raw %>%
  clean_names() %>%
  mutate(doi = tolower(doi)) %>%
  group_by(doi) %>%
  filter(row_number() == n()) %>%
  filter(!is.na(oa_status)) %>%
  mutate(journal_is_in_doaj = case_when(journal_is_in_doaj == 1 ~ TRUE,
                                        journal_is_in_doaj == 0 ~ FALSE,
                                        TRUE ~ NA)) %>%
  mutate(is_oa = case_when(is_oa == 1 ~ TRUE,
                           is_oa == 0 ~ FALSE,
                           TRUE ~ NA)) %>%
  mutate(has_repository_copy = case_when(has_repository_copy == 0 ~ FALSE, has_repository_copy == 1 ~ TRUE))
# Clean bad format of 2021 data caused by storing json in Excel file
unpaywall_2021_oalocations_only <- unpaywall_2021_clean %>%
  select(doi, oa_locations) %>%
  drop_na(oa_locations) %>%
  filter(oa_locations != "[]") %>%
  rowwise() %>%
  mutate(oa_locations = list(fromJSON(oa_locations)))
# Re-join oa_locations to other 2021 data
unpaywall_2021_prep_for_license_check <- unpaywall_2021_clean %>%
  select(-oa_locations) %>%
  rename(updated_resource = updated) %>%
  left_join(unpaywall_2021_oalocations_only, by = "doi")

unpaywall_2021_slim <- unpaywall_2021_prep_for_license_check %>%
  find_best_license() %>%
  unpaywall_set_oa() %>%
  unpaywall_slim("2021")

# 2022: Load and clean Unpaywall data ----
unpaywall_2022_raw <- readRDS("raw_data/unpaywall_2022_2023-06-23.Rds")

unpaywall_2022_slim <- unpaywall_2022_raw %>%
  find_best_license() %>%
  unpaywall_set_oa() %>%
  unpaywall_slim("2022")

# 2023: Load and clean Unpaywall data ----
unpaywall_2023_raw <- readRDS("raw_data/unpaywall_2023_2024-09-25.Rds")

unpaywall_2023_slim <- unpaywall_2023_raw %>%
  find_best_license() %>%
  unpaywall_set_oa() %>%
  unpaywall_slim("2023")

# Combine Unpaywall data all years, keep earliest request ----
unpaywall_2016_2023_slim <- rbind(unpaywall_2016_2020_slim, unpaywall_2021_slim, unpaywall_2022_slim, unpaywall_2023_slim) %>%
  distinct(doi, .keep_all = TRUE)
