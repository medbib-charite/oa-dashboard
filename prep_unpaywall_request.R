#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Request Unpaywall API via roadoi ----
# This is only executed once per yearly survey
# to keep the state at a specific date.
# The result is saved as R object (Rds file) and CSV file
# containing the request date in file name
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This script uses the `roadoi` package ----
# https://cran.r-project.org/web/packages/roadoi/readme/README.html
# https://docs.ropensci.org/roadoi/
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load libraries ----
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(readxl)
library(roadoi)
library(tidyverse)
library(stringr)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Functions to request and save ----
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' Function to request the Unpaywall API via roadoi.
#' @param file Name of an xlsx file, that contains a column `DOI` with dois to request
#' (other columns can be there but will not be considered)
#' @param sheet name of the Worksheet in the xlsx file
request_unpaywall <- function(file, sheet, excl=NULL) {
  raw <- read_xlsx(file, sheet = sheet)
  dois <- raw %>%
    select(doi = DOI) %>%
    filter(!str_detect(tolower(doi), "keine doi"),
           !is.na(doi),
           !doi %in% excl)
  unpw <- roadoi::oadoi_fetch(dois = dois$doi,
                     email = "openaccess@charite.de",
                     .progress = "text")
  return(unpw)
}

#' Requests the Unpaywall API via roadoi using request_unpaywall()
#' and saves the results and the warnings each in an Rds file.
#' The file name includes the current date to avoid overwriting of existing
#' data for the requested year.
#' @param year year of article data, used for the file name
#' @param excl (optional) a character vector of DOIs not to be requested to
#' Unpaywall, e.g. c("10.1234/56789", "10.1235/46789")
#' This is needed for DOIs that are not included in Unpaywall.
#' no return value (results will be saved directly to files)
request_and_save_unpaywall <- function(file, sheet, year, excl=NULL) {
  unpw <- request_unpaywall(file, sheet, excl)
  warn <- warnings()
  saveRDS(unpw, file = paste0("raw_data/unpaywall_", year, "_", Sys.Date(), ".Rds"))
  saveRDS(warn, file = paste0("raw_data/unpaywall_", year, "_", Sys.Date(), "_warnings", ".Rds"))

  # TODO: unnest cols c(best_oa_location, oa_locations, oa_locations_embargoed, authors) before writing to CSV
  unpw_as_character <- apply(unpw,2,as.character)  # not the best solution
  write.csv(unpw_as_character, file = paste0("raw_data/unpaywall_", year, "_", Sys.Date(), ".csv"))
}

# 2022 ----
request_and_save_unpaywall(file = "raw_data/2022.xlsx", sheet = "Merge", year = 2022)

# 2023 ----
read_xlsx("raw_data/2023.xlsx", sheet = "Merge") %>%
  filter(!DOI == "10.1158/1078-0432.CCR-22-3790") %>% # remove this DOI because it causes a timeout every time
  mutate(Addresses = str_trunc(Addresses, 32767, side = "right")) %>%
  write_xlsx("raw_data/2023_for_unpw_request.xlsx")
request_and_save_unpaywall(file = "raw_data/2023_for_unpw_request.xlsx", sheet = "Sheet1", year = 2023)
request_and_save_unpaywall(file = "raw_data/2023.xlsx", sheet = "Merge", year = 2023)


## Test: request again the dois without result in the first time; result: they are not in Unpaywall at all ----
rest_of_2023 <- read_xlsx("raw_data/2023.xlsx", sheet = "Merge") %>%
  filter(!(tolower(DOI) %in% `unpaywall_2023_2024-09-25`$doi)) %>%
  filter(!DOI == "10.1158/1078-0432.CCR-22-3790") # remove this DOI because it causes a timeout every time
write_xlsx(rest_of_2023, "raw_data/rest_of_2023.xlsx")
request_and_save_unpaywall(file = "raw_data/rest_of_2023.xlsx", sheet = "Sheet1", year = "2023_rest_without_unpw_result")


# DOIs that are not in Crossref (e.g. DataCite instead)
exclude_unpw_2024 <- c(
  "10.17879/freeneuropathology-2024-5345",
  "10.19224/ai2024.289",
  "10.2024/1/e49646",
  "10.19224/ai2024.381",
  "10.3290/j.ijcd.b4110975",
  "10.1486/rp-01-2024_01",
  "10.19224/ai2024.479",
  "10.5414/DBP72016",
  "10.3205/zma001701",
  "10.1136/ard-2024-225982",
  "10.1136/ard-2024-225981",
  "10.30476/beat.2024.104033.1543",
  "10.3205/dgkh000474",
  "10.3238/arztebl.m2024.0071534",
  "10.3290/j.ijcd.b3916799",
  "10.22099/mbrc.2024.48466.1876",
  "10.1486/rp-01-2024_03",
  "10.1486/rp-02-2024_02",
  "10.3205/iprs000185",
  "10.1486/rp-03-2024_02",
  "10.3205/zma001702",
  "10.19224/ai2024.156",
  "10.1302/0301-620X.106B3.BJJ-2023-0820",
  "10.20495/seas.13.1_7",
  "10.3290/j.ijcd.b4451424",
  "10.12659/A1CR.943467",
  "10.2376/1439-0299-2024-9",
  "10.22088/IJMCM.BUMS.13.1.64",
  "10.19224/ai2024.193",
  "10.4244/EIJ-D-23-00868",
  "10.3205/zma001699",
  "10.19224/ai2024.541",
  "10.3290/j.jad.b5748881",
  "10.3290/j.jad.b5749192",
  "10.1002/ejhf.3185",
  "10.2024/1/e50049",
  "10.21911/aai.2024.692",
  "10.4455/eu.2024.012",
  "10.3205/000336",
  "10.3205/000332"
)

# 2024 ----
request_and_save_unpaywall(file = "raw_data/2024.xlsx", sheet = "Merge", year = 2024, excl = exclude_unpw_2024)


# other approaches
# datacite <- data.frame(doi=exclude_unpw_2024) |>
#   mutate(url = paste0("https://api.datacite.org/dois/", doi)) |>
#   pull(url)
#
# for (u in datacite) {
#   print(httr::GET(u, httr::accept_json()))
# }
#
# plyr::llply(dois, tryCatch(oadoi_fetch_, NA), email, .progress = .progress) %>%
#
#   graceful_fetch_oa <- function(variables) {
#     tryCatch()
#   }

